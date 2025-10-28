use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require JSON::PP;
require YAML::PP;
require Mojo::Promise;
require List::Util;
require Game::EvonyTKR::Model::General;
require Game::EvonyTKR::Model::General::Pair;
require Game::EvonyTKR::Model::Buff::Summarizer;
require Game::EvonyTKR::Control::Generals::Routing;
require Game::EvonyTKR::Model::Data;
require Mojo::Util;
require UUID;
require Data::Printer;
use namespace::autoclean;

package Game::EvonyTKR::Controller::Pairs {
  use Mojo::Base 'Game::EvonyTKR::Controller::ControllerBase';
  use Mojo::Base 'Game::EvonyTKR::Role::Logger', -role, -signatures;
  use Mojo::Base 'Game::EvonyTKR::Role::Common', -role, -signatures;
  use Mojo::Base 'Game::EvonyTKR::Controller::Role::Generals', -role;
  use Mojo::IOLoop;
  use Mojo::JSON     qw(to_json encode_json);
  use MIME::Base64   qw(encode_base64);
  use List::AllUtils qw( all any none );
  use Carp;

  my $session_store = {};

  my $max_concurrency = 15;

  my $reference_base = '/Reference/Generals';

  sub getReferenceBase($self) {
    return $reference_base;
  }

  my $base = '/Generals';

  sub getBase($self) {
    return $base;
  }

  sub getPairs {
    state %pairs_by_type;
    return \%pairs_by_type;
  }

  sub getDataModel {
    state $dm = Game::EvonyTKR::Model::Data->new();
    return $dm;
  }

  sub register($c, $app, $config = {}) {
    $c->SUPER::register($app, $config);
    $c->logger->info("Registering routes for " . ref($c));

    my $mainRoutes = $app->routes->any($base);

    eval {
      say "calling Pairs setup_event_handlers";
      $c->setup_event_handlers($app);
    } or do {
      $c->logger->error('setup_event_handlers failed ' . $@);
    };

    eval {
      $c->setup_routes($app);
      1;
    } or do {
      say "route setup failed in Pairs controller";
      $c->logger->error("route setup failed in Pairs controller");
    };

    $app->helper(
      get_general_pairs => sub {
        return $c->getPairs();
      }
    );
  }

  sub setup_event_handlers ($c, $app) {
    $app->plugins->on(
      pairs_complete => sub {
        my $plugin = shift @_;
        my @args   = @_;
        $c->pair_receiver($app, @args);
      }
    );

    $app->plugins->on(
      pairs_by_type => sub {
        my $plugin = shift @_;
        my @args   = @_;
        $c->pair_receiver($app, @args);
      }
    );

  }

  sub setup_routes ($c, $app,) {
    say 'starting setup routes for Pairs';
    my @parts = split '::', __PACKAGE__;
    my $general_routing;

    my $controller_name = $parts[$#parts] // 'unknown_controller';

    $c->logger->debug("got controller_name $controller_name.");
    my $mainRoutes = $app->routes->any($base);

    if (defined($app->renderer->helpers->{general_routing})) {
      $general_routing = $app->general_routing;
    }
    else {
      $c->logger->debug('general_routing not available yet');
    }
    state $retryCount = 0;
    unless (defined($general_routing)) {
      my $grerror =
        'General Routing Object not available in Pairs setup_routes';
      say $grerror;
      $c->logger->error($grerror);
      my $delay = 1 + rand($retryCount);
      if ($retryCount++ < 100) {
        Mojo::IOLoop->delay(
          $delay => sub {
            $c->setup_routes($app);
          }
        );
      }
      return;
    }

    eval {
      $mainRoutes->get('/:uiTarget/:buffActivation/pair-comparison')->to(
        controller => 'Pairs',
        action     => 'pairTable',
      )->name('General_dynamic_pairTable');
      1;
    } or do {
      $c->logger->error('failed to set up General_dynamic_pairTable');
    };

    eval {
      $mainRoutes->any(
        ['GET', 'POST'] => '/:uiTarget/:buffActivation/pair/data.json')->to(
        controller => 'Pairs',
        action     => 'pairCatalog',
        )->name('Generals_dynamic_pairCatalog');
      1;
    } or do {
      $c->logger->error('failed to set up Generals_dynamic_pairCatalog');
    };

    eval {
      $mainRoutes->get('/:uiTarget/:buffActivation/pair-details-stream')->to(
        controller => 'Pairs',
        action     => 'stream_pair_details',
      )->name('Generals_dynamic_pairDetails');
      1;
    } or do {
      $c->logger->error('failed to set up Generals_dynamic_pairDetails');
    };

    foreach my $route ($general_routing->all_valid_routes()) {
      $c->logger->debug("building nav items for "
          . $route->{uiTarget} . "|"
          . $route->{buffActivation});
      my $printableUI = $route->{uiTarget} =~ s/-/ /rg;

      # Add pair comparison navigation item if applicable
      if ($route->{has_pairs}) {
        my $pair_path = sprintf('/Generals/%s/%s/pair-comparison',
          $route->{uiTarget}, $route->{buffActivation});

        $app->add_navigation_item({
          title => sprintf(
            '%s %s Pair Comparison',
            $printableUI, $route->{buffActivation}
          ),
          path   => $pair_path,
          parent => sprintf('/Generals/%s/%s',
            $route->{uiTarget}, $route->{buffActivation}),
          order => 50 + ($route->{order} || 0),
        });
        $c->logger->debug(sprintf('built route %s',));
      }
    }
  }

  sub pair_receiver ($c, $app, @args) {
    my $pairs = shift(@args);
    $c->logger->debug(
      'pair_receiver called: ' . Data::Printer::np($pairs, multiline => 0));
    my $pairs_by_type = __PACKAGE__->getPairs();

    unless (ref($pairs) eq 'HASH') {
      $c->logger->error(sprintf(
        'pair_receiver got a %s instead of a HASH', ref($pairs)));
      return;
    }

    foreach my $type (sort keys $pairs->%*) {
      $c->logger->debug("pairs for type '$type'");
      my $increment = 0;
      foreach my $p ($pairs->{$type}->@*) {
        unless (
          List::AllUtils::any {
            $_->primary->name eq $p->{primary}
              && $_->secondary->name eq $p->{secondary}
          }
          @{ $pairs_by_type->{$type} }
        ) {
          $increment++;
          my $primary   = $app->get_general($p->{primary});
          my $secondary = $app->get_general($p->{secondary});
          if ($primary && $secondary) {
            my $pair = Game::EvonyTKR::Model::General::Pair->new(
              primary   => $primary,
              secondary => $secondary,
            );
            push @{ $pairs_by_type->{$type} }, $pair;
          }
          else {
            $c->logger->warn('missing generals for pair: '
                . Data::Printer::np($p, multiline => 0));
          }
        }
      }
      $c->logger->debug(sprintf(
        'there are %s pairs of type %s added', $increment, $type));
    }
  }

  sub pairTable ($c) {
    my $distDir = Mojo::File::Share::dist_dir('Game::EvonyTKR');

    my $slug_ui   = $c->stash('uiTarget');
    my $slug_buff = $c->stash('buffActivation');

    # Lookup route metadata
    my $routing    = Game::EvonyTKR::Control::Generals::Routing->new;
    my $route_meta = $routing->lookup_route($slug_ui, $slug_buff);

    unless ($route_meta) {
      $c->logger->error("Invalid pair route: $slug_ui | $slug_buff");

      if ($c->app->mode eq 'development') {
        $c->logger->debug("Known valid routes:");
        $routing->each_valid_route(
          sub ($key, $meta) {
            $c->logger->debug(
              "  $key => " . Data::Printer::np($meta, multiline => 0));
          }
        );
      }

      return $c->render_not_found;
    }

    # Extract metadata
    my $generalType    = $route_meta->{generalType};
    my $buffActivation = $route_meta->{buffActivation};
    my $uiTarget       = $route_meta->{uiTarget};

    my $pair_count = 0;
    for my $type (keys %{ $c->getPairs() }) {
      $pair_count +=
        scalar @{ $c->getPairs()->{$generalType} };
    }
    if ($pair_count == 0) {
      # Pairs not loaded yet, show loading page
      return $c->render(
        template => 'generals/pairs/loading',
        message  => 'Pairs are still being built. Please refresh in a moment.',
        refresh_seconds => 5
      );
    }

    # Fetch query params with defaults
    my $ascendingLevel       = $c->param('ascendingLevel') // 'red5';
    my $primaryCovenantLevel = $c->param('primaryCovenantLevel')
      // 'civilization';
    my $secondaryCovenantLevel = $c->param('secondaryCovenantLevel')
      // 'civilization';

    my @primarySpecialties =
      map { $c->param("primarySpecialty$_") // 'gold' } (1 .. 4);
    my @secondarySpecialties =
      map { $c->param("secondarySpecialty$_") // 'gold' } (1 .. 4);

    # Validate
    my $data_model = Game::EvonyTKR::Model::Data->new;

    unless ($data_model->validateBuffActivation($buffActivation)) {
      $c->logger->warn(
        "Invalid Buff Activation: $buffActivation, using 'Overall'");
      $buffActivation = 'Overall';
    }

    unless ($data_model->checkAscendingLevel($ascendingLevel)) {
      $c->logger->warn("Invalid ascendingLevel: $ascendingLevel, using 'red5'");
      $ascendingLevel = 'red5';
    }

    unless ($data_model->checkCovenantLevel($primaryCovenantLevel)) {
      $c->logger->warn(
        sprintf('Invalid primaryCovenantLevel: %s, using "civilization"',
          $primaryCovenantLevel)
      );
      $primaryCovenantLevel = 'civilization';
    }

    unless ($data_model->checkCovenantLevel($secondaryCovenantLevel)) {
      $c->logger->warn(
        sprintf('Invalid secondaryCovenantLevel: %s, using "civilization"',
          $secondaryCovenantLevel)
      );
      $secondaryCovenantLevel = 'civilization';
    }

    @primarySpecialties =
      $data_model->normalizeSpecialtyLevels(@primarySpecialties);
    @secondarySpecialties =
      $data_model->normalizeSpecialtyLevels(@secondarySpecialties);

    $c->stash(
      template               => 'generals/pairs/GeneralTablePair',
      mode                   => 'pair',
      generalType            => $generalType,
      buffActivation         => $buffActivation,
      uiTarget               => $uiTarget,
      slugTarget             => $slug_ui,
      ascendingLevel         => $ascendingLevel,
      allowedBuffActivation  => $buffActivation,
      primaryCovenantLevel   => $primaryCovenantLevel,
      secondaryCovenantLevel => $secondaryCovenantLevel,
      primarySpecialties     => \@primarySpecialties,
      secondarySpecialties   => \@secondarySpecialties,
    );

    my $markdown_path = $distDir->child(
      "pages/Generals/$uiTarget/$buffActivation/pair comparison.md");

    if (-f $markdown_path) {
      $c->logger->debug("Rendering from markdown index file");
      return $c->render_markdown_file($markdown_path);
    }

    $c->logger->debug("Rendering without markdown file");
    return $c->render;
  }

  sub pairCatalog ($c) {
    my $slug_ui             = $c->stash('uiTarget');
    my $slug_buff           = $c->stash('buffActivation');
    my $requested_primaries = [];

    if ($c->req->method eq 'POST') {
      my $json_data = $c->req->json;
      $requested_primaries = $json_data->{primaries} // [];
    }

    my $uidseed = join(', ', @$requested_primaries) . ' ' . UUID::uuid7();
    $c->logger->debug("uidseed is '$uidseed'");

    my $session_id = UUID::uuid5($c->getDataModel->UUID5_base, $uidseed);
    $c->logger->debug("final session_id is '$session_id'");

    # Lookup route metadata
    my $routing    = Game::EvonyTKR::Control::Generals::Routing->new;
    my $route_meta = $routing->lookup_route($slug_ui, $slug_buff);

    unless ($route_meta) {
      $c->logger->error("Invalid pair route: $slug_ui | $slug_buff");

      if ($c->app->mode eq 'development') {
        $c->logger->debug("Known valid routes:");
        $routing->each_valid_route(
          sub ($key, $meta) {
            $c->logger->debug(
              "  $key => " . Data::Printer::np($meta, multiline => 0));
          }
        );
      }

      return $c->render_not_found;
    }

    # Extract metadata
    my $generalType    = $route_meta->{generalType};
    my $buffActivation = $route_meta->{buffActivation};
    my $uiTarget       = $route_meta->{uiTarget};

    my @pairs = @{ $c->getPairs()->{$generalType} };

    $c->logger->debug(sprintf('There are %s pairs to return.', scalar(@pairs)));

    # Return just the basic pair information without computing buffs
    my @json_data = map { {
      primary   => { name => $_->primary->name },
      secondary => { name => $_->secondary->name },
    } } @pairs;

    @json_data = sort {
      # First compare primary->name
      my $primary_cmp = $a->{primary} cmp $b->{primary};

      # If primary names are the same, compare secondary->name
      if ($primary_cmp == 0) {
        return $a->{secondary} cmp $b->{secondary};
      }

      # Otherwise, return the primary name comparison result
      return $primary_cmp;
    } @json_data;

    # if there were requested primaries, filter to only include those
    if (scalar @$requested_primaries) {

      my %requested = map { $_ => 1 } @$requested_primaries;
      my @filtered;
      foreach my $entry (@json_data) {
        if (exists $requested{ $entry->{primary} }) {
          $c->logger->debug(sprintf(
            '%s was requsted for session %s',
            $entry->{primary}, $session_id
          ));
          push @filtered, $entry;
        }
      }

      $session_store->{$session_id} = \@filtered;

      return $c->render(
        json => {
          sessionId => $session_id,
          selected  => \@filtered,
        }
      );
    }
    else {
      $c->logger->debug(
        "no requested primaries for session '$session_id' returning full list: "
          . Data::Printer::np(@json_data, multiline => 0));
      $session_store->{$session_id} = \@json_data;

      return $c->render(
        json => {
          sessionId => $session_id,
          selected  => \@json_data,
        }
      );
    }
  }

  sub stream_pair_details ($c) {
    $c->res->headers->content_type('text/event-stream');
    $c->res->headers->content_encoding('utf-8');
    $c->res->headers->add('Cache-Control', 'no-cache');

    my $slug_ui    = $c->stash('uiTarget');
    my $slug_buff  = $c->stash('buffActivation');
    my $run_id     = 0+ $c->param('runId');
    my $session_id = $c->param('sessionId');
    unless (defined($session_id) && length($session_id)) {
      $c->logger->error('Session ID must be present!');
      my $payload = encode_json({ runId => 0+ $run_id });
      $c->write_sse({ type => 'complete', text => $payload });
      return;
    }
    my $selected =
      exists $session_store->{$session_id} ? $session_store->{$session_id} : [];

    $c->logger->debug(sprintf(
      'stream_pair_details called url: %s,'
        . ' uiTarget: %s; buffActivation: %s; run_id: %s',
      $c->req->url->path->to_string,
      $slug_ui, $slug_buff, 0+ $run_id
    ));

    $c->logger->debug(sprintf(
      'session info: sessionId: "%s"; selected: %s',
      $session_id // 'Not Present',
      join ', ',
      map { sprintf('%s/%s', $_->{primary}->{name}, $_->{secondary}->{name}) }
        @$selected
    ));

    # Lookup route metadata
    my $routing    = Game::EvonyTKR::Control::Generals::Routing->new;
    my $route_meta = $routing->lookup_route($slug_ui, $slug_buff);

    unless ($route_meta) {
      $c->logger->error("Invalid pair route: $slug_ui | $slug_buff");

      if ($c->app->mode eq 'development') {
        $c->logger->debug("Known valid routes:");
        $routing->each_valid_route(
          sub ($key, $meta) {
            $c->logger->debug(
              "  $key => " . Data::Printer::np($meta, multiline => 0));
          }
        );
      }
      my $payload = encode_json({ runId => 0+ $run_id });
      $c->write_sse({ type => 'complete', text => $payload });
      return;
    }

    my $generalType    = $route_meta->{generalType};
    my $buffActivation = $route_meta->{buffActivation};
    my $uiTarget       = $route_meta->{uiTarget};
    my $pairs;
    @$pairs = $c->getPairs()->{$generalType}->@*;
    @$pairs = sort {
      my $pc = $a->primary->name cmp $b->primary->name;
      if ($pc == 0) {
        return $a->secondary->name cmp $b->secondary->name;
      }
      return $pc;
    } $pairs->@*;

    $c->logger->debug(sprintf(
      'There are %s pairs compute details for %s.',
      scalar(@$pairs), $session_id
    ));

    my $ascendingLevel       = $c->param('ascendingLevel') // 'red5';
    my $primaryCovenantLevel = $c->param('primaryCovenantLevel')
      // 'civilization';
    my @primarySpecialties;
    push @primarySpecialties, $c->param('primarySpecialty1') // 'gold';
    push @primarySpecialties, $c->param('primarySpecialty2') // 'gold';
    push @primarySpecialties, $c->param('primarySpecialty3') // 'gold';
    push @primarySpecialties, $c->param('primarySpecialty4') // 'gold';
    my $secondaryCovenantLevel = $c->param('secondaryCovenantLevel')
      // 'civilization';
    my @secondarySpecialties;
    push @secondarySpecialties, $c->param('secondarySpecialty1') // 'gold';
    push @secondarySpecialties, $c->param('secondarySpecialty2') // 'gold';
    push @secondarySpecialties, $c->param('secondarySpecialty3') // 'gold';
    push @secondarySpecialties, $c->param('secondarySpecialty4') // 'gold';

    my $validated_params = $c->validatePairParams(
      $ascendingLevel,      $primaryCovenantLevel,
      \@primarySpecialties, $secondaryCovenantLevel,
      \@secondarySpecialties,
    );

    $validated_params->{buffActivation} = $buffActivation;
    $validated_params->{route_meta}     = $route_meta;

    my $typeMap = {
      'Ground Specialists'  => 'ground_specialist',
      'Ranged Specialists'  => 'ranged_specialist',
      'Siege Specialists'   => 'siege_specialist',
      'Mounted Specialists' => 'mounted_specialist',
      'Wall Specialists'    => 'wall',
    };

    $validated_params->{typeMap} = $typeMap;

    $c->render_later;
    $c->write_sse;
    $c->inactivity_timeout(1200);

    my @subs;
    # Simple process-by-process approach instead of batching
    my $completed_processes = {};
    my $total_processes     = scalar(@$pairs);
    my $max_index           = scalar(@$pairs) - 1;
    my $active_processes    = 0;
    my $pair_index          = 0;

    for my $index (0 .. $max_index) {
      my $pair = $pairs->[$index];

      # Build args hash for the Worker class
      my $args = {
        mode                 => 'pair',
        runId                => $run_id,
        general1             => $pair->primary->name,
        general2             => $pair->secondary->name,
        targetType           => $validated_params->{route_meta}->{generalType},
        activationType       => $validated_params->{buffActivation},
        ascendingLevel       => $validated_params->{ascendingLevel},
        primaryCovenantLevel => $validated_params->{primaryCovenantLevel},
        primarySpecialty1    => $validated_params->{primarySpecialties}->[0],
        primarySpecialty2    => $validated_params->{primarySpecialties}->[1],
        primarySpecialty3    => $validated_params->{primarySpecialties}->[2],
        primarySpecialty4    => $validated_params->{primarySpecialties}->[3],
        secondaryCovenantLevel => $validated_params->{secondaryCovenantLevel},
        secondarySpecialty1 => $validated_params->{secondarySpecialties}->[0],
        secondarySpecialty2 => $validated_params->{secondarySpecialties}->[1],
        secondarySpecialty3 => $validated_params->{secondarySpecialties}->[2],
        secondarySpecialty4 => $validated_params->{secondarySpecialties}->[3],
      };

      $c->logger->debug("Enqueueing job for pair index: $index");
      my $jid = $c->app->minion->enqueue(
        pair_worker => [$args],
        {
          delay => ($index * 0.001) + rand(0.5),
          notes => {
            pair_index => $index,
            run_id     => $run_id,
            session_id => $session_id,
          }
        }
      );
      $c->logger->debug("Enqueued job with ID: $jid");
      push @subs, $jid;

    }

    my @promises;

    foreach my $jid (@subs) {
      my $job = $c->app->minion->job($jid);

      my $promise = $c->app->minion->result_p($jid)->then(sub {
        return if !$c->tx || $c->tx->is_finished;
        my $result = shift;
        if (defined($result) && ref($result) eq 'HASH') {
          $c->logger->debug(
            "job $jid result is " . Data::Printer::np($result, multiline => 0));
          if ($result->{result}->{status} eq 'complete') {
            $c->write_sse(
              { type => 'pair', text => $result->{result}->{result} });
          }
        }
        return $result;
      })->catch(sub {
        my $err = shift;
        $c->logger->error(
          "Job $jid failed: " . Data::Printer::np($err, multiline => 0));
        return undef;    # Return something for Promise->all
      });

      push @promises, $promise;
    }

    # Send completion when ALL jobs are done
    Mojo::Promise->all(@promises)->then(sub {
      $c->logger->debug("all jobs complete promise handler starting timer");
      return if !$c->tx || $c->tx->is_finished;
      # I cannot know which order the promise handlers will
      # run in, I *need* this one to be *after* all the individual
      # job handlers have run.
      Mojo::IOLoop->timer(
        10 => sub ($loop) {
          $c->logger->debug(
            'all jobs complete promise handler sending complete event');
          my $payload = encode_json({ runId => $run_id });
          $c->write_sse({ type => 'complete', text => $payload });
        }
      );

    })->catch(sub {
      $c->logger->error("Some jobs failed in batch");
      return undef;
    });

    $c->on(
      finish => sub {
        $c->logger->debug(
          "Client disconnected, canceling " . scalar(@subs) . " jobs");
        foreach my $jid (@subs) {
          my $job = $c->app->minion->job($jid);
          if ($job) {
            my $info = $job->info;
            next unless $info;    # Job might be gone
            my $state = $info->{state};
            if ($state eq 'inactive') {
              $job->remove;
              $c->logger->debug("Removed inactive job $jid");
            }
            elsif ($state eq 'active' && $info->{pid}) {
              eval { $job->kill(); };
              if ($@) {
                $c->logger->debug("Failed to kill job $jid: $@");
              }
              else {
                $c->logger->debug("Killed active job $jid");
              }
            }
          }
        }

        if (exists $session_store->{$session_id}) {
          delete $session_store->{$session_id};
        }
      }
    );
  }

  sub validatePairParams($self, $ascendingLevel, $primaryCovenantLevel,
    $primarySpecialties, $secondaryCovenantLevel, $secondarySpecialties,) {
    my $data_model = Game::EvonyTKR::Model::Data->new();

    # Validate ascending level
    if (!$data_model->checkAscendingLevel($ascendingLevel)) {
      $self->logger->warn(
        "Invalid ascendingLevel: $ascendingLevel, using default 'red5'");
      $ascendingLevel = 'red5';
    }

    if (!$data_model->checkCovenantLevel($primaryCovenantLevel)) {
      $self->logger->warn(
        sprintf('Invalid covenantLevel: %s, using default "civilization"',
          $primaryCovenantLevel)
      );
      $primaryCovenantLevel = 'civilization';
    }

    @$primarySpecialties =
      $data_model->normalizeSpecialtyLevels(@$primarySpecialties);

    if (!$data_model->checkCovenantLevel($secondaryCovenantLevel)) {
      $self->logger->warn(
        sprintf('Invalid covenantLevel: %s, using default "civilization"',
          $secondaryCovenantLevel)
      );
      $secondaryCovenantLevel = 'civilization';
    }

    @$secondarySpecialties =
      $data_model->normalizeSpecialtyLevels(@$secondarySpecialties);

    return {
      ascendingLevel         => $ascendingLevel,
      primaryCovenantLevel   => $primaryCovenantLevel,
      primarySpecialties     => $primarySpecialties,
      secondaryCovenantLevel => $secondaryCovenantLevel,
      secondarySpecialties   => $secondarySpecialties,
    };
  }

}
