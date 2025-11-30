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
require Game::EvonyTKR::Control::Generals::Routing;
require Game::EvonyTKR::Model::Data;
require Mojo::Util;
require UUID;
require Data::Printer;
use namespace::autoclean;

package Game::EvonyTKR::Controller::Pairs {
  use Mojo::Base 'Game::EvonyTKR::Controller::ControllerBase';
  use Mojo::Base 'Game::EvonyTKR::Role::Persistence',        -role;
  use Mojo::Base 'Game::EvonyTKR::Controller::Role::Pairs',           -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::GeneralConstants', -role;
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

  sub register($c, $app, $config = {}) {
    $c->SUPER::register($app, $config);
    $c->logger->info("Registering routes for " . ref($c));

    my $mainRoutes = $app->routes->any($base);

    Mojo::IOLoop->timer(
      0.001 => sub {
        $c->setup_pairs_by_type();
      }
    );

    eval { $c->setup_routes($app); } or do {
      say "route setup failed in Pairs controller";
      $c->logger->error("route setup failed in Pairs controller");
    };

    Mojo::IOLoop->timer(
      0.01 => sub {
        $c->schedule_merge_pairs_from_cache($app);
      }
    );
  }

  sub schedule_merge_pairs_from_cache ($c, $app, $delay = 0) {

    # Check if pair building is complete
    my $is_complete = $c->pair_cache()->get('pair_building_complete');
    my $npbt;

    my $repeat = 0;
    $delay++;
    my $maxdelay = defined($app->config('mode'))
      && $app->config('mode') eq 'development' ? 15 : 60;
    $maxdelay = defined($maxdelay) ? $maxdelay : 60;
    $delay    = $delay % $maxdelay;
    $delay    = $delay == 0 ? 0.001 : $delay;

    if (!$is_complete) {
      $c->logger->debug(
        sprintf('Pair building not complete yet, will retry in %s', $delay));
      $repeat = 1;
    }
    else {
      $npbt = $c->get_pairs_by_type();
      if (!$npbt) {
        $c->logger->debug(sprintf(
          'Pair Building Complete but no pairs by type yet. retry in %s',
          $delay));
        $repeat = 1;
      }
    }

    if ($repeat) {
      Mojo::IOLoop->timer(
        $delay => sub {
          $c->schedule_merge_pairs_from_cache($app, $delay);
        }
      );
    }
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
      return 0;
    }

    eval {
      # Diagnostic route for pairs by type
      $mainRoutes->get('/diagnostic/:type')->to(
        controller => 'Pairs',
        action     => 'diagnostic_pairs_by_type',
      )->name('pairs_diagnostic_by_type');

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
    return 1;
  }

  sub diagnostic_pairs_by_type ($c) {
    my $type = $c->param('type');

    # Validate type against GeneralKeys
    unless (grep { $_ eq $type } $c->GeneralKeys()->@*) {
      return $c->render(
        text => "Invalid type: $type. Valid types: "
          . join(', ', $c->GeneralKeys()->@*),
        status => 400
      );
    }
    $c->logger->debug('diagnostic_pairs_by_type calling get_pairs_by_type');
    my $pairs          = $c->get_pairs_by_type();
    my $pairs_for_type = $pairs->{$type} // [];
    $c->logger->debug(sprintf(
      'diagnostic_pairs_by_type has %s pairs of type %s',
      scalar(@{$pairs_for_type}), $type
    ));

    $c->render(
      template   => 'pairs/diagnostic',
      type       => $type,
      pairs      => $pairs_for_type,
      pair_count => scalar @$pairs_for_type,
      all_types  => [$c->GeneralKeys()->@*],
    );
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
    for my $type (keys %{ $c->get_pairs_by_type() }) {
      $pair_count +=
        scalar @{ $c->get_pairs_by_type()->{$generalType} };
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
      return $c->render_markdown_page($markdown_path);
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

    my $session_id = UUID::uuid5($c->UUID5_base, $uidseed);
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

    my @pairs = sort { $a cmp $b } @{ $c->get_pairs_by_type()->{$generalType} };

    $c->logger->debug(sprintf('There are %s pairs to return.', scalar(@pairs)));

    # if there were requested primaries, filter to only include those
    if (scalar @$requested_primaries) {

      my %requested = map { $_ => 1 } @$requested_primaries;
      my @filtered;
      foreach my $entry (@pairs) {
        if (exists $requested{ $entry->primary->name }) {
          $c->logger->debug(sprintf(
            '%s was requsted for session %s',
            $entry->primary->name, $session_id
          ));
          push @filtered, $entry->to_wire_hash();
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
      my @json_data = map { $_->to_wire_hash() } @pairs;
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
    @$pairs = $c->get_pairs_by_type()->{$generalType}->@*;
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
      my $jid = 0;
      $c->app->minion->enqueue(
        summarize_pair => [
          $args->{runId},                $args->{general1},
          $args->{general2},             $args->{targetType},
          $args->{activationType},       $args->{ascendingLevel},
          $args->{primaryCovenantLevel}, $args->{primarySpecialty1},
          $args->{primarySpecialty2},    $args->{primarySpecialty3},
          $args->{primarySpecialty4},    $args->{secondaryCovenantLevel},
          $args->{secondarySpecialty1},  $args->{secondarySpecialty2},
          $args->{secondarySpecialty3},  $args->{secondarySpecialty4},
        ] => {
          delay    => ($index * 0.001) + rand(0.5),
          attempts => 2,
        }
      );
      #$c->app->minion->enqueue(
      #  pair_worker => [$args],
      #  {
      #    delay => ($index * 0.001) + rand(0.5),
      #    notes => {
      #      pair_index => $index,
      #      run_id     => $run_id,
      #      session_id => $session_id,
      #    }
      #  }
      #);
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
}
1;
__END__
