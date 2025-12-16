use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';

require YAML::PP;
require Mojo::Promise;
require List::Util;
require Game::EvonyTKR::Model::General;
require Game::EvonyTKR::Model::General::Pair;
require Game::EvonyTKR::Model::Data;
require Mojo::Util;
require UUID;
require Data::Printer;
use namespace::autoclean;

package Game::EvonyTKR::Controller::Pairs {
  use Mojo::Base 'Game::EvonyTKR::Controller::ControllerBase';
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::BuffConstants',      -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::GeneralConstants',   -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::Covenants',          -role;
  use Mojo::Base 'Game::EvonyTKR::Controller::Role::Generals::Routing', -role;
  use Mojo::Base 'Game::EvonyTKR::Role::JSON',                          -role;
  use Mojo::IOLoop;
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

  has prereqs => sub {
    return [qw(
      load_all_ascending_attributes
      load_all_builtin_books
      load_all_covenants
      load_all_generals
      load_all_generic_books
      load_all_pair_builders
      load_all_specialties
    )];
  };


  sub register($c, $app, $config = {}) {
    $c->SUPER::register($app, $config);
    $c->log_info("Registering routes for " . ref($c));

    my $mainRoutes = $app->routes->any($base);

    Mojo::IOLoop->timer(
      0.001 => sub {
        $c->setup_pairs_by_type();
      }
    );

    eval { $c->setup_routes($app); } or do {
      say "route setup failed in Pairs controller";
      $c->log_error("route setup failed in Pairs controller");
    };

  }

  sub setup_routes ($c, $app,) {
    say 'starting setup routes for Pairs';
    my @parts = split '::', __PACKAGE__;

    my $controller_name = $parts[$#parts] // 'unknown_controller';

    $c->log_debug("got controller_name $controller_name.");
    my $mainRoutes = $app->routes->any($base);

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
      $c->log_error('failed to set up General_dynamic_pairTable');
    };

    eval {
      $mainRoutes->any(
        ['GET', 'POST'] => '/:uiTarget/:buffActivation/pair/data.json')->to(
        controller => 'Pairs',
        action     => 'pairCatalog',
        )->name('Generals_dynamic_pairCatalog');
      1;
    } or do {
      $c->log_error('failed to set up Generals_dynamic_pairCatalog');
    };

    eval {
      $mainRoutes->get('/:uiTarget/:buffActivation/pair-details-stream')->to(
        controller => 'Pairs',
        action     => 'stream_pair_details',
      )->name('Generals_dynamic_pairDetails');
      1;
    } or do {
      $c->log_error('failed to set up Generals_dynamic_pairDetails');
    };

    foreach my $route ($c->all_valid_routes()) {
      $c->log_debug("building nav items for "
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
        $c->log_debug(sprintf('built route %s',));
      }
    }
    return 1;
  }

  sub get_pairs_for_type ($c, $generalType) {
    my $all_pairs = $c->get_pairs_by_type();

    unless ($all_pairs && ref($all_pairs) eq 'HASH') {
      $c->log_error('get_pairs_by_type returned invalid data');
      return [];
    }

    my $pairs_for_type = $all_pairs->{$generalType};

    unless (defined $pairs_for_type) {
      $c->log_debug(sprintf(
        'No pairs found for type "%s". Available types: %s',
        $generalType, join(', ', sort keys %$all_pairs)
      ));
      return [];
    }

    unless (ref($pairs_for_type) eq 'ARRAY') {
      $c->log_error(sprintf(
        'Pairs for type "%s" is not an array: %s',
        $generalType, ref($pairs_for_type)
      ));
      return [];
    }

    $c->log_debug(sprintf(
      'Found %d pairs for type "%s"',
      scalar(@$pairs_for_type), $generalType
    ));

    return $pairs_for_type;
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
    $c->log_debug('diagnostic_pairs_by_type calling get_pairs_for_type');
    my $pairs_for_type = $c->get_pairs_for_type($type);
    $c->log_debug(sprintf(
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
    return if $c->check_prereqs_or_wait($c->prereqs);
    my $distDir = Mojo::File::Share::dist_dir('Game::EvonyTKR');

    my $slug_ui   = $c->stash('uiTarget');
    my $slug_buff = $c->stash('buffActivation');

    # Lookup route metadata
    my $route_meta = $c->lookup_route($slug_ui, $slug_buff);

    unless ($route_meta) {
      $c->log_error("Invalid pair route: $slug_ui | $slug_buff");

      if ($c->app->mode eq 'development') {
        $c->log_debug("Known valid routes:");
        $c->each_valid_route(
          sub ($key, $meta) {
            $c->log_debug(
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

    my $pairs_for_type = $c->get_pairs_for_type($generalType);
    my $pair_count     = scalar(@$pairs_for_type);

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
      $c->log_warn("Invalid Buff Activation: $buffActivation, using 'Overall'");
      $buffActivation = 'Overall';
    }

    unless ($data_model->checkAscendingLevel($ascendingLevel)) {
      $c->log_warn("Invalid ascendingLevel: $ascendingLevel, using 'red5'");
      $ascendingLevel = 'red5';
    }

    unless ($data_model->checkCovenantLevel($primaryCovenantLevel)) {
      $c->log_warn(
        sprintf('Invalid primaryCovenantLevel: %s, using "civilization"',
          $primaryCovenantLevel)
      );
      $primaryCovenantLevel = 'civilization';
    }

    unless ($data_model->checkCovenantLevel($secondaryCovenantLevel)) {
      $c->log_warn(
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
      $c->log_debug("Rendering from markdown index file");
      return $c->render_markdown_page($markdown_path);
    }

    $c->log_debug("Rendering without markdown file");
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
    $c->log_debug("uidseed is '$uidseed'");

    my $session_id = UUID::uuid5($c->UUID5_base, $uidseed);
    $c->log_debug("final session_id is '$session_id'");

    # Lookup route metadata
    my $route_meta = $c->lookup_route($slug_ui, $slug_buff);

    unless ($route_meta) {
      $c->log_error("Invalid pair route: $slug_ui | $slug_buff");

      if ($c->app->mode eq 'development') {
        $c->log_debug("Known valid routes:");
        $c->each_valid_route(
          sub ($key, $meta) {
            $c->log_debug(
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

    my $pairs_for_type = $c->get_pairs_for_type($generalType);
    my @pairs          = sort { $a cmp $b } @$pairs_for_type;

    $c->log_debug(sprintf('There are %s pairs to return.', scalar(@pairs)));

    # if there were requested primaries, filter to only include those
    if (scalar @$requested_primaries) {

      my %requested = map { $_ => 1 } @$requested_primaries;
      my @filtered;
      foreach my $entry (@pairs) {
        if (exists $requested{ $entry->primary->name }) {
          $c->log_debug(sprintf(
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
      $c->log_debug(
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
      $c->log_error('Session ID must be present!');
      my $payload = $c->encode({ runId => 0+ $run_id });
      $c->write_sse({ type => 'complete', text => $payload });
      return;
    }
    my $selected =
      exists $session_store->{$session_id} ? $session_store->{$session_id} : [];

    $c->log_debug(sprintf(
      'stream_pair_details called url: %s,'
        . ' uiTarget: %s; buffActivation: %s; run_id: %s',
      $c->req->url->path->to_string,
      $slug_ui, $slug_buff, 0+ $run_id
    ));

    $c->log_debug(sprintf(
      'session info: sessionId: "%s"; selected: %s',
      $session_id // 'Not Present',
      join ', ',
      map { sprintf('%s/%s', $_->{primary}->{name}, $_->{secondary}->{name}) }
        @$selected
    ));

    # Lookup route metadata
    my $route_meta = $c->lookup_route($slug_ui, $slug_buff);

    unless ($route_meta) {
      $c->log_error("Invalid pair route: $slug_ui | $slug_buff");

      if ($c->app->mode eq 'development') {
        $c->log_debug("Known valid routes:");
        $c->each_valid_route(
          sub ($key, $meta) {
            $c->log_debug(
              "  $key => " . Data::Printer::np($meta, multiline => 0));
          }
        );
      }
      my $payload = $c->encode({ runId => 0+ $run_id });
      $c->write_sse({ type => 'complete', text => $payload });
      return;
    }

    my $generalType    = $route_meta->{generalType};
    my $buffActivation = $route_meta->{buffActivation};
    my $uiTarget       = $route_meta->{uiTarget};

    my $pairs_for_type = $c->get_pairs_for_type($generalType);
    my @sorted_pairs   = sort {
      my $pc = $a->primary->name cmp $b->primary->name;
      if ($pc == 0) {
        return $a->secondary->name cmp $b->secondary->name;
      }
      return $pc;
    } @$pairs_for_type;

    $c->log_debug(sprintf(
      'There are %s pairs to compute details for %s.',
      scalar(@sorted_pairs), $session_id
    ));

    my $ascendingLevel       = $c->param('ascendingLevel')       // 'none';
    my $primaryCovenantLevel = $c->param('primaryCovenantLevel') // 'none';
    my @primarySpecialties;
    push @primarySpecialties, $c->param('primarySpecialty1') // 'gold';
    push @primarySpecialties, $c->param('primarySpecialty2') // 'gold';
    push @primarySpecialties, $c->param('primarySpecialty3') // 'gold';
    push @primarySpecialties, $c->param('primarySpecialty4') // 'green';
    my $secondaryCovenantLevel = $c->param('secondaryCovenantLevel') // 'none';
    my @secondarySpecialties;
    push @secondarySpecialties, $c->param('secondarySpecialty1') // 'gold';
    push @secondarySpecialties, $c->param('secondarySpecialty2') // 'gold';
    push @secondarySpecialties, $c->param('secondarySpecialty3') // 'gold';
    push @secondarySpecialties, $c->param('secondarySpecialty4') // 'green';

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
    my @promises;

    # Enqueue jobs in batches using recurring timer to avoid SQLite lock contention
    my $completed_processes = {};
    my $total_processes     = scalar(@sorted_pairs);
    my $max_index           = scalar(@sorted_pairs) - 1;
    my $active_processes    = 0;
    my $pair_index          = 0;
    my $batch_size          = 5;  # Enqueue 100 jobs per tick
    my $current_idx         = 0;

    my $recurring_id;
    $recurring_id = Mojo::IOLoop->recurring(5 => sub {
      my $loop = shift;

      # Calculate batch range
      my $end_idx = $current_idx + $batch_size - 1;
      $end_idx = $max_index if $end_idx > $max_index;

      # Enqueue this batch
      for my $index ($current_idx .. $end_idx) {
        my $pair = $sorted_pairs[$index];
        unless ($pair && $pair->primary->name && $pair->secondary->name) {
          $c->log_error(sprintf(
            'invalid pair at index %s : %s',
            $index, $pair ? Data::Printer::np($pair) : 'undefined'
          ));
          next;
        }

        # Build args hash for the Worker class
        my $args = {
          runId                => $run_id,
          primaryName          => $pair->primary->name,
          secondaryName        => $pair->secondary->name,
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

        my $jid = $c->app->minion->enqueue(
          summarize_pair => [$args] => {
            attempts => 2,
          }
        );

        push @subs, $jid;

        # Set up promise for this job immediately
        my $promise = $c->app->minion->result_p($jid)->then(sub {
          return if !$c->tx || $c->tx->is_finished;
          my $result = shift;
          if (defined($result) && ref($result) eq 'HASH') {
            $c->log_debug(
              "job $jid result is " . Data::Printer::np($result, multiline => 0));
            if ($result->{result}->{status} eq 'complete') {
              my $encoded = encode_base64($result->{result}->{result}, '');
              $c->write_sse({ type => 'pair', text => $encoded });
            }
          }
          return $result;
        })->catch(sub {
          my $err = shift;
          $c->log_error(
            "Job $jid failed: " . Data::Printer::np($err, multiline => 0));
          return undef;    # Return something for Promise->all
        });

        push @promises, $promise;
      }

      $c->log_debug(sprintf(
        'Enqueued batch: jobs %d-%d (%d total)',
        $current_idx, $end_idx, scalar(@subs)
      ));

      # Stop recurring when all jobs are enqueued
      if ($end_idx >= $max_index) {
        $loop->remove($recurring_id);
        $c->log_info(sprintf('Finished enqueueing all %d jobs', scalar(@subs)));

        # Now that all jobs are enqueued, set up completion handler
        Mojo::Promise->all(@promises)->then(sub {
          $c->log_debug("all jobs complete promise handler starting timer");
          return if !$c->tx || $c->tx->is_finished;
          # I cannot know which order the promise handlers will
          # run in, I *need* this one to be *after* all the individual
          # job handlers have run.
          Mojo::IOLoop->timer(
            0.1 => sub ($loop) {
              $c->log_debug(
                'all jobs complete promise handler sending complete event');
              my $payload = $c->encode({ runId => $run_id });
              $c->write_sse({ type => 'complete', text => $payload });
            }
          );
        })->catch(sub {
          $c->log_error("Some jobs failed in batch");
          return undef;
        });
      }

      $current_idx = $end_idx + 1;
    });

    $c->on(
      finish => sub {
        $c->log_debug(
          "Client disconnected, canceling " . scalar(@subs) . " jobs");
        foreach my $jid (@subs) {
          my $job = $c->app->minion->job($jid);
          Mojo::IOLoop->timer(rand(5.00) => sub {
            if ($job) {
              my $info = $job->info;
              next unless $info;    # Job might be gone
              my $state = $info->{state};
              if ($state eq 'inactive') {
                $job->remove;
                $c->log_debug("Removed inactive job $jid");
              }
              elsif ($state eq 'active' && $info->{pid}) {
                eval { $job->kill(); };
                if ($@) {
                  $c->log_debug("Failed to kill job $jid: $@");
                }
                else {
                  $c->log_debug("Killed active job $jid");
                }
              }
            }
          });
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
