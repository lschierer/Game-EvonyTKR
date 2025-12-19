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

    my $args = {
      runId => $run_id,

      targetType             => $validated_params->{route_meta}->{generalType},
      activationType         => $validated_params->{buffActivation},
      ascendingLevel         => $validated_params->{ascendingLevel},
      primaryCovenantLevel   => $validated_params->{primaryCovenantLevel},
      primarySpecialty1      => $validated_params->{primarySpecialties}->[0],
      primarySpecialty2      => $validated_params->{primarySpecialties}->[1],
      primarySpecialty3      => $validated_params->{primarySpecialties}->[2],
      primarySpecialty4      => $validated_params->{primarySpecialties}->[3],
      secondaryCovenantLevel => $validated_params->{secondaryCovenantLevel},
      secondarySpecialty1    => $validated_params->{secondarySpecialties}->[0],
      secondarySpecialty2    => $validated_params->{secondarySpecialties}->[1],
      secondarySpecialty3    => $validated_params->{secondarySpecialties}->[2],
      secondarySpecialty4    => $validated_params->{secondarySpecialties}->[3],
    };

    my $batchJid = $c->app->minion->enqueue(
      batch_summarize_pairs => [\@sorted_pairs, $args] => {
        attempts => 2,
      }
    );

 # Enqueue jobs in batches using recurring timer to avoid SQLite lock contention
    my $completed_processes = {};
    my $pending_processes   = {};
    my $total_processes     = scalar(@sorted_pairs);
    my $max_index           = scalar(@sorted_pairs) - 1;
    my $active_processes    = 0;
    my $pair_index          = 0;
    my $batch_size          = 5;    # Enqueue 100 jobs per tick
    my $current_idx         = 0;

    my $recurring_id;
    my $timer_logic = sub {
      my $loop = shift;
      if (scalar keys $completed_processes->%*) {
        unless (scalar(keys $pending_processes->%*)) {
          # I *need* this one to be *after* all the individual
          # job handlers have run.
          Mojo::IOLoop->timer(
            $c->standard_delay * 2 => sub ($loop) {
              $c->log_debug('all jobs complete, sending complete event');
              my $payload = $c->encode({ runId => $run_id });
              $c->write_sse({ type => 'complete', text => $payload });
            }
          );
          Mojo::IOLoop->remove($recurring_id);
          return;
        }
      }
      $c->log_debug(sprintf('timer_logic fired for run_id "%s" session', $run_id, $session_id));
      my $batchJob = $c->app->minion->job($batchJid);
      unless ($batchJob) {
        $c->log_warn(sprintf('cannot find job for batch jid %s', $batchJid));
        unless (scalar keys $pending_processes->%*) {
          Mojo::IOLoop->remove($recurring_id);
        }
        return;
      }
      $c->log_debug(sprintf('batch job for jid "%s" found', $batchJid));

      # Add newly spawned jobs to pending list
      my $batch_info = $batchJob->info;
      if ($batch_info && $batch_info->{notes} && $batch_info->{notes}->{spawned_jobs}) {
        foreach my $spawned_jid ($batch_info->{notes}->{spawned_jobs}->@*) {
          unless (exists $pending_processes->{$spawned_jid}
            || exists $completed_processes->{$spawned_jid}) {
            $pending_processes->{$spawned_jid} = 1;
          }
        }
      }

      my @spawned_processes = keys $pending_processes->%*;
      $c->log_debug(sprintf('there are %s spawned_processes and %s completed_processes',
      scalar(@spawned_processes), scalar(keys $completed_processes->%* ), ));
      foreach my $sp (@spawned_processes) {
        if (exists $completed_processes->{$sp}) {
          delete $pending_processes->{$sp};
          next;
        }
        my $spj = $c->app->minion->job($sp);
        unless ($spj) {
          $c->log_warn(sprintf('no job for spawned job %s', $sp));
          $completed_processes->{$sp} = 0;
          next;
        }
        if ($spj->info->{state} eq 'failed') {
          $completed_processes->{$sp} = 0;
          next;
        }
        elsif ($spj->info->{state} eq 'finished') {
          my $result = $spj->info->{result};
          unless (defined($result) && ref($result) && ref($result) eq 'HASH') {
            $c->log_error(sprintf('odd result for job %s: %s', $sp, $result));
          }
          $c->log_debug(sprintf(
            'job %s result is %s',
            $sp, Data::Printer::np($result, multiline => 0)
          ));

          if ($result->{status} eq 'complete') {
            my $encoded = encode_base64($result->{result}, '');
            $c->write_sse({ type => 'pair', text => $encoded });
          }else{
            $c->log_warn(sprintf('finished process %s has status "%s"',
            $sp, $result->{status}));
          }
          $completed_processes->{$sp} = $result;
        }
        else {
         # in case I need information to debug, lets go ahead and cache it here.
          $pending_processes->{$sp} = $c->app->minion->job($sp)->info // 0;
          $c->log_debug(sprintf('process %s was in state %s', $sp, $spj->info->{state}));
        }
      }
    };

    # Execute immediately to start processing
    $timer_logic->();

    # Then set up recurring timer
    $recurring_id = Mojo::IOLoop->recurring($c->standard_delay => $timer_logic);

    $c->on(
      finish => sub {
        # Stop the recurring timer
        Mojo::IOLoop->remove($recurring_id) if $recurring_id;

        # Kill the batch job (which will cascade to spawned jobs via BatchSummarizer->kill)
        my $batch_job = $c->app->minion->job($batchJid);
        if ($batch_job) {
          my $batch_info = $batch_job->info;
          my $batch_state = $batch_info->{state} if $batch_info;

          if ($batch_state && $batch_state =~ /^(inactive|active)$/) {
            $c->log_debug("Client disconnected, killing batch job $batchJid");
            eval { $batch_job->kill('INT'); };
            if ($@) {
              $c->log_warn("Failed to kill batch job $batchJid: $@");
            }
            $batch_job->remove;
          }
        }

        # Also kill any pending spawned jobs as backup (in case batch kill didn't cascade)
        my @pending_jids = keys %$pending_processes;
        if (@pending_jids) {
          $c->log_debug("Killing " . scalar(@pending_jids) . " pending spawned jobs");
          foreach my $jid (@pending_jids) {
            my $job = $c->app->minion->job($jid);
            if ($job) {
              my $info = $job->info;
              next unless $info;
              my $state = $info->{state};

              if ($state eq 'inactive') {
                eval { $job->remove; };
                $c->log_debug("Removed inactive job $jid") unless $@;
              }
              elsif ($state eq 'active') {
                eval { $job->kill(); };
                $c->log_debug("Killed active job $jid") unless $@;
              }
            }
          }
        }

        # Clean up session store
        if (exists $session_store->{$session_id}) {
          delete $session_store->{$session_id};
        }
      }
    );
  }
}
1;
__END__
