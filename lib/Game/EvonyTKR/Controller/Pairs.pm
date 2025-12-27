use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';

require YAML::PP;
require Mojo::Promise;
require List::Util;
require Game::EvonyTKR::Model::General;
require Game::EvonyTKR::Model::General::Pair;
require Game::EvonyTKR::Service::PDL::Runtime;
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
      load_ml_conflicts
    )];
  };

  # PDL Runtime service for fast buff computation
  has 'pdl_runtime' => sub ($self) {
    Game::EvonyTKR::Service::PDL::Runtime->new(
      data_dir => $self->app->home->child('share/collections/data')->to_string
    );
  };

  sub register($c, $app, $config = {}) {
    $c->SUPER::register($app, $config);
    $c->log_info("Registering routes for " . ref($c));

    my $mainRoutes = $app->routes->any($base);

    # Pairs are now loaded from SQLite on-demand via get_pairs_by_type()
    # No initialization needed

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

    # Load pairs asynchronously - shows wait page until ready
    my ($waiting, $pairs_for_type) = $c->load_pairs_async_or_wait(
      $type,
      { skip_generic_books => 1 }
    );
    return if $waiting;  # Wait page rendered, exit

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

    my $type = $generalType;
    unless (grep { $_ eq $type } $c->GeneralKeys()->@*) {
      return $c->render(
        text => "Invalid type: $type. Valid types: "
          . join(', ', $c->GeneralKeys()->@*),
        status => 400
      );
    }
    $c->log_debug('diagnostic_pairs_by_type calling get_pairs_for_type_batch');
    my $pairs_for_type = $c->get_pairs_for_type_batch($type);
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

    my $type = $generalType;
    unless (grep { $_ eq $type } $c->GeneralKeys()->@*) {
      return $c->render(
        text => "Invalid type: $type. Valid types: "
          . join(', ', $c->GeneralKeys()->@*),
        status => 400
      );
    }

    # Load pairs asynchronously - shows wait page until ready
    my ($waiting, $pairs_for_type) = $c->load_pairs_async_or_wait(
      $type,
      { skip_generic_books => 1 }
    );
    if ($waiting) {
      # Return JSON for catalog endpoint instead of HTML wait page
      return $c->render(
        json => {
          loading => 1,
          message => 'Pairs are being loaded, please retry in 2 seconds',
          sessionId => '',
          selected => []
        },
        status => 503
      );
    }

    my @pairs = sort { $a cmp $b } @$pairs_for_type;
    $c->log_debug(sprintf('There are %s pairs to return.', scalar(@pairs)));

    # Safety check: if no pairs loaded yet, return error
    if (scalar(@pairs) == 0) {
      $c->log_warn("pairCatalog called but no pairs loaded yet for $generalType");
      return $c->render(
        json => {
          error => 'Pairs not loaded yet, please try again',
          sessionId => '',
          selected => []
        },
        status => 503  # Service Unavailable
      );
    }

    # if there were requested primaries, filter to only include those
    if (scalar @$requested_primaries) {
      $c->log_debug(sprintf(
        'pairCatalog filtering to %d requested primaries for session %s',
        scalar(@$requested_primaries), $session_id
      ));

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

      $c->log_debug(sprintf(
        'pairCatalog filtered to %d pairs (from %d total) for session %s',
        scalar(@filtered), scalar(@pairs), $session_id
      ));

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

    # Get primaries filter from query param (JSON array)
    my $primaries_json = $c->param('primaries');
    my $requested_primaries = [];
    if ($primaries_json) {
      eval { $requested_primaries = $c->decode($primaries_json); };
      if ($@) {
        $c->log_warn("Failed to decode primaries param: $@");
        $requested_primaries = [];
      }
    }

    unless (defined($session_id) && length($session_id)) {
      $c->log_error('Session ID must be present!');
      my $payload = $c->encode({ runId => 0+ $run_id });
      $c->write_sse({ type => 'complete', text => $payload });
      return;
    }

    $c->log_debug(sprintf(
      'stream_pair_details called url: %s,'
        . ' uiTarget: %s; buffActivation: %s; run_id: %s; primaries: %d',
      $c->req->url->path->to_string,
      $slug_ui, $slug_buff, 0+ $run_id, scalar(@$requested_primaries)
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
    my $type = $generalType;
    my $buffActivation = $route_meta->{buffActivation};
    my $uiTarget       = $route_meta->{uiTarget};

    # Get all pairs for this type
    unless (grep { $_ eq $type } $c->GeneralKeys()->@*) {
      return $c->render(
        text => "Invalid type: $type. Valid types: "
          . join(', ', $c->GeneralKeys()->@*),
        status => 400
      );
    }
    $c->log_debug('diagnostic_pairs_by_type calling get_pairs_for_type_batch');
    my $pairs_for_type = $c->get_pairs_for_type_batch($type);
    my @all_pairs = @$pairs_for_type;

    # Filter to requested primaries if specified
    my @sorted_pairs;
    my @unsorted_pairs;
    if (scalar(@$requested_primaries) > 0) {
      my %requested = map { $_ => 1 } @$requested_primaries;
      foreach my $pair (@all_pairs) {
        if (exists $requested{ $pair->primary->name }) {
          push @unsorted_pairs, $pair->to_wire_hash();
        }
      }
      $c->log_debug(sprintf(
        'Filtered to %d pairs from %d total for %d primaries',
        scalar(@unsorted_pairs), scalar(@all_pairs), scalar(@$requested_primaries)
      ));
    } else {
      # No filter - use all pairs
      @unsorted_pairs = map { $_->to_wire_hash() } @all_pairs;
      $c->log_debug(sprintf(
        'No filter specified, using all %d pairs',
        scalar(@unsorted_pairs)
      ));
    }
    @sorted_pairs = sort {
      my $pc = $a->{primary}->{name} cmp $b->{primary}->{name};
      my $sc = $a->{secondary}->{name} cmp $b->{secondary}->{name};
      return $pc ? $pc : $sc;
      } @unsorted_pairs;
    $c->log_debug(sprintf(
      'There are %s pairs to compute details for session %s.',
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

    # Use PDL runtime for fast pair computation (replaces Minion jobs)
    $c->log_info(sprintf(
      'Computing %d pairs using PDL runtime (activation: %s)',
      scalar(@sorted_pairs), $buffActivation
    ));

    my $primary_filters = {
      ascendingLevel => $validated_params->{ascendingLevel},
      covenantLevel  => $validated_params->{primaryCovenantLevel},
      specialty1     => $validated_params->{primarySpecialties}->[0],
      specialty2     => $validated_params->{primarySpecialties}->[1],
      specialty3     => $validated_params->{primarySpecialties}->[2],
      specialty4     => $validated_params->{primarySpecialties}->[3],
      generic1       => 'level4',  # TODO: Make configurable
    };

    my $secondary_filters = {
      ascendingLevel => $validated_params->{ascendingLevel},
      covenantLevel  => $validated_params->{secondaryCovenantLevel},
      specialty1     => $validated_params->{secondarySpecialties}->[0],
      specialty2     => $validated_params->{secondarySpecialties}->[1],
      specialty3     => $validated_params->{secondarySpecialties}->[2],
      specialty4     => $validated_params->{secondarySpecialties}->[3],
      generic1       => 'level4',  # TODO: Make configurable
    };

    # Compute all pairs using PDL (no Minion jobs needed!)
    my $targetType = $typeMap->{$validated_params->{route_meta}->{generalType}} || 'mounted_specialist';

    # Process pairs in batches to avoid blocking
    my $batch_size = 50;  # Increased from 15 - client batching handles this now
    my $current_idx = 0;
    my $total_pairs = scalar(@sorted_pairs);
    my $complete_sent = 0;  # Flag to track if we've sent the complete event

    ### START OF LOOP ###
    my $recurring_id;
    my $loopDelay = 0.01;  # Back to fast - callback ensures reliable delivery
    my $process_batch = sub {
      my $loop = shift;

      # If we've already completed, just keep connection alive (don't process more)
      return if $complete_sent;

      # Compute next batch of pairs using PDL
      my $batch_end = List::Util::min($current_idx + $batch_size, $total_pairs);

      $c->log_debug(sprintf(
        'Computing pairs %d-%d of %d',
        $current_idx + 1, $batch_end, $total_pairs
      ));

      for my $i ($current_idx .. $batch_end - 1) {
        my $pair_hash = $sorted_pairs[$i];

        # Extract names from wire hash format: { primary => { name => '...' }, secondary => { name => '...' } }
        my $primary_name = ref($pair_hash->{primary}) eq 'HASH'
          ? $pair_hash->{primary}{name}
          : $pair_hash->{primary};
        my $secondary_name = ref($pair_hash->{secondary}) eq 'HASH'
          ? $pair_hash->{secondary}{name}
          : $pair_hash->{secondary};

        eval {
          # Get full general objects to send their hash representations
          my $primary_general = $c->get_general(lc($c->normalize($primary_name)));
          my $secondary_general = $c->get_general(lc($c->normalize($secondary_name)));

          unless ($primary_general && $secondary_general) {
            $c->log_error(sprintf(
              'Cannot load generals: %s, %s',
              $primary_name, $secondary_name
            ));
            return;
          }

          # Compute combined buffs for this pair using PDL
          my $combined_buffs = $c->pdl_runtime->compute_pair_buffs(
            primary            => $primary_name,
            secondary          => $secondary_name,
            activation         => $buffActivation,
            primary_filters    => $primary_filters,
            secondary_filters  => $secondary_filters,
          );

          # Map general type to troop column suffix (ground, mounted, ranged, siege)
          my $troop_suffix;
          if ($generalType =~ /ground/i) {
            $troop_suffix = 'ground';
          } elsif ($generalType =~ /mounted/i) {
            $troop_suffix = 'mounted';
          } elsif ($generalType =~ /ranged/i) {
            $troop_suffix = 'ranged';
          } elsif ($generalType =~ /siege/i) {
            $troop_suffix = 'siege';
          } else {
            $troop_suffix = 'ground';  # Default fallback
          }

          # Format buffs into the structure the frontend expects
          my $result = {
            runId              => 0+ $run_id,
            data  => {
            primary            => $primary_general->to_hash(),
            secondary          => $secondary_general->to_hash(),
            attackbuff         => ($combined_buffs->{"attack_$troop_suffix"} // 0) + ($combined_buffs->{attack_all} // 0),
            defensebuff        => ($combined_buffs->{"defense_$troop_suffix"} // 0) + ($combined_buffs->{defense_all} // 0),
            hpbuff             => ($combined_buffs->{"hp_$troop_suffix"} // 0) + ($combined_buffs->{hp_all} // 0),
            marchbuff          => $combined_buffs->{march_size} // 0,
            groundattackdebuff => ($combined_buffs->{enemy_attack_ground} // 0) + ($combined_buffs->{enemy_attack_all} // 0),
            grounddefensedebuff => ($combined_buffs->{enemy_defense_ground} // 0) + ($combined_buffs->{enemy_defense_all} // 0),
            groundhpdebuff     => ($combined_buffs->{enemy_hp_ground} // 0) + ($combined_buffs->{enemy_hp_all} // 0),
            mountedattackdebuff => ($combined_buffs->{enemy_attack_mounted} // 0) + ($combined_buffs->{enemy_attack_all} // 0),
            mounteddefensedebuff => ($combined_buffs->{enemy_defense_mounted} // 0) + ($combined_buffs->{enemy_defense_all} // 0),
            mountedhpdebuff    => ($combined_buffs->{enemy_hp_mounted} // 0) + ($combined_buffs->{enemy_hp_all} // 0),
            rangedattackdebuff => ($combined_buffs->{enemy_attack_ranged} // 0) + ($combined_buffs->{enemy_attack_all} // 0),
            rangeddefensedebuff => ($combined_buffs->{enemy_defense_ranged} // 0) + ($combined_buffs->{enemy_defense_all} // 0),
            rangedhpdebuff     => ($combined_buffs->{enemy_hp_ranged} // 0) + ($combined_buffs->{enemy_hp_all} // 0),
            siegeattackdebuff  => ($combined_buffs->{enemy_attack_siege} // 0) + ($combined_buffs->{enemy_attack_all} // 0),
            siegedefensedebuff => ($combined_buffs->{enemy_defense_siege} // 0) + ($combined_buffs->{enemy_defense_all} // 0),
            siegehpdebuff      => ($combined_buffs->{enemy_hp_siege} // 0) + ($combined_buffs->{enemy_hp_all} // 0),
            }
          };

          # Encode and stream via SSE
          my $json_result = $c->encode($result);
          my $encoded = encode_base64($json_result, '');
          $c->write_sse({
            type => 'pair',
            text => $encoded
          });
          $c->log_debug(sprintf(
            'Sent pair %d/%d: %s / %s',
            $i + 1, $total_pairs, $primary_name, $secondary_name
          ));
        };
        if ($@) {
          $c->log_error(sprintf(
            'Error computing pair %s/%s: %s',
            $primary_name, $secondary_name, $@
          ));
        }
      }

      $current_idx = $batch_end;
      # Check if all pairs have been processed
      if ($current_idx >= $total_pairs && !$complete_sent) {
        $complete_sent = 1;  # Mark that we're sending complete

        # Small delay to ensure last batch is written before complete event
        my $flush_delay = 0.1;  # 100ms is enough with callback pattern

        Mojo::IOLoop->timer($flush_delay => sub {
          $c->log_debug(sprintf(
            'All %d pairs computed and flushed, sending complete event',
            $total_pairs
          ));
          my $payload = $c->encode({ runId => $run_id });
          # Use write_sse callback to ensure complete event is written before closing
          $c->write_sse({ type => 'complete', text => $payload } => sub { $c->finish });

          $c->log_debug('Complete event queued with finish callback');
        });
        # Stop processing more batches, but keep the connection alive
        return;
      }
    };

    # Execute immediately to start processing
    $process_batch->();

    # Then set up recurring timer to process remaining batches
    $recurring_id = Mojo::IOLoop->recurring($loopDelay => $process_batch);

    $c->on(
      finish => sub {
        # Stop the recurring timer (no Minion jobs to clean up with PDL!)
        Mojo::IOLoop->remove($recurring_id) if $recurring_id;
        $c->log_debug("Client disconnected: stopped pair computation");
      }
    );
  }
}
1;
__END__
