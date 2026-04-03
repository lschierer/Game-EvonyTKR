use v5.42.0;
use utf8::all;
#cspell: disable
use File::FindLib 'lib';
require Game::EvonyTKR::Model::General;
require Game::EvonyTKR::Model::Data;
use namespace::autoclean;

package Game::EvonyTKR::Controller::Pairs {
  use Mooish::Base -standard;
  extends 'Game::EvonyTKR::Controller::ControllerBase';

  # Compose required roles
  with 'Game::EvonyTKR::Controller::Role::Tables';
  with 'Game::EvonyTKR::Role::TableSessions';
  with 'Game::EvonyTKR::Controller::Role::Generals::Routing';
  with 'Game::EvonyTKR::Role::Constants::BuffConstants';
  with 'Game::EvonyTKR::Role::Constants::GeneralConstants';
  with 'Game::EvonyTKR::Role::Constants::Covenants';

  use List::Util     qw(min);
  use List::AllUtils qw(all any none first);
  use Carp;
  use Path::Tiny   qw(path);
  use URI::Escape  qw(uri_unescape);
  use Encode       qw(decode is_utf8);
  use Scalar::Util qw(blessed);
  use Future::AsyncAwait;
  use Game::EvonyTKR::Service::PDL::Runtime;

  # PDL Runtime service for fast buff computation
  has 'pdl_runtime' => (
    is      => 'ro',
    lazy    => 1,
    default => sub ($self) {
      my $runtime = Game::EvonyTKR::Service::PDL::Runtime->new(
        data_dir => 'share/collections/data');
      # Inject generals_loader for basic attribute calculations
      $runtime->generals_loader($self->generals_loader);
      return $runtime;
    },
  );

  # Specify which collection this controller handles
  sub collection_name {'Pairs'}

  my $reference_base = '/Reference/Generals';
  my $base           = '/Generals';

  sub getBase ($self) {
    return $base;
  }

  sub getReferenceBase ($self) {
    return $reference_base;
  }

  sub controller_name ($self) {
    return "Pairs";
  }

  # Build method - replaces register() from Mojolicious
  sub build ($self) {
    $self->logger->info("Building Pairs controller");

    # Call parent to register common routes
    $self->SUPER::build();

    # Register pair-specific routes
    # These are nested under the Generals routes

    # Pair table UI page
    $self->router->add(
      "$base/:uiTarget/:buffActivation/pair-comparison",
      {
        to => sub ($self, $ctx, @args) {
          my $uiTarget       = uri_unescape($args[0]);
          my $buffActivation = uri_unescape($args[1]);
          $uiTarget = decode('UTF-8', $uiTarget) unless is_utf8($uiTarget);
          $buffActivation = decode('UTF-8', $buffActivation)
            unless is_utf8($buffActivation);
          return $self->pairTable($ctx, $uiTarget, $buffActivation);
        },
        action => 'http.*',
      }
    );

    # Pair catalog endpoint (POST for filter body)
    $self->router->add(
      "$base/:uiTarget/:buffActivation/pair/data.json",
      {
        to => sub ($self, $ctx, @args) {
          my $uiTarget       = uri_unescape($args[0]);
          my $buffActivation = uri_unescape($args[1]);
          $uiTarget = decode('UTF-8', $uiTarget) unless is_utf8($uiTarget);
          $buffActivation = decode('UTF-8', $buffActivation)
            unless is_utf8($buffActivation);
          return $self->pairCatalog($ctx, $uiTarget, $buffActivation);
        },
        action => 'http.post',
      }
    );

    # Pair details stream (SSE)
    $self->router->add(
      "$base/:uiTarget/:buffActivation/pair-details-stream",
      {
        to => sub ($self, $ctx, @args) {
          my $uiTarget       = uri_unescape($args[0]);
          my $buffActivation = uri_unescape($args[1]);
          $uiTarget = decode('UTF-8', $uiTarget) unless is_utf8($uiTarget);
          $buffActivation = decode('UTF-8', $buffActivation)
            unless is_utf8($buffActivation);
          return $self->stream_pair_details($ctx, $uiTarget, $buffActivation);
        },
        action => 'sse.get',
      }
    );

    # Diagnostic route for pairs by type
    # Valid types: ground_specialist, mounted_specialist, ranged_specialist,
    #              siege_specialist, mayor, officer, wall
    $self->router->add(
      "$base/diagnostic/:type",
      {
        to => sub ($self, $ctx, @args) {
          my $type = uri_unescape($args[0]);
          $type = decode('UTF-8', $type) unless is_utf8($type);
          return $self->diagnostic_pairs_by_type($ctx, $type);
        },
        action => 'http.*',
        checks => {
          type =>
qr/(?:ground_specialist|mounted_specialist|ranged_specialist|siege_specialist|mayor|officer|wall)/,
        },
      }
    );

    # Add navigation items for pair comparisons
    $self->_build_pair_nav_items();
  }

  sub _build_pair_nav_items ($self) {
    foreach my $route ($self->all_valid_routes()) {
      next unless $route->{has_pairs};

      my $printableUI = $route->{uiTarget} =~ s/-/ /rg;
      my $pair_path   = sprintf('/Generals/%s/%s/pair-comparison',
        $route->{uiTarget}, $route->{buffActivation});

      eval {
        $self->add_navigation_route(
          $pair_path,
          sprintf(
            '%s %s Pair Comparison',
            $printableUI, $route->{buffActivation}
          ),
          {
            order  => 50 + ($route->{order} || 0),
            parent => sprintf('/Generals/%s/%s',
              $route->{uiTarget}, $route->{buffActivation}),
          }
        );
      };
      if ($@) {
        $self->logger->error("Failed to add pair nav item: $@");
      }
    }
  }

  # Pair comparison table UI
  sub pairTable ($self, $ctx, $uiTarget, $buffActivation) {
    $self->logger->debug(
      "Rendering pair table for $uiTarget / $buffActivation");

    # Validate route
    my $route_meta = $self->try_lookup_route($uiTarget, $buffActivation);
    unless ($route_meta) {
      $self->logger->error("Invalid pair route: $uiTarget | $buffActivation");
      return $self->render_error($ctx, 404,
        "Invalid general type or buff activation");
    }

    # Extract metadata
    my $generalType     = $route_meta->{generalType};
    my $buff_activation = $route_meta->{buffActivation};
    my $ui_target       = $route_meta->{uiTarget};

    # Check if pairs are available
    my $pairs_loader = $self->pairs_loader();
    unless ($pairs_loader) {
      return $self->render_error($ctx, 500, "Pairs data not loaded");
    }

    my $type           = $self->_general_type_to_loader_type($generalType);
    my $pairs_for_type = $pairs_loader->get_pairs_for_type($type);
    my $pair_count     = scalar(@$pairs_for_type);

    if ($pair_count == 0) {
      $self->logger->warn("No pairs found for type: $type");
    }

    # Get filter parameters with defaults
    my $ascendingLevel = $ctx->req->query_param('ascendingLevel') // 'red5';
    my $primaryCovenantLevel = $ctx->req->query_param('primaryCovenantLevel')
      // 'civilization';
    my $secondaryCovenantLevel =
      $ctx->req->query_param('secondaryCovenantLevel') // 'civilization';
    my @primarySpecialties =
      map { $ctx->req->query_param("primarySpecialty$_") // 'gold' } (1 .. 4);
    my @secondarySpecialties =
      map { $ctx->req->query_param("secondarySpecialty$_") // 'gold' } (1 .. 4);

    # Validate using Data model
    my $data_model = Game::EvonyTKR::Model::Data->new;

    unless ($data_model->validateBuffActivation($buff_activation)) {
      $self->logger->warn(
        "Invalid Buff Activation: $buff_activation, using 'Overall'");
      $buff_activation = 'Overall';
    }

    unless ($data_model->checkAscendingLevel($ascendingLevel)) {
      $self->logger->warn(
        "Invalid ascendingLevel: $ascendingLevel, using 'red5'");
      $ascendingLevel = 'red5';
    }

    unless ($data_model->checkCovenantLevel($primaryCovenantLevel)) {
      $self->logger->warn("Invalid primaryCovenantLevel, using 'civilization'");
      $primaryCovenantLevel = 'civilization';
    }

    unless ($data_model->checkCovenantLevel($secondaryCovenantLevel)) {
      $self->logger->warn(
        "Invalid secondaryCovenantLevel, using 'civilization'");
      $secondaryCovenantLevel = 'civilization';
    }

    @primarySpecialties =
      $data_model->normalizeSpecialtyLevels(@primarySpecialties);
    @secondarySpecialties =
      $data_model->normalizeSpecialtyLevels(@secondarySpecialties);

    my $vars = {
      mode                   => 'pair',
      generalType            => $generalType,
      buffActivation         => $buff_activation,
      uiTarget               => $ui_target,
      slugTarget             => $uiTarget,
      ascendingLevel         => $ascendingLevel,
      allowedBuffActivation  => $buff_activation,
      primaryCovenantLevel   => $primaryCovenantLevel,
      secondaryCovenantLevel => $secondaryCovenantLevel,
      primarySpecialties     => \@primarySpecialties,
      secondarySpecialties   => \@secondarySpecialties,
      pair_count             => $pair_count,
      title        => "Pair Comparison - $ui_target / $buff_activation",
      current_year => (localtime)[5] + 1900,
      css_files  => ['/css/GeneralTable.css', '/css/GenericSpectrumTable.css'],
      sidebar    => 1,
      navigation => $self->render_navigation($ctx->req->path),
      site_logo  => $self->site_logo(),
    };

    return $self->template('generals/pairs/GeneralTablePair.tt', $vars);
  }

  # Diagnostic endpoint for viewing conflicts by type
  sub diagnostic_pairs_by_type ($self, $ctx, $type) {
    $self->logger->debug("Diagnostic: conflicts for type $type");

    my $conflicts_loader = $self->conflicts_loader();
    unless ($conflicts_loader) {
      return $self->render_error($ctx, 500, "Conflicts data not loaded");
    }

    my $generals_loader = $self->generals_loader();
    unless ($generals_loader) {
      return $self->render_error($ctx, 500, "Generals data not loaded");
    }

    # Get all generals of this type
    my @type_generals;
    foreach my $general_key ($generals_loader->list_generals->@*) {
      my $general = $generals_loader->get_general($general_key);
      next unless $general;
      my $general_types = $general->type // [];
      $general_types = [$general_types] unless ref($general_types) eq 'ARRAY';
      my $matches = grep { lc($_) eq lc($type) } @$general_types;
      push @type_generals, $general->name if $matches;
    }

    # Find all conflicts where primary is of this type
    my @conflicts;
    foreach my $primary (@type_generals) {
      my $conflicts_for = $conflicts_loader->get_conflicts_for($primary);
      foreach my $secondary (keys %$conflicts_for) {
        next unless $conflicts_for->{$secondary}; # Only conflicts, not compatible
        # Get display name for secondary
        my $sec_general = $generals_loader->get_general($secondary);
        push @conflicts, {
          primary   => $primary,
          secondary => $sec_general ? $sec_general->name : $secondary,
        };
      }
    }

    my $pairs_loader = $self->pairs_loader();
    my $stats = $pairs_loader ? $pairs_loader->stats : {};

    my @sorted_conflicts = sort { 
      my $p = $a->{primary} cmp $b->{primary};
      return $p if $p;
      return $a->{secondary} cmp $b->{secondary};
    } @conflicts;

    my $diagnostic = {
      type               => $type,
      conflict_count     => scalar(@conflicts),
      total_conflicts    => $conflicts_loader->conflict_count,
      available_types    => $pairs_loader ? $pairs_loader->list_types : [],
      all_conflicts      => \@sorted_conflicts,
      type_generals      => \@type_generals,
    };

    my $vars = {
      diagnostic   => $diagnostic,
      title        => "Conflicts Diagnostic - $type",
      current_year => (localtime)[5] + 1900,
      css_files    => ['/css/GeneralPairsIndex.css'],
      sidebar      => 1,
      navigation   => $self->render_navigation($ctx->req->path),
      site_logo    => $self->site_logo(),
    };

    return $self->template('generals/pairs/diagnostic.tt', $vars);
  }

  # Pair catalog endpoint (returns list of pairs for session)
  async sub pairCatalog ($self, $ctx, $uiTarget, $buffActivation) {
    $self->logger->debug(
      "Fetching pair catalog for $uiTarget / $buffActivation");

    # Validate route
    my $route_meta = $self->try_lookup_route($uiTarget, $buffActivation);
    unless ($route_meta) {
      $self->logger->error("Invalid pair route: $uiTarget | $buffActivation");
      return $self->render_error($ctx, 404,
        "Invalid general type or buff activation");
    }

    my $generalType = $route_meta->{generalType};

    # Check for POST body with primaries filter (optional)
    my $requested_primaries = [];
    if ($ctx->req->method eq 'POST') {
      my $json_data = await $ctx->req->json;
      $requested_primaries = $json_data->{primaries} // [];
      $self->logger->debug(sprintf("Catalog request with %d primaries filter",
        scalar(@$requested_primaries)));
    }

    # Generate unique session ID
    my $session_id = $self->generate_table_session_id($requested_primaries);

    # Get pairs from loader
    my $pairs_loader = $self->pairs_loader();
    unless ($pairs_loader) {
      return await $ctx->res->json({
        error     => 'Pairs data not loaded',
        sessionId => '',
        selected  => []
      });
    }

    my $type      = $self->_general_type_to_loader_type($generalType);
    my $all_pairs = $pairs_loader->get_pairs_for_type($type);

    if (scalar(@$all_pairs) == 0) {
      $self->logger->warn("No pairs found for type: $type");
      return await $ctx->res->json({
        error     => 'No pairs available for this type',
        sessionId => '',
        selected  => []
      });
    }

    # Filter if primaries requested
    my @filtered_pairs;
    if (scalar(@$requested_primaries) > 0) {
      my %requested = map { $_ => 1 } @$requested_primaries;
      @filtered_pairs =
        grep { exists $requested{ $_->{primary}{name} } } @$all_pairs;

      $self->logger->debug(sprintf(
        "Filtered to %d pairs from %d total",
        scalar(@filtered_pairs), scalar(@$all_pairs)
      ));
    }
    else {
      @filtered_pairs = @$all_pairs;
    }

    # Store session for streaming endpoint
    my @pair_keys =
      map { $_->{primary}{name} . '|' . $_->{secondary}{name} } @filtered_pairs;

    $self->store_table_session(
      $session_id,
      {
        generalType    => $generalType,
        buffActivation => $buffActivation,
        items          => \@pair_keys,
        ttl            => 3600,              # 1 hour
      }
    );

    # Return catalog response
    return await $ctx->res->json({
      sessionId => $session_id,
      selected  => \@filtered_pairs,
    });
  }

  # Stream pair buff details via SSE
  async sub stream_pair_details ($self, $ctx, $uiTarget, $buffActivation) {
    # Get SSE object directly from context
    my $sse = $ctx->sse;
    $ctx->consume;

    # Start SSE stream
    await $sse->start;

    # Enable keepalive for proxy compatibility
    await $sse->keepalive($self->table_keepalive_interval);

    # Register cleanup callback
    $sse->on_close(sub {
      my ($sse_obj, $reason) = @_;
      $self->logger->debug("SSE connection closed: $reason");
    });

    # Extract parameters
    my $run_id     = 0+ $ctx->req->query_param('runId');
    my $session_id = $ctx->req->query_param('sessionId');

    # Validate session ID
    unless (defined($session_id) && length($session_id)) {
      $self->logger->error('Session ID must be present!');
      await $self->send_complete_event($sse, $run_id, 0);
      await $sse->run unless $sse->is_closed;
      return;
    }

    # Validate route
    my $route_meta = $self->try_lookup_route($uiTarget, $buffActivation);
    unless ($route_meta) {
      $self->logger->error("Invalid pair route: $uiTarget | $buffActivation");
      await $self->send_complete_event($sse, $run_id, 0);
      await $sse->run unless $sse->is_closed;
      return;
    }

    my $generalType = $route_meta->{generalType};
    my $activation  = $route_meta->{buffActivation};

    # Retrieve session data
    my $session_data = $self->get_table_session($session_id);
    my @pair_keys;

    if ($session_data) {
      @pair_keys = @{ $session_data->{items} };
      $self->logger->debug(sprintf(
'stream_pair_details: uiTarget=%s, buffActivation=%s, runId=%s, session items=%d',
        $uiTarget, $buffActivation, $run_id, scalar(@pair_keys)
      ));
    }
    else {
      # Session not found - fallback to loading all pairs for type
      $self->logger->warn(
"Session $session_id not found, fetching pairs directly for $generalType"
      );

      my $pairs_loader = $self->pairs_loader();
      unless ($pairs_loader) {
        $self->logger->error("Pairs data not loaded");
        await $self->send_complete_event($sse, $run_id, 0);
        await $sse->run unless $sse->is_closed;
        return;
      }

      my $type      = $self->_general_type_to_loader_type($generalType);
      my $all_pairs = $pairs_loader->get_pairs_for_type($type);

      @pair_keys =
        map { $_->{primary}{name} . '|' . $_->{secondary}{name} } @$all_pairs;
    }

    # Extract filter parameters
    my $ascendingLevel = $ctx->req->query_param('ascendingLevel') // 'red5';
    my $primaryCovenantLevel = $ctx->req->query_param('primaryCovenantLevel')
      // 'civilization';
    my $secondaryCovenantLevel =
      $ctx->req->query_param('secondaryCovenantLevel') // 'civilization';
    my @primarySpecialties =
      map { $ctx->req->query_param("primarySpecialty$_") // 'gold' } (1 .. 4);
    my @secondarySpecialties =
      map { $ctx->req->query_param("secondarySpecialty$_") // 'gold' } (1 .. 4);

    # Extract basic attribute filter parameters
    my $generalLevel       = $ctx->req->query_param('generalLevel')       // 40;
    my $victoryColumnLevel = $ctx->req->query_param('victoryColumnLevel') // 0;

    # Validate ranges
    $generalLevel = 40 unless ($generalLevel >= 25 && $generalLevel <= 50);
    $victoryColumnLevel = 0
      unless ($victoryColumnLevel >= 0 && $victoryColumnLevel <= 11);

    # Validate filter parameters
    my $data_model = Game::EvonyTKR::Model::Data->new;
    unless ($data_model->checkAscendingLevel($ascendingLevel)) {
      $ascendingLevel = 'red5';
    }
    unless ($data_model->checkCovenantLevel($primaryCovenantLevel)) {
      $primaryCovenantLevel = 'civilization';
    }
    unless ($data_model->checkCovenantLevel($secondaryCovenantLevel)) {
      $secondaryCovenantLevel = 'civilization';
    }
    @primarySpecialties =
      $data_model->normalizeSpecialtyLevels(@primarySpecialties);
    @secondarySpecialties =
      $data_model->normalizeSpecialtyLevels(@secondarySpecialties);

    # Build filter objects for PDL Runtime
    my $primary_filters = {
      ascendingLevel     => $ascendingLevel,
      covenantLevel      => $primaryCovenantLevel,
      generalLevel       => $generalLevel,
      victoryColumnLevel => $victoryColumnLevel,
      specialty1         => $primarySpecialties[0],
      specialty2         => $primarySpecialties[1],
      specialty3         => $primarySpecialties[2],
      specialty4         => $primarySpecialties[3],
      generic1           => 'level4',
    };

    my $secondary_filters = {
      ascendingLevel     => 'none',
      covenantLevel      => $secondaryCovenantLevel,
      generalLevel       => $generalLevel,
      victoryColumnLevel => $victoryColumnLevel,
      specialty1         => $secondarySpecialties[0],
      specialty2         => $secondarySpecialties[1],
      specialty3         => $secondarySpecialties[2],
      specialty4         => $secondarySpecialties[3],
      generic1           => 'level4',
    };

    $self->logger->debug(sprintf(
      'Computing buffs for %d pairs', scalar(@pair_keys)));

    # Process pairs using the streaming helper
    # Use event_type => 'pair' to match what pairStore.ts expects
    await $self->process_items_streaming(
      $sse,
      {
        items        => \@pair_keys,
        run_id       => $run_id,
        item_type    => 'pairs',
        event_type   => 'pair',
        process_item => async sub ($pair_key, $idx) {
          my ($primary_name, $secondary_name) = split /\|/, $pair_key;

          # Get general objects
          my $generals_loader = $self->generals_loader();
          my $primary_general =
            $generals_loader->get_general(lc($self->normalize($primary_name)));
          my $secondary_general =
            $generals_loader->get_general(
            lc($self->normalize($secondary_name)));

          unless ($primary_general && $secondary_general) {
            $self->logger->error(sprintf(
              'Cannot load generals: %s, %s',
              $primary_name, $secondary_name
            ));
            return undef;
          }

          # Compute combined buffs using PDL Runtime
          my $combined_buffs = $self->pdl_runtime->compute_pair_buffs(
            primary           => $primary_name,
            secondary         => $secondary_name,
            activation        => $activation,
            primary_filters   => $primary_filters,
            secondary_filters => $secondary_filters,
          );

          # Map general type to troop suffix
          my $troop_suffix = $self->_get_troop_suffix($generalType);

          # Build result
          my $primary_ba   = $primary_general->basicAttributes;
          my $secondary_ba = $secondary_general->basicAttributes;

          my $result = {
            runId => 0+ $run_id,
            data  => {
              primary => {
                id              => $primary_general->id,
                name            => $primary_general->name,
                type            => $primary_general->type,
                ascending       => $primary_general->ascending ? \1 : \0,
                builtInBookName => $primary_general->builtInBookName // '',
                specialtyNames  => $primary_general->specialtyNames  // [],
                basicAttributes => {
                  attack => {
                    base      => $primary_ba->attack->base,
                    increment => $primary_ba->attack->increment
                  },
                  defense => {
                    base      => $primary_ba->defense->base,
                    increment => $primary_ba->defense->increment
                  },
                  leadership => {
                    base      => $primary_ba->leadership->base,
                    increment => $primary_ba->leadership->increment
                  },
                  politics => {
                    base      => $primary_ba->politics->base,
                    increment => $primary_ba->politics->increment
                  },
                },
              },
              secondary => {
                id              => $secondary_general->id,
                name            => $secondary_general->name,
                type            => $secondary_general->type,
                ascending       => $secondary_general->ascending ? \1 : \0,
                builtInBookName => $secondary_general->builtInBookName // '',
                specialtyNames  => $secondary_general->specialtyNames  // [],
                basicAttributes => {
                  attack => {
                    base      => $secondary_ba->attack->base,
                    increment => $secondary_ba->attack->increment
                  },
                  defense => {
                    base      => $secondary_ba->defense->base,
                    increment => $secondary_ba->defense->increment
                  },
                  leadership => {
                    base      => $secondary_ba->leadership->base,
                    increment => $secondary_ba->leadership->increment
                  },
                  politics => {
                    base      => $secondary_ba->politics->base,
                    increment => $secondary_ba->politics->increment
                  },
                },
              },
              attackbuff => ($combined_buffs->{"attack_$troop_suffix"} // 0) +
                ($combined_buffs->{attack_all} // 0),
              defensebuff => ($combined_buffs->{"defense_$troop_suffix"} // 0)
                + ($combined_buffs->{defense_all} // 0),
              hpbuff => ($combined_buffs->{"hp_$troop_suffix"} // 0) +
                ($combined_buffs->{hp_all} // 0),
              marchbuff          => $combined_buffs->{march_size} // 0,
              groundattackdebuff =>
                ($combined_buffs->{enemy_attack_ground} // 0) +
                ($combined_buffs->{enemy_attack_all}    // 0),
              grounddefensedebuff =>
                ($combined_buffs->{enemy_defense_ground} // 0) +
                ($combined_buffs->{enemy_defense_all}    // 0),
              groundhpdebuff => ($combined_buffs->{enemy_hp_ground} // 0) +
                ($combined_buffs->{enemy_hp_all} // 0),
              mountedattackdebuff =>
                ($combined_buffs->{enemy_attack_mounted} // 0) +
                ($combined_buffs->{enemy_attack_all}     // 0),
              mounteddefensedebuff =>
                ($combined_buffs->{enemy_defense_mounted} // 0) +
                ($combined_buffs->{enemy_defense_all}     // 0),
              mountedhpdebuff => ($combined_buffs->{enemy_hp_mounted} // 0) +
                ($combined_buffs->{enemy_hp_all} // 0),
              rangedattackdebuff =>
                ($combined_buffs->{enemy_attack_ranged} // 0) +
                ($combined_buffs->{enemy_attack_all}    // 0),
              rangeddefensedebuff =>
                ($combined_buffs->{enemy_defense_ranged} // 0) +
                ($combined_buffs->{enemy_defense_all}    // 0),
              rangedhpdebuff => ($combined_buffs->{enemy_hp_ranged} // 0) +
                ($combined_buffs->{enemy_hp_all} // 0),
              siegeattackdebuff => ($combined_buffs->{enemy_attack_siege} // 0)
                + ($combined_buffs->{enemy_attack_all} // 0),
              siegedefensedebuff =>
                ($combined_buffs->{enemy_defense_siege} // 0) +
                ($combined_buffs->{enemy_defense_all}   // 0),
              siegehpdebuff => ($combined_buffs->{enemy_hp_siege} // 0) +
                ($combined_buffs->{enemy_hp_all} // 0),
            }
          };

          $self->logger->debug(sprintf(
            'Computed pair %d/%d: %s / %s',
            $idx + 1, scalar(@pair_keys), $primary_name, $secondary_name
          ));

          return $result;
        },
      }
    );

    # Wait for client disconnect
    await $sse->run unless $sse->is_closed;

    return;
  }

  # Helper: Map generalType to loader type key
  # Note: $generalType comes from $route_meta->{generalType} which is already
  # in the format used by GeneralKeys (e.g., 'siege_specialist')
  sub _general_type_to_loader_type ($self, $generalType) {
    my %map = (
      'ground_specialist'  => 'ground_specialist',
      'mounted_specialist' => 'mounted_specialist',
      'ranged_specialist'  => 'ranged_specialist',
      'siege_specialist'   => 'siege_specialist',
      'mayor'              => 'mayor',
      'officer'            => 'officer',
      'wall'               => 'wall',
    );
    return $map{$generalType} // 'ground_specialist';
  }

  # Helper: Map generalType to troop suffix for buff extraction
  sub _get_troop_suffix ($self, $generalType) {
    if ($generalType =~ /ground/i) {
      return 'ground';
    }
    elsif ($generalType =~ /mounted/i) {
      return 'mounted';
    }
    elsif ($generalType =~ /ranged/i) {
      return 'ranged';
    }
    elsif ($generalType =~ /siege/i) {
      return 'siege';
    }
    elsif ($generalType =~ /wall/i) {
      return 'wall';
    }
    return 'ground';    # Default
  }
}

1;

__END__

=head1 NAME

Game::EvonyTKR::Controller::Pairs - Thunderhorse controller for General Pairs

=head1 DESCRIPTION

Manages routes and views for General Pair comparisons in EvonyTKR.

Routes:
- GET /Generals/:uiTarget/:buffActivation/pair-comparison - Pair table UI
- POST /Generals/:uiTarget/:buffActivation/pair/data.json - Pair catalog endpoint
- GET /Generals/:uiTarget/:buffActivation/pair-details-stream - SSE buff streaming
- GET /Generals/diagnostic/:type - Diagnostic view of pairs by type

=head1 METHODS

=head2 pairTable

Interactive table UI for browsing and filtering general pairs by type and activation.
Displays filterable table with specialty, ascending, and covenant level controls.

=head2 pairCatalog

JSON endpoint returning filtered pairs list and session ID. Accepts optional
POST body with primaries array to filter to specific primary generals.

=head2 stream_pair_details

SSE endpoint streaming buff computations for filtered pairs. Uses PDL Runtime
for fast vectorized computation of combined buff values for primary/secondary
general combinations.

=head2 diagnostic_pairs_by_type

Diagnostic endpoint showing pair statistics and sample data for a given type.
Useful for debugging and verifying pair generation at startup.

=cut
