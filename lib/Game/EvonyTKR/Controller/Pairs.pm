use v5.42.0;
use utf8::all;
use File::FindLib 'lib';
require Game::EvonyTKR::Model::General;
require Game::EvonyTKR::Model::Data;
use namespace::autoclean;

package Game::EvonyTKR::Controller::Pairs {
  use Mooish::Base -standard;
  extends 'Game::EvonyTKR::Controller::ControllerBase';

  # Compose required roles
  with 'Game::EvonyTKR::Controller::Role::Tables';
  with 'Game::EvonyTKR::Role::Persistence::TableSessions';
  with 'Game::EvonyTKR::Controller::Role::Generals::Routing';
  with 'Game::EvonyTKR::Role::Constants::BuffConstants';
  with 'Game::EvonyTKR::Role::Constants::GeneralConstants';
  with 'Game::EvonyTKR::Role::Constants::Covenants';
  with 'WebFramework::Role::Markdown';

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
      return Game::EvonyTKR::Service::PDL::Runtime->new(
        data_dir => 'share/collections/data');
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
          type => qr/(?:ground_specialist|mounted_specialist|ranged_specialist|siege_specialist|mayor|officer|wall)/,
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
          sprintf('%s %s Pair Comparison', $printableUI, $route->{buffActivation}),
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

    my $type = $self->_general_type_to_loader_type($generalType);
    my $pairs_for_type = $pairs_loader->get_pairs_for_type($type);
    my $pair_count     = scalar(@$pairs_for_type);

    if ($pair_count == 0) {
      $self->logger->warn("No pairs found for type: $type");
    }

    # Get filter parameters with defaults
    my $ascendingLevel         = $ctx->req->query('ascendingLevel') // 'red5';
    my $primaryCovenantLevel   = $ctx->req->query('primaryCovenantLevel') // 'civilization';
    my $secondaryCovenantLevel = $ctx->req->query('secondaryCovenantLevel') // 'civilization';
    my @primarySpecialties =
      map { $ctx->req->query("primarySpecialty$_") // 'gold' } (1 .. 4);
    my @secondarySpecialties =
      map { $ctx->req->query("secondarySpecialty$_") // 'gold' } (1 .. 4);

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
      $self->logger->warn("Invalid secondaryCovenantLevel, using 'civilization'");
      $secondaryCovenantLevel = 'civilization';
    }

    @primarySpecialties   = $data_model->normalizeSpecialtyLevels(@primarySpecialties);
    @secondarySpecialties = $data_model->normalizeSpecialtyLevels(@secondarySpecialties);

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
      title => "Pair Comparison - $ui_target / $buff_activation",
      current_year => (localtime)[5] + 1900,
      css_files    => ['/css/GeneralTable.css'],
      sidebar      => 1,
      navigation   => $self->render_navigation($ctx->req->path),
      site_logo    => $self->site_logo(),
    };

    return $self->template('generals/pairs/GeneralTablePair.tt', $vars);
  }

  # Diagnostic endpoint for viewing pairs by type
  sub diagnostic_pairs_by_type ($self, $ctx, $type) {
    $self->logger->debug("Diagnostic: pairs for type $type");

    my $pairs_loader = $self->pairs_loader();
    unless ($pairs_loader) {
      return $self->render_error($ctx, 500, "Pairs data not loaded");
    }

    # Get pairs for the requested type
    my $pairs = $pairs_loader->get_pairs_for_type($type);
    my $pair_count = scalar(@$pairs);

    # Get stats
    my $stats = $pairs_loader->stats;

    # Build diagnostic info
    my $diagnostic = {
      type           => $type,
      pair_count     => $pair_count,
      total_pairs    => $stats->{total_pairs},
      conflicts_filtered => $stats->{conflicts_found},
      pairs_by_type  => $stats->{pairs_by_type},
      available_types => $pairs_loader->list_types,
      sample_pairs   => [map {
        { primary => $_->{primary}{name}, secondary => $_->{secondary}{name} }
      } @$pairs[0 .. min(9, $#$pairs)]],
    };

    my $vars = {
      diagnostic   => $diagnostic,
      title        => "Pairs Diagnostic - $type",
      current_year => (localtime)[5] + 1900,
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

    my $type = $self->_general_type_to_loader_type($generalType);
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
      @filtered_pairs = grep {
        exists $requested{ $_->{primary}{name} }
      } @$all_pairs;

      $self->logger->debug(sprintf(
        "Filtered to %d pairs from %d total",
        scalar(@filtered_pairs), scalar(@$all_pairs)
      ));
    }
    else {
      @filtered_pairs = @$all_pairs;
    }

    # Store session for streaming endpoint
    my @pair_keys = map {
      $_->{primary}{name} . '|' . $_->{secondary}{name}
    } @filtered_pairs;

    $self->store_table_session(
      $session_id,
      {
        generalType    => $generalType,
        buffActivation => $buffActivation,
        items          => \@pair_keys,
        ttl            => 3600,    # 1 hour
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
    my $run_id     = 0+ $ctx->req->query('runId');
    my $session_id = $ctx->req->query('sessionId');

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
        "Session $session_id not found, fetching pairs directly for $generalType");

      my $pairs_loader = $self->pairs_loader();
      unless ($pairs_loader) {
        $self->logger->error("Pairs data not loaded");
        await $self->send_complete_event($sse, $run_id, 0);
        await $sse->run unless $sse->is_closed;
        return;
      }

      my $type = $self->_general_type_to_loader_type($generalType);
      my $all_pairs = $pairs_loader->get_pairs_for_type($type);

      @pair_keys = map {
        $_->{primary}{name} . '|' . $_->{secondary}{name}
      } @$all_pairs;
    }

    # Extract filter parameters
    my $ascendingLevel         = $ctx->req->query('ascendingLevel') // 'red5';
    my $primaryCovenantLevel   = $ctx->req->query('primaryCovenantLevel') // 'civilization';
    my $secondaryCovenantLevel = $ctx->req->query('secondaryCovenantLevel') // 'civilization';
    my @primarySpecialties =
      map { $ctx->req->query("primarySpecialty$_") // 'gold' } (1 .. 4);
    my @secondarySpecialties =
      map { $ctx->req->query("secondarySpecialty$_") // 'gold' } (1 .. 4);

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
    @primarySpecialties   = $data_model->normalizeSpecialtyLevels(@primarySpecialties);
    @secondarySpecialties = $data_model->normalizeSpecialtyLevels(@secondarySpecialties);

    # Build filter objects for PDL Runtime
    my $primary_filters = {
      ascendingLevel => $ascendingLevel,
      covenantLevel  => $primaryCovenantLevel,
      specialty1     => $primarySpecialties[0],
      specialty2     => $primarySpecialties[1],
      specialty3     => $primarySpecialties[2],
      specialty4     => $primarySpecialties[3],
      generic1       => 'level4',
    };

    my $secondary_filters = {
      ascendingLevel => $ascendingLevel,
      covenantLevel  => $secondaryCovenantLevel,
      specialty1     => $secondarySpecialties[0],
      specialty2     => $secondarySpecialties[1],
      specialty3     => $secondarySpecialties[2],
      specialty4     => $secondarySpecialties[3],
      generic1       => 'level4',
    };

    $self->logger->debug(sprintf(
      'Computing buffs for %d pairs', scalar(@pair_keys)));

    # Process pairs using the streaming helper
    # Use event_type => 'pair' to match what pairStore.ts expects
    await $self->process_items_streaming($sse, {
      items      => \@pair_keys,
      run_id     => $run_id,
      item_type  => 'pairs',
      event_type => 'pair',
      process_item => async sub ($pair_key, $idx) {
        my ($primary_name, $secondary_name) = split /\|/, $pair_key;

        # Get general objects
        my $generals_loader = $self->generals_loader();
        my $primary_general =
          $generals_loader->get_general(lc($self->normalize($primary_name)));
        my $secondary_general =
          $generals_loader->get_general(lc($self->normalize($secondary_name)));

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
              specialtyNames  => $primary_general->specialtyNames // [],
              basicAttributes => {
                attack     => { base => $primary_ba->attack->base,     increment => $primary_ba->attack->increment },
                defense    => { base => $primary_ba->defense->base,    increment => $primary_ba->defense->increment },
                leadership => { base => $primary_ba->leadership->base, increment => $primary_ba->leadership->increment },
                politics   => { base => $primary_ba->politics->base,   increment => $primary_ba->politics->increment },
              },
            },
            secondary => {
              id              => $secondary_general->id,
              name            => $secondary_general->name,
              type            => $secondary_general->type,
              ascending       => $secondary_general->ascending ? \1 : \0,
              builtInBookName => $secondary_general->builtInBookName // '',
              specialtyNames  => $secondary_general->specialtyNames // [],
              basicAttributes => {
                attack     => { base => $secondary_ba->attack->base,     increment => $secondary_ba->attack->increment },
                defense    => { base => $secondary_ba->defense->base,    increment => $secondary_ba->defense->increment },
                leadership => { base => $secondary_ba->leadership->base, increment => $secondary_ba->leadership->increment },
                politics   => { base => $secondary_ba->politics->base,   increment => $secondary_ba->politics->increment },
              },
            },
            attackbuff => ($combined_buffs->{"attack_$troop_suffix"} // 0) +
              ($combined_buffs->{attack_all} // 0),
            defensebuff => ($combined_buffs->{"defense_$troop_suffix"} // 0) +
              ($combined_buffs->{defense_all} // 0),
            hpbuff => ($combined_buffs->{"hp_$troop_suffix"} // 0) +
              ($combined_buffs->{hp_all} // 0),
            marchbuff => $combined_buffs->{march_size} // 0,
            groundattackdebuff =>
              ($combined_buffs->{enemy_attack_ground} // 0) +
              ($combined_buffs->{enemy_attack_all}    // 0),
            grounddefensedebuff =>
              ($combined_buffs->{enemy_defense_ground} // 0) +
              ($combined_buffs->{enemy_defense_all}    // 0),
            groundhpdebuff =>
              ($combined_buffs->{enemy_hp_ground} // 0) +
              ($combined_buffs->{enemy_hp_all}    // 0),
            mountedattackdebuff =>
              ($combined_buffs->{enemy_attack_mounted} // 0) +
              ($combined_buffs->{enemy_attack_all}     // 0),
            mounteddefensedebuff =>
              ($combined_buffs->{enemy_defense_mounted} // 0) +
              ($combined_buffs->{enemy_defense_all}     // 0),
            mountedhpdebuff =>
              ($combined_buffs->{enemy_hp_mounted} // 0) +
              ($combined_buffs->{enemy_hp_all}     // 0),
            rangedattackdebuff =>
              ($combined_buffs->{enemy_attack_ranged} // 0) +
              ($combined_buffs->{enemy_attack_all}    // 0),
            rangeddefensedebuff =>
              ($combined_buffs->{enemy_defense_ranged} // 0) +
              ($combined_buffs->{enemy_defense_all}    // 0),
            rangedhpdebuff =>
              ($combined_buffs->{enemy_hp_ranged} // 0) +
              ($combined_buffs->{enemy_hp_all}    // 0),
            siegeattackdebuff =>
              ($combined_buffs->{enemy_attack_siege} // 0) +
              ($combined_buffs->{enemy_attack_all}   // 0),
            siegedefensedebuff =>
              ($combined_buffs->{enemy_defense_siege} // 0) +
              ($combined_buffs->{enemy_defense_all}   // 0),
            siegehpdebuff =>
              ($combined_buffs->{enemy_hp_siege} // 0) +
              ($combined_buffs->{enemy_hp_all}   // 0),
          }
        };

        $self->logger->debug(sprintf(
          'Computed pair %d/%d: %s / %s',
          $idx + 1, scalar(@pair_keys), $primary_name, $secondary_name
        ));

        return $result;
      },
    });

    # Wait for client disconnect
    await $sse->run unless $sse->is_closed;

    return;
  }

  # Helper: Map generalType to loader type key
  sub _general_type_to_loader_type ($self, $generalType) {
    my %map = (
      'Ground Specialists'  => 'ground_specialist',
      'Mounted Specialists' => 'mounted_specialist',
      'Ranged Specialists'  => 'ranged_specialist',
      'Siege Specialists'   => 'siege_specialist',
      'Mayor Specialists'   => 'mayor',
      'Officer Specialists' => 'officer',
      'Wall Specialists'    => 'wall',
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
