use v5.42.0;
use utf8::all;
use File::FindLib 'lib';
require Game::EvonyTKR::Model::General;
require Game::EvonyTKR::Model::Data;
use namespace::autoclean;

package Game::EvonyTKR::Controller::Generals {
  use Mooish::Base -standard;

  # Compose table-related roles
  with 'Game::EvonyTKR::Controller::Role::Tables';
  with 'Game::EvonyTKR::Role::Persistence::TableSessions';
  with 'Game::EvonyTKR::Controller::Role::Generals::Routing';
  with 'Game::EvonyTKR::Role::Constants::BuffConstants';
  with 'Game::EvonyTKR::Role::Constants::GeneralConstants';

  extends 'Game::EvonyTKR::Controller::ControllerBase';

  use List::Util qw(min);
  use List::AllUtils qw(all any none first);
  use Carp;
  use Path::Tiny qw(path);
  use URI::Escape qw(uri_unescape);
  use Encode qw(decode is_utf8);
  use Scalar::Util qw(blessed);
  use Game::EvonyTKR::Service::PDL::Runtime;

  # PDL Runtime service for fast buff computation
  has 'pdl_runtime' => (
    is => 'ro',
    lazy => 1,
    default => sub ($self) {
      return Game::EvonyTKR::Service::PDL::Runtime->new(
        data_dir => 'share/collections/data'
      );
    },
  );

  # Specify which collection this controller handles
  sub collection_name { 'Generals' }

  my $base = '/Reference/Generals';

  sub getBase($self) {
    return $base;
  }

  sub controller_name ($self) {
    return "Generals";
  }

  # Build method - replaces register() from Mojolicious
  sub build ($self) {
    $self->logger->info("Building Generals controller");

    # Call parent to register common routes
    $self->SUPER::build();

    # Add navigation for main generals page
    $self->add_navigation_route(
      $base,
      'Generals',
      { order => 20, parent => '/Reference' }
    );

    # Register routes
    # Main generals landing page
    $self->router->add($base, {
      to => sub ($self, $ctx) {
        return $self->index($ctx);
      },
      action => 'http.get',
    });

    # Register static troop type index routes FIRST (before /:name dynamic route)
    # to avoid routing conflicts
    use Game::EvonyTKR::Role::Constants::GeneralConstants;
    my @general_types = keys %Game::EvonyTKR::Role::Constants::GeneralConstants::generalKeys;

    $self->logger->info(sprintf("Registering %d troop type index routes", scalar(@general_types)));

    foreach my $generalType (@general_types) {
      my $ui_target = $self->_ui_target_name($generalType);
      my $slug = $self->_slugify($ui_target);

      $self->router->add("$base/$slug", {
        to => sub ($self, $ctx) {
          return $self->troopTypeIndex($ctx, $slug);
        },
        action => 'http.get',
      });

      $self->logger->info("Registered troop type index route: $base/$slug for type: $generalType");
    }

    # Single general detail page (dynamic route - registered after static routes)
    $self->router->add("$base/:name", {
      to => sub ($self, $ctx, @args) {
        my $name = uri_unescape($args[0]);
        # Ensure UTF-8 decoding
        $name = decode('UTF-8', $name) unless is_utf8($name);
        return $self->show($ctx, $name);
      },
      action => 'http.get',
    });

    # NEW: Table routes (Phase 2)
    # Route 1: Table UI page
    $self->router->add("$base/:uiTarget/:buffActivation/comparison", {
      to => sub ($self, $ctx, @args) {
        my $uiTarget = uri_unescape($args[0]);
        my $buffActivation = uri_unescape($args[1]);
        $uiTarget = decode('UTF-8', $uiTarget) unless is_utf8($uiTarget);
        $buffActivation = decode('UTF-8', $buffActivation) unless is_utf8($buffActivation);
        return $self->singleTable($ctx, $uiTarget, $buffActivation);
      },
      action => 'http.get',
    });

    # Route 2: Catalog endpoint (POST for filter body)
    $self->router->add("$base/:uiTarget/:buffActivation/data.json", {
      to => sub ($self, $ctx, @args) {
        my $uiTarget = uri_unescape($args[0]);
        my $buffActivation = uri_unescape($args[1]);
        $uiTarget = decode('UTF-8', $uiTarget) unless is_utf8($uiTarget);
        $buffActivation = decode('UTF-8', $buffActivation) unless is_utf8($buffActivation);
        return $self->singleCatalog($ctx, $uiTarget, $buffActivation);
      },
      action => 'http.post',
    });

    # Route 3: Details stream (SSE)
    $self->router->add("$base/:uiTarget/:buffActivation/details-stream", {
      to => sub ($self, $ctx, @args) {
        my $uiTarget = uri_unescape($args[0]);
        my $buffActivation = uri_unescape($args[1]);
        $uiTarget = decode('UTF-8', $uiTarget) unless is_utf8($uiTarget);
        $buffActivation = decode('UTF-8', $buffActivation) unless is_utf8($buffActivation);
        return $self->stream_single_details($ctx, $uiTarget, $buffActivation);
      },
      action => 'http.get',
    });

    # Route 4: Activation index (shows single/pair choice or redirects)
    $self->router->add("$base/:uiTarget/:buffActivation", {
      to => sub ($self, $ctx, @args) {
        my $uiTarget = uri_unescape($args[0]);
        my $buffActivation = uri_unescape($args[1]);
        $uiTarget = decode('UTF-8', $uiTarget) unless is_utf8($uiTarget);
        $buffActivation = decode('UTF-8', $buffActivation) unless is_utf8($buffActivation);
        return $self->activationIndex($ctx, $uiTarget, $buffActivation);
      },
      action => 'http.get',
    });

    # Build navigation items for individual generals
    $self->build_nav_items();
  }

  sub build_nav_items ($self) {
    # Get generals loader from app (registered by DataLoaders module)
    my $generals_loader = $self->generals_loader();

    unless ($generals_loader) {
      $self->logger->error("Generals loader not available");
      return;
    }

    # Note: list_generals() returns normalized keys, not display names
    # We need to load each general to get the proper display name
    foreach my $normalized_key ($generals_loader->list_generals->@*) {
      my $general = eval { $generals_loader->get_general($normalized_key) };

      unless ($general) {
        $self->logger->warn(sprintf(
          'Failed to load general with key "%s"',
          $normalized_key
        ));
        next;
      }

      # Get display name from the general object
      my $display_name = eval { $general->name };
      unless (defined($display_name) && length($display_name)) {
        $self->logger->warn(sprintf(
          'General with key "%s" has no valid name, skipping',
          $normalized_key
        ));
        next;
      }

      # Add to navigation using the proper display name
      eval {
        $self->add_navigation_route(
          "$base/$display_name",
          $display_name,
          { order => 20, parent => $base }
        );
      };
      if ($@) {
        $self->logger->error(sprintf(
          'Failed to add nav item for general "%s" (key: %s): %s',
          $display_name, $normalized_key, $@
        ));
      }
      else {
        $self->logger->debug(sprintf(
          'Added nav item for general "%s" with path "%s/%s" (key: %s)',
          $display_name, $base, $display_name, $normalized_key
        ));
      }
    }
  }

  # Main generals landing page
  sub index($self, $ctx) {
    $self->logger->debug("Rendering generals landing page");

    my $generals_loader = $self->generals_loader();

    unless ($generals_loader) {
      return $self->render_error($ctx, 500, "Generals data not loaded");
    }

    # Gather all generals
    my $items = [];
    foreach my $general_name ($generals_loader->list_generals->@*) {
      my $general = $generals_loader->get_general($general_name);
      unless ($general) {
        $self->logger->error(sprintf('Failed to get listed general "%s"', $general_name));
        next;
      }
      push @{$items}, $general;
    }

    $self->logger->debug(
      sprintf('Generals: %s with %s items', ref($items), scalar(@$items))
    );

    my $vars = {
      items        => $items,
      title        => 'Generals',
      current_year => (localtime)[5] + 1900,
      css_files    => ['/css/collectionIndex.css'],
      sidebar      => 1,
      navigation   => $self->render_navigation($ctx->req->path),
      site_logo    => $self->site_logo(),
    };

    return $self->render('generals/index.tt', $vars);
  }

  # Show general details
  sub show ($self, $ctx, $name) {
    $self->logger->debug("Show details for general: $name");

    my $generals_loader = $self->generals_loader();

    unless ($generals_loader) {
      return $self->render_error($ctx, 500, "Generals data not loaded");
    }

    # Normalize the name for lookup
    my $normalized_name = $self->normalize($name);

    my $general = $generals_loader->get_general($normalized_name);

    unless ($general) {
      $self->logger->debug(
        "General '$name' (normalized: '$normalized_name') not found"
      );
      return $self->render_error($ctx, 404, "General not found");
    }

    $self->logger->debug("Retrieved general object");

    # Debug: Check what we got
    $self->logger->debug(sprintf(
      "General data - name: %s, type: %s, ascending: %s, book: %s",
      $general->can('name') ? ($general->name // 'undef') : 'no name method',
      $general->can('type') ? (ref($general->type) || $general->type // 'undef') : 'no type method',
      $general->can('ascending') ? ($general->ascending // 'undef') : 'no ascending method',
      $general->can('builtInBookName') ? ($general->builtInBookName // 'undef') : 'no book method'
    ));

    # Get ascending attributes for this general if applicable
    my $ascending_attrs;
    eval {
      if ($general->can('ascending') && $general->ascending) {
        $self->logger->debug("General is ascending, looking up attributes");
        my $aa_loader = $self->ascending_attributes_loader();
        if ($aa_loader) {
          my $gen_name = $general->can('name') ? $general->name : $normalized_name;
          my $normalized_general_name = $self->normalize($gen_name);
          $self->logger->debug(sprintf(
            "Looking up ascending attrs for '%s' (normalized: '%s')",
            $gen_name, $normalized_general_name
          ));
          $ascending_attrs = $aa_loader->get_for_general($normalized_general_name);
          $self->logger->debug(sprintf(
            "Ascending attrs lookup result: %s",
            $ascending_attrs ? 'found' : 'not found'
          ));
        } else {
          $self->logger->warn("No ascending attributes loader available");
        }
      } else {
        $self->logger->debug("General is not ascending or ascending field not set");
      }
    };
    if ($@) {
      $self->logger->error("Error getting ascending attributes: $@");
    }

    my $gen_name = eval { $general->name } // $normalized_name;

    # Get built-in book if available
    my $built_in_book;
    eval {
      if ($general->can('builtInBookName') && $general->builtInBookName) {
        my $books_loader = $self->books_loader();
        if ($books_loader) {
          my $book_name = $general->builtInBookName;
          my $normalized_book_name = $self->normalize($book_name);
          $built_in_book = $books_loader->get_book($normalized_book_name);
          $self->logger->debug(sprintf(
            "Looked up book '%s' (normalized: '%s'), found: %s",
            $book_name, $normalized_book_name, $built_in_book ? 'yes' : 'no'
          ));
        }
      }
    };
    if ($@) {
      $self->logger->error("Error getting built-in book: $@");
    }

    # Check if buff calculation is requested via query parameter
    my $buff_summaries;
    my $calculate_buffs = $ctx->req->query('calculate_buffs');

    if ($calculate_buffs) {
      $self->logger->debug("Buff calculation requested for $gen_name");

      # Extract query parameters with defaults
      my $ascending_level = $ctx->req->query('ascendingLevel') // 'red5';
      my $covenant_level  = $ctx->req->query('covenantLevel') // 'civilization';
      my $specialty1      = $ctx->req->query('specialty1') // 'gold';
      my $specialty2      = $ctx->req->query('specialty2') // 'gold';
      my $specialty3      = $ctx->req->query('specialty3') // 'gold';
      my $specialty4      = $ctx->req->query('specialty4') // 'gold';
      my $activation      = $ctx->req->query('activation') // 'Attacking';

      $self->logger->debug(sprintf(
        "Buff params: activation=%s, ascending=%s, covenant=%s, specialties=%s/%s/%s/%s",
        $activation, $ascending_level, $covenant_level,
        $specialty1, $specialty2, $specialty3, $specialty4
      ));

      # Compute buffs using PDL Runtime
      eval {
        $buff_summaries = $self->pdl_runtime->get_buff_summary(
          general => $gen_name,
          activation => $activation,
          filters => {
            ascendingLevel => $ascending_level,
            covenantLevel  => $covenant_level,
            specialty1     => $specialty1,
            specialty2     => $specialty2,
            specialty3     => $specialty3,
            specialty4     => $specialty4,
            generic1       => 'level4',  # Default to level 4 generic books
          }
        );

        $self->logger->debug("Successfully computed buff summaries");
      };
      if ($@) {
        $self->logger->error("Error computing buffs: $@");
      }
    }

    my $vars = {
      item          => $general,
      ascending     => $ascending_attrs,
      builtInBook   => $built_in_book,
      buff_summaries => $buff_summaries,  # Add computed buffs if available
      title         => "Details for $gen_name",
      current_year  => (localtime)[5] + 1900,
      css_files     => ['/css/collectionDetails.css'],
      sidebar       => 1,
      navigation    => $self->render_navigation($ctx->req->path),
      site_logo     => $self->site_logo(),
    };

    $self->logger->debug("About to render generals/details.tt");
    my $result = $self->render('generals/details.tt', $vars);
    $self->logger->debug("Render returned: " . ref($result));
    return $result;
  }

  # Single general table UI page
  sub singleTable ($self, $ctx, $uiTarget, $buffActivation) {
    $self->logger->debug("Rendering single general table for $uiTarget / $buffActivation");

    # Validate route using Routing role - use try_lookup_route which doesn't croak
    my $route_meta = $self->try_lookup_route($uiTarget, $buffActivation);
    unless ($route_meta) {
      $self->logger->error("Invalid single route: $uiTarget | $buffActivation");
      return $self->render_error($ctx, 404, "Invalid general type or buff activation");
    }

    # Extract validated metadata
    my $generalType = $route_meta->{generalType};
    my $buff_activation = $route_meta->{buffActivation};
    my $ui_target = $route_meta->{uiTarget};

    # Get filter parameters with defaults
    my $ascendingLevel = $ctx->req->query('ascendingLevel') // 'red5';
    my $covenantLevel = $ctx->req->query('covenantLevel') // 'civilization';
    my @specialties = map { $ctx->req->query("specialty$_") // 'gold' } (1..4);

    # Validate using Data model
    my $data_model = Game::EvonyTKR::Model::Data->new;

    unless ($data_model->validateBuffActivation($buff_activation)) {
      $self->logger->warn("Invalid Buff Activation: $buff_activation, using 'Overall'");
      $buff_activation = 'Overall';
    }

    unless ($data_model->checkAscendingLevel($ascendingLevel)) {
      $self->logger->warn("Invalid ascendingLevel: $ascendingLevel, using 'red5'");
      $ascendingLevel = 'red5';
    }

    unless ($data_model->checkCovenantLevel($covenantLevel)) {
      $self->logger->warn("Invalid covenantLevel: $covenantLevel, using 'civilization'");
      $covenantLevel = 'civilization';
    }

    @specialties = $data_model->normalizeSpecialtyLevels(@specialties);

    # Prepare template variables
    my $vars = {
      generalType           => $generalType,
      buffActivation        => $buff_activation,
      uiTarget              => $ui_target,
      ascendingLevel        => $ascendingLevel,
      covenantLevel         => $covenantLevel,
      specialties           => \@specialties,
      PrimaryFormTitle      => 'General Buff Selection',
      title                 => "Single General Table - $ui_target / $buff_activation",
      current_year          => (localtime)[5] + 1900,
      css_files             => ['/css/GeneralTable.css'],
      sidebar               => 1,
      navigation            => $self->render_navigation($ctx->req->path),
      site_logo             => $self->site_logo(),
    };

    return $self->render('generals/GeneralTableSingle.tt', $vars);
  }

  # Single general catalog endpoint (returns list of generals)
  sub singleCatalog ($self, $ctx, $uiTarget, $buffActivation) {
    $self->logger->debug("Fetching single general catalog for $uiTarget / $buffActivation");

    # Validate route - use try_lookup_route which returns undef without croaking
    my $route_meta = $self->try_lookup_route($uiTarget, $buffActivation);
    unless ($route_meta) {
      $self->logger->error("Invalid single route: $uiTarget | $buffActivation");
      return $self->render_error($ctx, 404, "Invalid general type or buff activation");
    }

    my $generalType = $route_meta->{generalType};

    # Check for POST body with primaries filter (optional)
    my $requested_primaries = [];
    if ($ctx->req->method eq 'POST') {
      my $json_data = $ctx->req->json;
      $requested_primaries = $json_data->{primaries} // [];
      $self->logger->debug(sprintf(
        "Catalog request with %d primaries filter",
        scalar(@$requested_primaries)
      ));
    }

    # Generate unique session ID
    my $session_id = $self->generate_table_session_id($requested_primaries);

    # Get all generals matching the generalType
    my $generals_loader = $self->generals_loader();
    unless ($generals_loader) {
      return $self->render_error($ctx, 500, "Generals data not loaded");
    }

    my @all_generals;
    foreach my $general_key ($generals_loader->list_generals->@*) {
      my $general = $generals_loader->get_general($general_key);
      next unless $general;

      # Filter by generalType
      my $gen_type = $general->type // '';
      next unless $gen_type eq $generalType;

      push @all_generals, $general;
    }

    # If primaries filter provided, filter to requested generals
    my @filtered_generals;
    if (scalar(@$requested_primaries) > 0) {
      my %requested = map { $_ => 1 } @$requested_primaries;
      @filtered_generals = grep {
        exists $requested{$_->name}
      } @all_generals;

      $self->logger->debug(sprintf(
        "Filtered to %d generals from %d total",
        scalar(@filtered_generals),
        scalar(@all_generals)
      ));
    } else {
      @filtered_generals = @all_generals;
    }

    # Convert to wire format (stubs for catalog)
    my @selected = map {
      { primary => $_->name }
    } @filtered_generals;

    # Store session for streaming endpoint
    my @general_keys = map { $_->{primary} } @selected;
    $self->store_table_session(
      $session_id,
      {
        generalType    => $generalType,
        buffActivation => $buffActivation,
        items          => \@general_keys,
        ttl            => 3600, # 1 hour
      }
    );

    # Return catalog response
    return $self->render_json($ctx, {
      sessionId => $session_id,
      selected  => \@selected,
    });
  }

  # Stream single general buff details via SSE
  sub stream_single_details ($self, $ctx, $uiTarget, $buffActivation) {
    # Setup SSE
    $self->setup_sse_headers();

    # Extract parameters
    my $run_id = 0+ $ctx->req->query('runId');
    my $session_id = $ctx->req->query('sessionId');

    # Validate session
    return unless $self->validate_session_id($session_id, $run_id);

    # Retrieve session data
    my $session_data = $self->get_table_session($session_id);
    unless ($session_data) {
      $self->logger->error("Session $session_id not found or expired");
      $self->write_table_sse('complete', { runId => 0+ $run_id });
      return;
    }

    $self->logger->debug(sprintf(
      'stream_single_details: uiTarget=%s, buffActivation=%s, runId=%s, session items=%d',
      $uiTarget, $buffActivation, $run_id, scalar(@{$session_data->{items}})
    ));

    # Validate route - use try_lookup_route which doesn't croak
    my $route_meta = $self->try_lookup_route($uiTarget, $buffActivation);
    unless ($route_meta) {
      $self->logger->error("Invalid single route: $uiTarget | $buffActivation");
      $self->write_table_sse('complete', { runId => 0+ $run_id });
      return;
    }

    my $generalType = $route_meta->{generalType};
    my $activation = $route_meta->{buffActivation};

    # Extract filter parameters
    my $ascendingLevel = $ctx->req->query('ascendingLevel') // 'red5';
    my $covenantLevel = $ctx->req->query('covenantLevel') // 'civilization';
    my @specialties = map { $ctx->req->query("specialty$_") // 'gold' } (1..4);

    # Validate filter parameters
    my $data_model = Game::EvonyTKR::Model::Data->new;
    unless ($data_model->checkAscendingLevel($ascendingLevel)) {
      $ascendingLevel = 'red5';
    }
    unless ($data_model->checkCovenantLevel($covenantLevel)) {
      $covenantLevel = 'civilization';
    }
    @specialties = $data_model->normalizeSpecialtyLevels(@specialties);

    # Build filter object for PDL Runtime
    my $filters = {
      ascendingLevel => $ascendingLevel,
      covenantLevel  => $covenantLevel,
      specialty1     => $specialties[0],
      specialty2     => $specialties[1],
      specialty3     => $specialties[2],
      specialty4     => $specialties[3],
      generic1       => 'level4',
    };

    # Get general names from session
    my @general_names = @{$session_data->{items}};

    $self->logger->debug(sprintf(
      'Computing buffs for %d generals',
      scalar(@general_names)
    ));

    # Setup streaming
    $ctx->render_later;
    $self->write_sse($ctx);
    $ctx->inactivity_timeout(1200); # 20 minutes

    # Process in batches (even though computation is fast, batch for UX)
    my $batch_size = 50;
    my $current_idx = 0;
    my $total_generals = scalar(@general_names);
    my $complete_sent = 0;

    my $recurring_id;
    my $loop_delay = $self->table_loop_delay // 0.01; # 10ms

    my $process_batch = sub {
      return if $complete_sent;

      my $batch_end = List::Util::min($current_idx + $batch_size, $total_generals);

      $self->logger->debug(sprintf(
        'Processing generals %d-%d of %d',
        $current_idx + 1,
        $batch_end,
        $total_generals
      ));

      for my $i ($current_idx .. $batch_end - 1) {
        my $general_name = $general_names[$i];

        eval {
          # Get general object
          my $generals_loader = $self->generals_loader();
          my $normalized_name = $self->normalize($general_name);
          my $general = $generals_loader->get_general($normalized_name);

          unless ($general) {
            $self->logger->error("Cannot load general: $general_name");
            return;
          }

          # Compute buffs using PDL Runtime
          my $buff_summary = $self->pdl_runtime->get_buff_summary(
            general    => $general_name,
            activation => $activation,
            filters    => $filters,
          );

          # Map to troop type for column extraction
          my $troop_suffix = $self->_get_troop_suffix($generalType);

          # Build result matching GeneralData schema
          my $result = {
            runId => 0+ $run_id,
            data  => {
              primary => {
                name => $general->name,
                type => $general->type,
              },
              marchbuff => $buff_summary->{buffValues}->{'Ground Troops'}->{'March Size'} // 0,
              attackbuff => $self->_extract_buff($buff_summary->{buffValues}, $troop_suffix, 'Attack'),
              defensebuff => $self->_extract_buff($buff_summary->{buffValues}, $troop_suffix, 'Defense'),
              hpbuff => $self->_extract_buff($buff_summary->{buffValues}, $troop_suffix, 'HP'),
              groundattackdebuff => $self->_extract_debuff($buff_summary->{debuffValues}, 'Ground Troops', 'Attack'),
              grounddefensedebuff => $self->_extract_debuff($buff_summary->{debuffValues}, 'Ground Troops', 'Defense'),
              groundhpdebuff => $self->_extract_debuff($buff_summary->{debuffValues}, 'Ground Troops', 'HP'),
              mountedattackdebuff => $self->_extract_debuff($buff_summary->{debuffValues}, 'Mounted Troops', 'Attack'),
              mounteddefensedebuff => $self->_extract_debuff($buff_summary->{debuffValues}, 'Mounted Troops', 'Defense'),
              mountedhpdebuff => $self->_extract_debuff($buff_summary->{debuffValues}, 'Mounted Troops', 'HP'),
              rangedattackdebuff => $self->_extract_debuff($buff_summary->{debuffValues}, 'Ranged Troops', 'Attack'),
              rangeddefensedebuff => $self->_extract_debuff($buff_summary->{debuffValues}, 'Ranged Troops', 'Defense'),
              rangedhpdebuff => $self->_extract_debuff($buff_summary->{debuffValues}, 'Ranged Troops', 'HP'),
              siegeattackdebuff => $self->_extract_debuff($buff_summary->{debuffValues}, 'Siege Machines', 'Attack'),
              siegedefensedebuff => $self->_extract_debuff($buff_summary->{debuffValues}, 'Siege Machines', 'Defense'),
              siegehpdebuff => $self->_extract_debuff($buff_summary->{debuffValues}, 'Siege Machines', 'HP'),
            }
          };

          # Stream result
          $self->write_table_sse('row', $result);

          $self->logger->debug(sprintf(
            'Sent general %d/%d: %s',
            $i + 1,
            $total_generals,
            $general_name
          ));
        };
        if ($@) {
          $self->logger->error("Error computing general $general_name: $@");
        }
      }

      $current_idx = $batch_end;

      # Check if complete
      if ($current_idx >= $total_generals && !$complete_sent) {
        $complete_sent = 1;

        # Small delay to flush last batch
        my $flush_delay = $self->table_complete_flush_delay // 0.1;
        Mojo::IOLoop->timer(
          $flush_delay => sub {
            $self->send_complete_event($run_id, $total_generals, 'generals');
          }
        );
        return;
      }
    };

    # Execute first batch immediately
    $process_batch->();

    # Schedule remaining batches
    $recurring_id = Mojo::IOLoop->recurring($loop_delay => $process_batch);

    # Cleanup on disconnect
    $ctx->on(
      finish => sub {
        Mojo::IOLoop->remove($recurring_id) if $recurring_id;
        $self->logger->debug("Client disconnected: stopped general computation");
      }
    );
  }

  # Helper: Map generalType to troop suffix for buff extraction
  sub _get_troop_suffix ($self, $generalType) {
    if ($generalType =~ /ground/i) {
      return 'ground';
    } elsif ($generalType =~ /mounted/i) {
      return 'mounted';
    } elsif ($generalType =~ /ranged/i) {
      return 'ranged';
    } elsif ($generalType =~ /siege/i) {
      return 'siege';
    } elsif ($generalType =~ /wall/i) {
      return 'wall';
    }
    return 'ground'; # Default
  }

  # Helper: Extract buff value for specific troop type
  sub _extract_buff ($self, $buff_values, $troop_suffix, $attribute) {
    my $troop_type = $self->_troop_type_from_suffix($troop_suffix);
    return $buff_values->{$troop_type}->{$attribute} // 0;
  }

  # Helper: Extract debuff value
  sub _extract_debuff ($self, $debuff_values, $troop_type, $attribute) {
    return $debuff_values->{$troop_type}->{$attribute} // 0;
  }

  # Helper: Convert troop suffix to display name
  sub _troop_type_from_suffix ($self, $suffix) {
    my %map = (
      ground  => 'Ground Troops',
      mounted => 'Mounted Troops',
      ranged  => 'Ranged Troops',
      siege   => 'Siege Machines',
      wall    => 'Wall',
    );
    return $map{$suffix} // 'Ground Troops';
  }

  # Troop type index - shows available buff activations
  sub troopTypeIndex ($self, $ctx, $troopType) {
    $self->logger->debug("Rendering troop type index for: $troopType");

    # Get all valid routes for this troop type
    my @routes = $self->get_routes_for_uiTarget($troopType);

    unless (@routes) {
      $self->logger->error("No valid routes found for troop type: $troopType");
      return $self->render_error($ctx, 404, "Invalid troop type");
    }

    # Sort routes by uiTarget, then buffActivation
    @routes = sort {
      my $uitc = $a->{uiTarget} cmp $b->{uiTarget};
      return $uitc if $uitc != 0;
      return $a->{buffActivation} cmp $b->{buffActivation};
    } @routes;

    # Prepare template variables
    my $vars = {
      troopType     => $troopType,
      routes        => \@routes,
      title         => "General Tables - $troopType",
      current_year  => (localtime)[5] + 1900,
      sidebar       => 1,
      navigation    => $self->render_navigation($ctx->req->path),
      site_logo     => $self->site_logo(),
    };

    return $self->render('generals/troopTypeIndex.tt', $vars);
  }

  # Activation index - shows single/pair choice or redirects
  sub activationIndex ($self, $ctx, $uiTarget, $buffActivation) {
    $self->logger->debug("Rendering activation index for: $uiTarget / $buffActivation");

    # Validate route - use try_lookup_route which returns undef without croaking
    my $route_meta = $self->try_lookup_route($uiTarget, $buffActivation);
    unless ($route_meta) {
      $self->logger->error("Invalid route: $uiTarget | $buffActivation");
      return $self->render_error($ctx, 404, "Invalid general type or buff activation");
    }

    # Check if pairs exist for this combination
    my $has_pairs = $route_meta->{has_pairs} // 0;

    if (!$has_pairs) {
      # No pairs exist, redirect directly to single table
      $self->logger->debug("No pairs for $uiTarget/$buffActivation, redirecting to single table");
      return $ctx->redirect("/Reference/Generals/$uiTarget/$buffActivation/comparison");
    }

    # Pairs exist, show choice
    my $vars = {
      uiTarget       => $uiTarget,
      buffActivation => $buffActivation,
      has_pairs      => $has_pairs,
      title          => "$uiTarget - $buffActivation",
      current_year   => (localtime)[5] + 1900,
      sidebar        => 1,
      navigation     => $self->render_navigation($ctx->req->path),
      site_logo      => $self->site_logo(),
    };

    return $self->render('generals/activationIndex.tt', $vars);
  }
}

1;

__END__

=head1 NAME

Game::EvonyTKR::Controller::Generals - Thunderhorse controller for Generals

=head1 DESCRIPTION

Manages routes and views for Generals in EvonyTKR.

Part 1 - Single General Display:
- GET /Reference/Generals - Index of all generals
- GET /Reference/Generals/:name - Details for specific general

Part 2 - Single General Tables (Interactive Comparison):
- GET /Reference/Generals/:troopType - Navigation page for troop type
- GET /Reference/Generals/:uiTarget/:buffActivation - Choice/redirect page
- GET /Reference/Generals/:uiTarget/:buffActivation/comparison - Table UI
- POST /Reference/Generals/:uiTarget/:buffActivation/data.json - Catalog endpoint
- GET /Reference/Generals/:uiTarget/:buffActivation/details-stream - SSE buff streaming

=head1 METHODS

=head2 index

Landing page showing all available generals with links to their detail pages.

=head2 show

Details page for a specific general. Supports optional ?calculate_buffs=1 parameter
to display computed buff summaries.

=head2 singleTable

Interactive table UI for browsing and filtering generals by type and activation.
Displays filterable table with specialty, ascending, and covenant level controls.

=head2 singleCatalog

JSON endpoint returning filtered generals list and session ID. Accepts optional
POST body with primaries array to filter to specific generals.

=head2 stream_single_details

SSE endpoint streaming buff computations for filtered generals. Uses PDL Runtime
for fast vectorized computation (~0.1ms per general). Processes generals in batches
of 50 with 10ms loop delay for smooth UX.

=head2 troopTypeIndex

Dynamic navigation page showing available buff activations for a troop type.
Displays button grid linking to activation-specific tables.

=head2 activationIndex

Decision page that checks if pairs exist for a given uiTarget/buffActivation
combination. If pairs exist, shows choice between single and pair comparison.
If no pairs exist, redirects directly to single table.

=head1 HELPER METHODS

=head2 _get_troop_suffix

Maps generalType to troop suffix (ground, mounted, ranged, siege, wall) for
buff extraction.

=head2 _extract_buff

Extracts buff value for specific troop type and attribute from PDL Runtime output.

=head2 _extract_debuff

Extracts debuff value for specific troop type and attribute from PDL Runtime output.

=head2 _troop_type_from_suffix

Converts troop suffix to display name (e.g., 'ground' => 'Ground Troops').

=cut
