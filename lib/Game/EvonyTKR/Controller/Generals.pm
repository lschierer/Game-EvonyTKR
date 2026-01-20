use v5.42.0;
use utf8::all;
use File::FindLib 'lib';
require Game::EvonyTKR::Model::General;
require Game::EvonyTKR::Model::Data;
use namespace::autoclean;

package Game::EvonyTKR::Controller::Generals {
  use Mooish::Base -standard;
  extends 'Game::EvonyTKR::Controller::ControllerBase';
  # Compose table-related roles
  with 'Game::EvonyTKR::Controller::Role::Tables';
  with 'Game::EvonyTKR::Role::Persistence::TableSessions';
  with 'Game::EvonyTKR::Controller::Role::Generals::Routing';
  with 'Game::EvonyTKR::Role::Constants::BuffConstants';
  with 'Game::EvonyTKR::Role::Constants::GeneralConstants';
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
  sub collection_name {'Generals'}

  my $base    = '/Generals';
  my $refBase = '/Reference/Generals';

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

    # Force initialization of validRoutes
    my $routes = $self->validRoutes;
    $self->logger->info(
      sprintf("Initialized %d valid routes", scalar(keys %$routes)));

    # Add navigation for main generals page (Reference section)
    $self->add_navigation_route(
      $refBase,
      'General Details',
      { order => 21, parent => '/Reference' }
    );

    $self->add_navigation_route($base, 'Generals',
      { order => 5, parent => '/' });

    # ===== /Reference/Generals routes (detail pages) =====

    # Main generals landing page
    $self->router->add(
      $refBase,
      {
        to => sub ($self, $ctx) {
          return $self->index($ctx);
        },
        action => 'http.*',
      }
    );

   # Register static troop type index routes FIRST (before /:name dynamic route)
   # to avoid routing conflicts
    use Game::EvonyTKR::Role::Constants::GeneralConstants;
    my @general_types =
      keys %Game::EvonyTKR::Role::Constants::GeneralConstants::generalKeys;

    $self->logger->info(
      sprintf("Registering %d troop type index routes", scalar(@general_types))
    );

   # Single general detail page (dynamic route - registered after static routes)
    $self->router->add(
      "$refBase/:name",
      {
        to => sub ($self, $ctx, @args) {
          my $name = uri_unescape($args[0]);
          # Ensure UTF-8 decoding
          $name = decode('UTF-8', $name) unless is_utf8($name);
          return $self->show($ctx, $name);
        },
        action => 'http.*',
      }
    );

    # ===== /Generals routes (table comparison pages) =====

    # Main tables landing page
    $self->router->add(
      $base,
      {
        to => sub ($self, $ctx) {
          return $self->tablesIndex($ctx);
        },
        action => 'http.*',
      }
    );

    foreach my $generalType (@general_types) {
      my $ui_target = $self->_ui_target_name($generalType);
      my $route     = "$base/$ui_target";
      $self->add_navigation_route($route, $ui_target,
        { order => 5, parent => '/' });

      $self->router->add(
        $route,
        {
          to => sub ($self, $ctx) {
            return $self->troopTypeTableIndex($ctx, $ui_target);
          },
          action => 'http.*',
        }
      );

      $self->logger->info(sprintf(
        'Registered troop type index route: "%s" for type: "%s"',
        $route, $generalType
      ));

      # Activation index (shows single/pair choice or redirects)
      foreach my $buffActivation ($self->AllowedBuffActivationValues->@*) {
        my $br = "$route/$buffActivation";
        next if ($ui_target =~ /mayor/i && $buffActivation !~ /mayor/i);

        $self->add_navigation_route(
          $br,
          "$buffActivation Comparison Tables",
          { order => 5, parent => '/' }
        );

        $self->router->add(
          $br,
          {
            to => sub ($self, $ctx, @args) {
              return $self->activationIndex($ctx, $ui_target, $buffActivation);
            },
            action => 'http.*',
          }
        );

        $self->logger->info(sprintf(
'Registered troop type index route: "%s" for type: "%s" activation: "%s"',
          $br, $generalType, $buffActivation
        ));
      }

    }

    # Table UI page
    $self->router->add(
      "$base/:uiTarget/:buffActivation/comparison",
      {
        to => sub ($self, $ctx, @args) {
          my $uiTarget       = uri_unescape($args[0]);
          my $buffActivation = uri_unescape($args[1]);
          $uiTarget = decode('UTF-8', $uiTarget) unless is_utf8($uiTarget);
          $buffActivation = decode('UTF-8', $buffActivation)
            unless is_utf8($buffActivation);
          return $self->singleTable($ctx, $uiTarget, $buffActivation);
        },
        action => 'http.*',
      }
    );

    # Catalog endpoint (POST for filter body)
    $self->router->add(
      "$base/:uiTarget/:buffActivation/data.json",
      {
        to => sub ($self, $ctx, @args) {
          my $uiTarget       = uri_unescape($args[0]);
          my $buffActivation = uri_unescape($args[1]);
          $uiTarget = decode('UTF-8', $uiTarget) unless is_utf8($uiTarget);
          $buffActivation = decode('UTF-8', $buffActivation)
            unless is_utf8($buffActivation);
          return $self->singleCatalog($ctx, $uiTarget, $buffActivation);
        },
        action => 'http.post',
      }
    );

    # Details stream (SSE) - must use 'sse.get' action for PAGI SSE protocol
    $self->router->add(
      "$base/:uiTarget/:buffActivation/details-stream",
      {
        to => sub ($self, $ctx, @args) {
          my $uiTarget       = uri_unescape($args[0]);
          my $buffActivation = uri_unescape($args[1]);
          $uiTarget = decode('UTF-8', $uiTarget) unless is_utf8($uiTarget);
          $buffActivation = decode('UTF-8', $buffActivation)
            unless is_utf8($buffActivation);
          return $self->stream_single_details($ctx, $uiTarget, $buffActivation);
        },
        action => 'sse.get',
      }
    );

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
          'Failed to load general with key "%s"', $normalized_key));
        next;
      }

      # Get display name from the general object
      my $display_name = eval { $general->name };
      unless (defined($display_name) && length($display_name)) {
        $self->logger->warn(sprintf(
          'General with key "%s" has no valid name, skipping',
          $normalized_key));
        next;
      }

      # Add to navigation using the proper display name
      eval {
        $self->add_navigation_route("$refBase/$display_name", $display_name,
          { order => 20, parent => $refBase });
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
          $display_name, $refBase, $display_name, $normalized_key
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
        $self->logger->error(
          sprintf('Failed to get listed general "%s"', $general_name));
        next;
      }
      push @{$items}, $general;
    }

    $self->logger->debug(
      sprintf('Generals: %s with %s items', ref($items), scalar(@$items)));

    my $static_content = '';
    my $path           = $ctx->req->path;
    $path =~ s|^/||;
    my $md_file = $self->pages_dir->child("$path/index.md");
    if ($md_file->exists) {
      $static_content = $self->retrieve_rendered_markdown($md_file);
    }

    my $vars = {
      items          => $items,
      title          => 'Generals',
      static_content => $static_content,
      current_year   => (localtime)[5] + 1900,
      css_files      => ['/css/collectionIndex.css'],
      sidebar        => 1,
      navigation     => $self->render_navigation($ctx->req->path),
      site_logo      => $self->site_logo(),
    };

    return $self->template('generals/index.tt', $vars);
  }

  # Tables landing page at /Generals
  sub tablesIndex($self, $ctx) {
    $self->logger->debug("Rendering generals tables landing page");

    # Get all troop types
    use Game::EvonyTKR::Role::Constants::GeneralConstants;
    my @general_types =
      keys %Game::EvonyTKR::Role::Constants::GeneralConstants::generalKeys;

    my @troop_types;
    foreach my $generalType (@general_types) {
      my $ui_target = $self->_ui_target_name($generalType);
      push @troop_types,
        {
        name => $ui_target,
        slug => $ui_target,
        path => "/Generals/$ui_target",
        };
    }

    my $static_content = '';
    my $path           = $ctx->req->path;
    $path =~ s|^/||;
    my $md_file = $self->pages_dir->child("$path/index.md");
    if ($md_file->exists) {
      $static_content = $self->retrieve_rendered_markdown($md_file);
    }

    my $vars = {
      troop_types    => \@troop_types,
      title          => 'General Comparison Tables',
      static_content => $static_content,
      current_year   => (localtime)[5] + 1900,
      css_files      => ['/css/generals.css'],
      sidebar        => 1,
      navigation     => $self->render_navigation($ctx->req->path),
      site_logo      => $self->site_logo(),
    };

    return $self->template('generals/tablesIndex.tt', $vars);
  }

  # Troop type table index at /Generals/:uiTarget
  sub troopTypeTableIndex($self, $ctx, $uiTarget) {
    $self->logger->debug("Rendering table index for troop type: $uiTarget");

    # Get all valid routes for this troop type
    my @routes = $self->get_routes_for_uiTarget($uiTarget);

    unless (@routes) {
      return $self->render_error($ctx, 404,
        "No routes found for troop type: $uiTarget");
    }

    # Sort by buff activation
    my @sorted_routes =
      sort { $a->{buffActivation} cmp $b->{buffActivation} } @routes;

    # Check for markdown content
    my $static_content = '';
    my $path           = $ctx->req->path;
    $path =~ s|^/||;
    my $md_file = $self->pages_dir->child("$path/index.md");
    if ($md_file->exists) {
      $static_content = $self->retrieve_rendered_markdown($md_file);
    }

    my $vars = {
      uiTarget       => $uiTarget,
      routes         => \@sorted_routes,
      static_content => $static_content,
      title          => "$uiTarget Tables",
      current_year   => (localtime)[5] + 1900,
      css_files      => ['/css/generals.css'],
      sidebar        => 1,
      navigation     => $self->render_navigation($ctx->req->path),
      site_logo      => $self->site_logo(),
    };

    return $self->template('generals/troopTypeTableIndex.tt', $vars);
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
        "General '$name' (normalized: '$normalized_name') not found");
      return $self->render_error($ctx, 404, "General not found");
    }

    $self->logger->debug("Retrieved general object");

    # Debug: Check what we got
    $self->logger->debug(sprintf(
      "General data - name: %s, type: %s, ascending: %s, book: %s",
      $general->can('name') ? ($general->name // 'undef') : 'no name method',
      $general->can('type') ? (ref($general->type) || $general->type // 'undef')
      : 'no type method',
      $general->can('ascending') ? ($general->ascending // 'undef')
      : 'no ascending method',
      $general->can('builtInBookName') ? ($general->builtInBookName // 'undef')
      : 'no book method'
    ));

    # Get ascending attributes for this general if applicable
    my $ascending_attrs;
    eval {
      if ($general->can('ascending') && $general->ascending) {
        $self->logger->debug("General is ascending, looking up attributes");
        my $aa_loader = $self->ascending_attributes_loader();
        if ($aa_loader) {
          my $gen_name =
            $general->can('name') ? $general->name : $normalized_name;
          my $normalized_general_name = $self->normalize($gen_name);
          $self->logger->debug(sprintf(
            "Looking up ascending attrs for '%s' (normalized: '%s')",
            $gen_name, $normalized_general_name
          ));
          $ascending_attrs =
            $aa_loader->get_for_general($normalized_general_name);
          $self->logger->debug(sprintf("Ascending attrs lookup result: %s",
            $ascending_attrs ? 'found' : 'not found'));
        }
        else {
          $self->logger->warn("No ascending attributes loader available");
        }
      }
      else {
        $self->logger->debug(
          "General is not ascending or ascending field not set");
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
          my $book_name            = $general->builtInBookName;
          my $normalized_book_name = $self->normalize($book_name);
          $built_in_book = $books_loader->get_book($normalized_book_name);
          $self->logger->debug(sprintf(
            "Looked up book '%s' (normalized: '%s'), found: %s",
            $book_name, $normalized_book_name,
            $built_in_book ? 'yes' : 'no'
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
      my $specialty1      = $ctx->req->query('specialty1')    // 'gold';
      my $specialty2      = $ctx->req->query('specialty2')    // 'gold';
      my $specialty3      = $ctx->req->query('specialty3')    // 'gold';
      my $specialty4      = $ctx->req->query('specialty4')    // 'gold';
      my $activation      = $ctx->req->query('activation')    // 'Attacking';

      $self->logger->debug(sprintf(
"Buff params: activation=%s, ascending=%s, covenant=%s, specialties=%s/%s/%s/%s",
        $activation, $ascending_level, $covenant_level, $specialty1,
        $specialty2, $specialty3,      $specialty4
      ));

      # Compute buffs using PDL Runtime
      eval {
        $buff_summaries = $self->pdl_runtime->get_buff_summary(
          general    => $gen_name,
          activation => $activation,
          filters    => {
            ascendingLevel => $ascending_level,
            covenantLevel  => $covenant_level,
            specialty1     => $specialty1,
            specialty2     => $specialty2,
            specialty3     => $specialty3,
            specialty4     => $specialty4,
            generic1       => 'level4',    # Default to level 4 generic books
          }
        );

        $self->logger->debug("Successfully computed buff summaries");
      };
      if ($@) {
        $self->logger->error("Error computing buffs: $@");
      }
    }

    my $vars = {
      item           => $general,
      ascending      => $ascending_attrs,
      builtInBook    => $built_in_book,
      buff_summaries => $buff_summaries,    # Add computed buffs if available
      title          => "Details for $gen_name",
      current_year   => (localtime)[5] + 1900,
      css_files      => ['/css/collectionDetails.css'],
      sidebar        => 1,
      navigation     => $self->render_navigation($ctx->req->path),
      site_logo      => $self->site_logo(),
    };

    $self->logger->debug("About to render generals/details.tt");
    my $result = $self->template('generals/details.tt', $vars);
    $self->logger->debug("Render returned: " . ref($result));
    return $result;
  }

  # Single general table UI page
  sub singleTable ($self, $ctx, $uiTarget, $buffActivation) {
    $self->logger->debug(
      "Rendering single general table for $uiTarget / $buffActivation");

  # Validate route using Routing role - use try_lookup_route which doesn't croak
    my $route_meta = $self->try_lookup_route($uiTarget, $buffActivation);
    unless ($route_meta) {
      $self->logger->error("Invalid single route: $uiTarget | $buffActivation");
      return $self->render_error($ctx, 404,
        "Invalid general type or buff activation");
    }

    # Extract validated metadata
    my $generalType     = $route_meta->{generalType};
    my $buff_activation = $route_meta->{buffActivation};
    my $ui_target       = $route_meta->{uiTarget};

    # Get filter parameters with defaults
    my $ascendingLevel = $ctx->req->query('ascendingLevel') // 'red5';
    my $covenantLevel  = $ctx->req->query('covenantLevel')  // 'civilization';
    my @specialties =
      map { $ctx->req->query("specialty$_") // 'gold' } (1 .. 4);

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

    unless ($data_model->checkCovenantLevel($covenantLevel)) {
      $self->logger->warn(
        "Invalid covenantLevel: $covenantLevel, using 'civilization'");
      $covenantLevel = 'civilization';
    }

    @specialties = $data_model->normalizeSpecialtyLevels(@specialties);

    # Prepare template variables
    my $vars = {
      generalType      => $generalType,
      buffActivation   => $buff_activation,
      uiTarget         => $ui_target,
      ascendingLevel   => $ascendingLevel,
      covenantLevel    => $covenantLevel,
      specialties      => \@specialties,
      PrimaryFormTitle => 'General Buff Selection',
      title        => "Single General Table - $ui_target / $buff_activation",
      current_year => (localtime)[5] + 1900,
      css_files    => ['/css/GeneralTable.css'],
      sidebar      => 1,
      navigation   => $self->render_navigation($ctx->req->path),
      site_logo    => $self->site_logo(),
    };

    return $self->template('generals/GeneralTableSingle.tt', $vars);
  }

  # Single general catalog endpoint (returns list of generals)
  async sub singleCatalog ($self, $ctx, $uiTarget, $buffActivation) {
    $self->logger->debug(
      "Fetching single general catalog for $uiTarget / $buffActivation");

    # Validate route - use try_lookup_route which returns undef without croaking
    my $route_meta = $self->try_lookup_route($uiTarget, $buffActivation);
    unless ($route_meta) {
      $self->logger->error("Invalid single route: $uiTarget | $buffActivation");
      return $self->render_error($ctx, 404,
        "Invalid general type or buff activation");
    }

    my $generalType = $route_meta->{generalType};

    # Check for POST body with primaries filter (optional)
    my $requested_primaries = [];
    if ($ctx->req->method eq 'POST') {
      my $json_data = await $ctx->req->json;    # await the async json parse
      $requested_primaries = $json_data->{primaries} // [];
      $self->logger->debug(sprintf("Catalog request with %d primaries filter",
        scalar(@$requested_primaries)));
    }

    # Generate unique session ID
    my $session_id = $self->generate_table_session_id($requested_primaries);

    # Get all generals matching the generalType via specialties
    my $generals_loader = $self->generals_loader();
    unless ($generals_loader) {
      return $self->render_error($ctx, 500, "Generals data not loaded");
    }

    $self->logger->debug("Looking for generals with type: $generalType");

    my @all_generals;
    foreach my $general_key ($generals_loader->list_generals->@*) {
      my $general = $generals_loader->get_general($general_key);
      next unless $general;

      # Check if general's type matches the requested generalType
      my $general_types = $general->type // [];
      $general_types = [$general_types] unless ref($general_types) eq 'ARRAY';

      # Match against the generalType key (e.g., 'siege_specialist', 'mayor')
      my $matches = grep { lc($_) eq lc($generalType) } @$general_types;
      next unless $matches;

      push @all_generals, $general;
    }

    $self->logger->debug(
      sprintf("Found %d generals for %s", scalar(@all_generals), $generalType));

    # If primaries filter provided, filter to requested generals
    my @filtered_generals;
    if (scalar(@$requested_primaries) > 0) {
      my %requested = map { $_ => 1 } @$requested_primaries;
      @filtered_generals = grep { exists $requested{ $_->name } } @all_generals;

      $self->logger->debug(sprintf(
        "Filtered to %d generals from %d total",
        scalar(@filtered_generals),
        scalar(@all_generals)
      ));
    }
    else {
      @filtered_generals = @all_generals;
    }

    # Convert to wire format (stubs for catalog)
    my @selected = map { { primary => $_->name } } @filtered_generals;

    # Store session for streaming endpoint
    my @general_keys = map { $_->{primary} } @selected;
    $self->store_table_session(
      $session_id,
      {
        generalType    => $generalType,
        buffActivation => $buffActivation,
        items          => \@general_keys,
        ttl            => 3600,              # 1 hour
      }
    );

    # Return catalog response
    return await $ctx->res->json({
      sessionId => $session_id,
      selected  => \@selected,
    });
  }

  # Stream single general buff details via SSE (PAGI::SSE version)
  # Uses flat async pattern - directly handles SSE without wrapper
  async sub stream_single_details ($self, $ctx, $uiTarget, $buffActivation) {
    # Get SSE object directly from context and consume immediately
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

    # Validate route first - use try_lookup_route which doesn't croak
    my $route_meta = $self->try_lookup_route($uiTarget, $buffActivation);
    unless ($route_meta) {
      $self->logger->error("Invalid single route: $uiTarget | $buffActivation");
      await $self->send_complete_event($sse, $run_id, 0);
      await $sse->run unless $sse->is_closed;
      return;
    }

    my $generalType = $route_meta->{generalType};
    my $activation  = $route_meta->{buffActivation};

# Try to retrieve session data, but fallback to fetching all generals if not found
    my $session_data = $self->get_table_session($session_id);
    my @general_names;

    if ($session_data) {
      @general_names = @{ $session_data->{items} };
      $self->logger->debug(sprintf(
'stream_single_details: uiTarget=%s, buffActivation=%s, runId=%s, session items=%d',
        $uiTarget, $buffActivation, $run_id, scalar(@general_names)
      ));
    }
    else {
      # Session not found - fetch generals directly (fallback for robustness)
      $self->logger->warn(
"Session $session_id not found, fetching generals directly for $generalType"
      );

      my $generals_loader = $self->generals_loader();
      unless ($generals_loader) {
        $self->logger->error("Generals data not loaded");
        await $self->send_complete_event($sse, $run_id, 0);
        await $sse->run unless $sse->is_closed;
        return;
      }

      foreach my $general_key ($generals_loader->list_generals->@*) {
        my $general = $generals_loader->get_general($general_key);
        next unless $general;

        # Check if general's type matches the requested generalType
        my $general_types = $general->type // [];
        $general_types = [$general_types] unless ref($general_types) eq 'ARRAY';

        # Match against the generalType key (e.g., 'siege_specialist', 'mayor')
        my $matches = grep { lc($_) eq lc($generalType) } @$general_types;
        next unless $matches;

        push @general_names, $general->name;
      }

      $self->logger->debug(sprintf(
        'stream_single_details (fallback): found %d generals for %s',
        scalar(@general_names), $generalType
      ));
    }

    # Extract filter parameters
    my $ascendingLevel = $ctx->req->query('ascendingLevel') // 'red5';
    my $covenantLevel  = $ctx->req->query('covenantLevel')  // 'civilization';
    my @specialties =
      map { $ctx->req->query("specialty$_") // 'gold' } (1 .. 4);

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

    $self->logger->debug(sprintf(
      'Computing buffs for %d generals', scalar(@general_names)));

    # Process generals using the streaming helper
    await $self->process_items_streaming(
      $sse,
      {
        items        => \@general_names,
        run_id       => $run_id,
        item_type    => 'generals',
        process_item => async sub ($general_name, $idx) {
          # Get general object
          my $generals_loader = $self->generals_loader();
          my $normalized_name = $self->normalize($general_name);
          my $general         = $generals_loader->get_general($normalized_name);

          unless ($general) {
            $self->logger->error("Cannot load general: $general_name");
            return undef;
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
          # primary must match the General Zod schema with all required fields
          my $ba     = $general->basicAttributes;
          my $result = {
            runId => 0+ $run_id,
            data  => {
              primary => {
                id              => $general->id,
                name            => $general->name,
                type            => $general->type,
                ascending       => $general->ascending ? \1 : \0,
                builtInBookName => $general->builtInBookName // '',
                specialtyNames  => $general->specialtyNames  // [],
                basicAttributes => {
                  attack => {
                    base      => $ba->attack->base,
                    increment => $ba->attack->increment
                  },
                  defense => {
                    base      => $ba->defense->base,
                    increment => $ba->defense->increment
                  },
                  leadership => {
                    base      => $ba->leadership->base,
                    increment => $ba->leadership->increment
                  },
                  politics => {
                    base      => $ba->politics->base,
                    increment => $ba->politics->increment
                  },
                },
              },
              marchbuff =>
                $buff_summary->{buffValues}->{'Ground Troops'}->{'March Size'}
                // 0,
              attackbuff => $self->_extract_buff(
                $buff_summary->{buffValues},
                $troop_suffix, 'Attack'
              ),
              defensebuff => $self->_extract_buff(
                $buff_summary->{buffValues},
                $troop_suffix, 'Defense'
              ),
              hpbuff => $self->_extract_buff(
                $buff_summary->{buffValues},
                $troop_suffix, 'HP'
              ),
              groundattackdebuff => $self->_extract_debuff(
                $buff_summary->{debuffValues},
                'Ground Troops', 'Attack'
              ),
              grounddefensedebuff => $self->_extract_debuff(
                $buff_summary->{debuffValues},
                'Ground Troops', 'Defense'
              ),
              groundhpdebuff => $self->_extract_debuff(
                $buff_summary->{debuffValues},
                'Ground Troops', 'HP'
              ),
              mountedattackdebuff => $self->_extract_debuff(
                $buff_summary->{debuffValues},
                'Mounted Troops', 'Attack'
              ),
              mounteddefensedebuff => $self->_extract_debuff(
                $buff_summary->{debuffValues},
                'Mounted Troops', 'Defense'
              ),
              mountedhpdebuff => $self->_extract_debuff(
                $buff_summary->{debuffValues},
                'Mounted Troops', 'HP'
              ),
              rangedattackdebuff => $self->_extract_debuff(
                $buff_summary->{debuffValues},
                'Ranged Troops', 'Attack'
              ),
              rangeddefensedebuff => $self->_extract_debuff(
                $buff_summary->{debuffValues},
                'Ranged Troops', 'Defense'
              ),
              rangedhpdebuff => $self->_extract_debuff(
                $buff_summary->{debuffValues},
                'Ranged Troops', 'HP'
              ),
              siegeattackdebuff => $self->_extract_debuff(
                $buff_summary->{debuffValues},
                'Siege Machines', 'Attack'
              ),
              siegedefensedebuff => $self->_extract_debuff(
                $buff_summary->{debuffValues},
                'Siege Machines', 'Defense'
              ),
              siegehpdebuff => $self->_extract_debuff(
                $buff_summary->{debuffValues},
                'Siege Machines', 'HP'
              ),
            }
          };

          $self->logger->debug(sprintf(
            'Computed general %d/%d: %s',
            $idx + 1, scalar(@general_names), $general_name
          ));

          return $result;
        },
      }
    );

    # Wait for client disconnect (if not already closed)
    await $sse->run unless $sse->is_closed;

    return;
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

  # Activation index - shows single/pair choice or redirects
  sub activationIndex ($self, $ctx, $uiTarget, $buffActivation) {
    $self->logger->debug(
      "Rendering activation index for: $uiTarget / $buffActivation");

    # Validate route - use try_lookup_route which returns undef without croaking
    my $route_meta = $self->try_lookup_route($uiTarget, $buffActivation);
    unless ($route_meta) {
      $self->logger->error("Invalid route: $uiTarget | $buffActivation");
      return $self->render_error($ctx, 404,
        "Invalid general type or buff activation");
    }

    # Check if pairs exist for this combination by querying the pairs_loader
    my $has_pairs    = 0;
    my $pairs_loader = $self->pairs_loader();
    if ($pairs_loader) {
      # Map generalType (e.g., 'ground_specialist') to loader type key
      my $generalType = $route_meta->{generalType};
      my $pair_count  = $pairs_loader->pair_count_for_type($generalType);
      $has_pairs = $pair_count > 0 ? 1 : 0;
      $self->logger->debug(sprintf(
        "Pairs check for %s: %d pairs found",
        $generalType, $pair_count
      ));
    }

    if (!$has_pairs) {
      # No pairs exist, redirect directly to single table
      $self->logger->debug(
        "No pairs for $uiTarget/$buffActivation, redirecting to single table");
      return $ctx->res->redirect(
        "/Generals/$uiTarget/$buffActivation/comparison");
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

    return $self->template('generals/activationIndex.tt', $vars);
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
- GET /Generals/:troopType - Navigation page for troop type
- GET /Generals/:uiTarget/:buffActivation - Choice/redirect page
- GET /Generals/:uiTarget/:buffActivation/comparison - Table UI
- POST /Generals/:uiTarget/:buffActivation/data.json - Catalog endpoint
- GET /Generals/:uiTarget/:buffActivation/details-stream - SSE buff streaming

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
