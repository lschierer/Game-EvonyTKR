use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require JSON::PP;
require YAML::PP;
require Mojo::Promise;
require Mojo::Util;
require List::Util;

require Game::EvonyTKR::Model::General;
require Game::EvonyTKR::Model::General::Pair;
require Game::EvonyTKR::Model::Buff::Summarizer::Single;
require Game::EvonyTKR::Control::Generals::Routing;
require Game::EvonyTKR::Model::Data;
require Game::EvonyTKR::Model::Base;

require UUID;
require Data::Printer;
use namespace::autoclean;

package Game::EvonyTKR::Controller::Generals {
  use Mojo::Base 'Game::EvonyTKR::Controller::ControllerBase';
  use Mojo::Base 'Game::EvonyTKR::Role::StaticPages', -role;
  use Mojo::IOLoop;
  use Mojo::Promise;
  use Mojo::JSON     qw(to_json encode_json);
  use MIME::Base64   qw(encode_base64);
  use List::AllUtils qw( all any none );
  require Game::EvonyTKR::External::General::Summarizer;

  use Carp;

  has prereqs => sub {
    return [qw(
      load_all_generals
      load_all_builtin_books
      load_all_generic_books
      load_all_covenants
      load_all_specialties
      load_all_ascending_attributes
      build_general_indexes
      load_all_pair_builders
      reduce_coordinator
      monitor_loaders
    )];
  };

  sub get_general_routing ($self) {
    state $routing = Game::EvonyTKR::Control::Generals::Routing->new();
    return $routing;
  }

  # Specify which collection this controller handles
  has collection_name => 'generals';

  has controller_name => 'Generals';

  my $base = '/Generals';

  my $reference_base = '/Reference/Generals';

  my $session_store = {};

  my $max_concurrency = 15;

  has getBase => sub ($self) {
    return $base;
  };

  sub register($c, $app, $config = {}) {
    $c->log_info("Registering routes for " . ref($c));
    $c->SUPER::register($app, $config);

    if ($app->mode eq 'development') {
      $c->get_general_routing->set_debug(1);
    }

    eval {
      $c->log_debug(
        sprintf('%s calling setup_event_handlers', __PACKAGE__));
      $c->setup_event_handlers($app);
      1;
    } or do {
      $c->log_error(
        sprintf('%s hit an error in setup_event_handlers: %s', __PACKAGE__, $@)
      );
    };
    eval {
      $c->log_debug(sprintf('%s calling setup_helpers', __PACKAGE__));
      $c->setup_helpers($app);
      1;
    } or do {
      $c->log_error(
        sprintf('%s hit an error in setup_helpers: %s', __PACKAGE__, $@));
    };
    $c->log_debug(sprintf('%s calling setup_routes', __PACKAGE__));
    $c->setup_routes($app);
    $c->log_debug(sprintf('%s register complete', __PACKAGE__));

  }

  sub setup_event_handlers ($c, $app) {
    $c->log_debug(sprintf('setup_event_handlers for %s', __PACKAGE__));

  }

  sub setup_helpers($c, $app) {

    $app->helper(
      general_routing => sub {
        return $c->get_general_routing();
      }
    );

    $app->plugins->emit(
      get_general_routing_available => { routing => $c->get_general_routing() }
    );
  }

  sub setup_routes($c, $app) {
    my $controller_name = $c->controller_name();
    $c->log_debug("got controller_name $controller_name.");

    my $mainRoutes      = $app->routes->any($base);
    my $referenceRoutes = $app->routes->any($reference_base);

    $mainRoutes->get('/')
      ->to(controller => $controller_name, action => 'index')
      ->name("${base}_index");

    $referenceRoutes->get('/')
      ->to(controller => $controller_name, action => 'index');

    # Dynamic catch-all route for individual generals
    $referenceRoutes->get('/:name')
      ->to(controller => $controller_name, action => 'show')
      ->name('general_details');

    # a routes for single general tables
    # and the index routes for troop type categories that go under /Generals
    # and thus are managed by this controller
    eval {
      # route for directory indices
      $mainRoutes->get('/:uiTarget')->requires(is_valid_uiTarget => 1)->to(
        controller => 'Generals',
        action     => 'uiTarget_index',
      )->name('General_dynamic_uiTarget_index');

      # check that :uiTarget is a valid route, otherwise this becomes
      # too broad a match and prevents anything else from matching.
      $app->routes->add_condition(
        is_valid_uiTarget => sub ($route, $controller, $captures, $arg) {
          my $ui = $captures->{uiTarget};
          # make this deterministic: compare exact left side of key
          my $slug = $c->get_general_routing()->_slugify($ui);
          my $ok   = 0;
          for my $key (keys $c->get_general_routing()->validRoutes->%*) {
            my ($left) = split /\|/, $key, 2;
            if ($left eq $slug) { $ok = 1; last }
          }
          return $ok;
        }
      );

      # check that :uiTarget/:buffActivation is a valid route combination
      $app->routes->add_condition(
        is_valid_buffActivation => sub ($route, $controller, $captures, $arg) {
          my ($ui, $buff) = @$captures{qw(uiTarget buffActivation)};
          my $ok = $c->get_general_routing()->has_route($ui, $buff) ? 1 : 0;
          $c->log_debug("check ui='$ui' buff='$buff' -> $ok");
          return $ok;    # never die here
        }
      );
      # route for directory indices
      $mainRoutes->get('/:uiTarget/:buffActivation')
        ->requires(is_valid_buffActivation => 1)
        ->to(
        controller => 'Generals',
        action     => 'buffActivation_index',
        )->name('General_dynamic_buffActivation_index');

      # routes for the user interface for single general tables
      $mainRoutes->get('/:uiTarget/:buffActivation/comparison')->to(
        controller => 'Generals',
        action     => 'singleTable',
      )->name('General_dynamic_singleTable');

      # route to generate the lists of names for single general tables
      $mainRoutes->any(
        ['GET', 'POST'] => '/:uiTarget/:buffActivation/data.json')->to(
        controller => 'Generals',
        action     => 'singleCatalog',
        )->name('Generals_dynamic_singleData');

      # route to generate data on a single row within the table
      $mainRoutes->get('/:uiTarget/:buffActivation/:isPrimary/details-stream')
        ->to(
        controller => 'Generals',
        action     => 'single_details_stream',
        )->name('Generals_dynamic_singleDetails');
    };
    if ($@) {
      my $error =
        sprintf('error building dynamic routes for Generals Controller: %s',
        $@);
      $c->log_error($error);
      if ($app->mode eq 'development') {
        croak($error);
      }
    }
    $c->setup_navigation($app);
    $c->static_pages($app, $base);
    $c->static_pages($app, $reference_base);
  }

  sub setup_navigation($c, $app) {
    $app->add_navigation_item({
      title  => 'General Details',
      path   => $reference_base,
      parent => '/Reference',
      order  => 10,
    });

    # nav items for the dynamic routes
    foreach my $route ($c->get_general_routing()->all_valid_routes()) {
      $c->log_debug("building nav items for "
          . $route->{uiTarget} . "|"
          . $route->{buffActivation});
      my $printableUI = $route->{uiTarget} =~ s/-/ /rg;

      $app->add_navigation_item({
        title  => sprintf('Picking %s',   $printableUI),
        path   => sprintf('/Generals/%s', $route->{uiTarget}),
        parent => '/Generals',
        order  => 20 + ($route->{order} || 0),
      });

      $app->add_navigation_item({
        title => sprintf(
          'Picking %s Generals for %s',
          $printableUI, $route->{buffActivation}
        ),
        path => sprintf('/Generals/%s/%s',
          $route->{uiTarget}, $route->{buffActivation}),
        parent => sprintf('/Generals/%s', $route->{uiTarget}),
        order  => 20 + ($route->{order} || 0),
      });

      my $path = sprintf('/Generals/%s/%s/comparison',
        $route->{uiTarget}, $route->{buffActivation});
      $app->add_navigation_item({
        title =>
          sprintf('%s %s Comparison', $printableUI, $route->{buffActivation}),
        path   => $path,
        parent => sprintf('/Generals/%s/%s',
          $route->{uiTarget}, $route->{buffActivation}),
        order => 20 + ($route->{order} || 0),
      });
    }
  }

  sub _build_general_routes($c, $general_name, $app) {
    # Build navigation item for this general
    use Encode qw(decode_utf8 is_utf8);

    # Ensure the name is properly decoded as UTF-8
    my $display_name =
      is_utf8($general_name) ? $general_name : decode_utf8($general_name);
    my $gr = "/Reference/Generals/$display_name";

    $c->log_debug(sprintf(
      "Building nav for: %s (is_utf8: %s, path: %s)",
      $display_name, is_utf8($display_name) ? 'yes' : 'no', $gr
    ));

    $app->add_navigation_item({
      title  => $display_name,
      path   => $gr,
      parent => '/Reference/Generals',
      order  => 20,
    });
  }

  sub _ensure_navigation_built($c) {
    state $nav_built = 0;
    return if $nav_built;

    # Check if generals are loaded
    my @general_names = eval { $c->list_generals()->@* };
    return unless @general_names;

    # Try to load one general to verify they're actually available
    my $test_general = eval { $c->get_general($general_names[0]) };
    return unless $test_general;

    # Build navigation for all generals
    foreach my $general_name (@general_names) {
      my $general = eval { $c->get_general($general_name) };
      next unless $general;
      $c->_build_general_routes($general->name, $c->app);
    }

    $nav_built = 1;
    $c->log_info(
      "Built navigation items for " . scalar(@general_names) . " generals");
  }

  sub get_generals_by_type ($self, $generalType) {
    my @all_generals = $self->get_generals()->@*;
    $self->log_debug(
      sprintf('get_generals returned %s generals', scalar(@all_generals)));

    my @selected = grep {
      my $gen    = $_;
      my $result = 0;

      eval {
        my $type = $gen->type;

        if (!defined $type) {
          # Skip generals with no type
          $result = 0;
        }
        elsif (ref($type) eq 'ARRAY') {
          $result = any { $_ eq $generalType } @$type;
        }
        else {
          $result = ($type eq $generalType);
        }
        1;
      } or do {
        $self->log_error(sprintf(
          'Error filtering general %s: %s',
          $gen->name // 'unknown', $@
        ));
        $result = 0;
      };

      $result;
    } @all_generals;

    $self->log_debug(sprintf(
      'grep filtered the list from %s to %s',
      scalar(@all_generals), scalar(@selected)
    ));
    return \@selected;
  }

  sub index($c) {

    # Build navigation items if not already done
    $c->_ensure_navigation_built();

    my $collection = collection_name();
    $c->log_debug("Rendering index for $collection");

    my $rp = $c->req->url->path->to_string;
    # Remove trailing slash from pages
    if ($rp =~ qr{/$}) {
      my $canonical = $rp;
      $canonical =~ s{/$}{};
      return $c->redirect_to($canonical, 301);
    }

    # Check if markdown exists for this collection
    my $distDir       = Mojo::File::Share::dist_dir('Game::EvonyTKR');
    my $markdown_path = $distDir->child("pages/Generals/index.md");

    my @parts     = split(/::/, ref($c));
    my $baseClass = pop(@parts);
    my $base      = $c->getBase();
    $c->log_debug("Generals index method has base $base");

    my $items = $c->get_generals() // [];
    $c->log_debug(
      sprintf('Items: %s with %s generals.', ref($items), scalar(@$items)));
    $c->stash(
      linkBase        => $base,
      items           => $items,
      collection_name => $collection,
      controller_name => $baseClass,
    );

    if (-f $markdown_path) {
      $c->log_debug(
        "rendering /Generals/ with markdown index content from $markdown_path");
      # Render with markdown
      $c->stash(template => '/generals/index');

      return $c->render_markdown_page($markdown_path,
        { template => 'generals/index' });
    }
    else {
      $c->log_debug("no markdown index content found at $markdown_path");
      # Render just the items
      return $c->render(template => '/generals/index');
    }
  }

  sub uiTarget_index($self) {

    my $uiTarget = $self->param('uiTarget');

    my $rp = $self->req->url->path->to_string;
    # Remove trailing slash from pages
    if ($rp =~ qr{/$}) {
      my $canonical = $rp;
      $canonical =~ s{/$}{};
      return $self->redirect_to($canonical, 301);
    }

    my @valid_routes =
      $self->get_general_routing->get_routes_for_uiTarget($uiTarget);
    $self->log_debug("found valid_routes "
        . Data::Printer::np(@valid_routes)
        . "for $uiTarget");
    # Validate the uiTarget parameter
    unless (@valid_routes) {
      return $self->reply->not_found;
    }

    # Stash data for the template
    $self->stash(
      uiTarget => $uiTarget,
      routes   => \@valid_routes,
      title    => "Picking $uiTarget"
    );

    # Check for static content
    my $distDir       = Mojo::File::Share::dist_dir('Game::EvonyTKR');
    my $markdown_path = $distDir->child("pages/Generals/$uiTarget/index.md");
    $self->log_debug("looking for index at $markdown_path");

    if (-f $markdown_path) {
      # Render with markdown
      $self->stash(template => "generals/uiTarget/index");
      return $self->render_markdown_page($markdown_path,
        { template => "generals/uiTarget/index_with_file" });
    }
    else {
      # Render just the dynamic content
      return $self->render(template => 'generals/uiTarget/index');
    }
  }

  sub buffActivation_index($self) {
    my $uiTarget       = $self->param('uiTarget');
    my $buffActivation = $self->param('buffActivation');

    my $rp = $self->req->url->path->to_string;
    # Remove trailing slash from pages
    if ($rp =~ qr{/$}) {
      my $canonical = $rp;
      $canonical =~ s{/$}{};
      return $self->redirect_to($canonical, 301);
    }

    my $route =
      $self->get_general_routing->lookup_route($uiTarget, $buffActivation);

    # Validate the parameters
    unless ($route) {
      return $self->reply->not_found;
    }

    # Stash data for the template
    $self->stash(
      uiTarget       => $uiTarget,
      buffActivation => $buffActivation,
      route          => $route,
      title          => "Picking $uiTarget Generals for $buffActivation"
    );

    # Check for static content
    my $distDir = Mojo::File::Share::dist_dir('Game::EvonyTKR');
    my $markdown_path =
      $distDir->child("pages/Generals/$uiTarget/$buffActivation/index.md");

    if (-f $markdown_path) {
      # Render with markdown
      $self->stash(
        template => "generals/uiTarget/buffActivation/index_with_file");
      return $self->render_markdown_page($markdown_path,
        { template => "generals/uiTarget/buffActivation/index_with_file" });
    }
    else {
      # Render just the dynamic content
      return $self->render(
        template => 'generals/uiTarget/buffActivation/index');
    }
  }

  sub show ($c) {
    return if $c->check_prereqs_or_wait($c->prereqs);

    # Build navigation items if not already done
    $c->_ensure_navigation_built();

    $c->log_debug("start of show method");

    # Get name from URL - Mojolicious should already decode it
    use Encode qw(decode is_utf8);
    my $name = $c->param('name');

    $c->log_debug(sprintf(
      "Raw param: %s (is_utf8: %s, bytes: %s)",
      $name,
      is_utf8($name) ? 'yes' : 'no',
      join(' ', map { sprintf('%02x', ord($_)) } split //, $name)
    ));

    # Mojolicious should handle UTF-8, but double-check
    $name = decode('UTF-8', $name) unless is_utf8($name);

    $c->log_debug(sprintf(
      "After decode: %s (is_utf8: %s)",
      $name, is_utf8($name) ? 'yes' : 'no'
    ));

    # Canonicalize trailing slash
    if ((my $rp = $c->req->url->path->to_string) =~ m{/$}) {
      (my $canonical = $rp) =~ s{/$}{};
      return $c->redirect_to($canonical, 301);
    }

    my $expected_list  = [map { $c->normalize($_) } $c->list_generals()->@*];
    my %expected       = map { $_ => 1 } $expected_list->@*;
    my $expected_total = scalar keys %expected;

    # Helper to render "pending" with proper headers / negotiation
    my $render_pending = sub ($why, $maybe_name = undef) {
      my $retry = 3;                  # seconds
      my $h     = $c->res->headers;
      $h->header('Retry-After'   => $retry);
      $h->header('Cache-Control' => 'no-store');

      # JSON/AJAX? return a 202 with status info
      if (($c->stash('format') // '') eq 'json' || $c->req->is_xhr) {
        return $c->render(
          status => 202,
          json   => {
            status          => 'pending',
            reason          => $why,
            name            => $maybe_name,
            expected_total  => $expected_total,
            retry_after_sec => $retry,
          }
        );
      }

      # HTML pending page (include a soft auto-refresh)
      $c->stash(
        pending_reason  => $why,
        pending_name    => $maybe_name,
        expected_total  => $expected_total,
        retry_after_sec => $retry,
      );
      # Your template can include:
      # <meta http-equiv="refresh" content="<%= stash('retry_after_sec') %>">
      return $c->render(status => 202, template => 'generals/pending');
    };

    # 1) If we don't yet know any generals at all, show global "pending"
    if ($expected_total == 0) {
      $c->log_info("No generals expected yet; returning pending page.");
      return $render_pending->('none-available-yet');
    }

    # 2) If a name is provided, validate it against the expected list
    if (defined $name && length $name) {
      # Normalize the name to match filesystem-based list
      my $normalized_name = $c->normalize($name);

      unless ($expected{$normalized_name}) {
        # invalid string: not a known/expected general name → proper 404
        $c->log_warn(sprintf(
          'Unknown general name "%s" (normalized "%s", should be one of %s)',
          $name, $normalized_name,
          join(',', map { sprintf('"%s"', $_) } $expected_list->@*)
        ));
        return $c->continue; # Let other routes (like static pages) try to match
      }

 # Name is valid/expected; check if it's loaded (use normalized name for lookup)
      my $general = $c->get_general($normalized_name);
      unless ($general) {
        $c->log_info(
          "General '$name' expected but not loaded yet; pending.");
        return $render_pending->('name-expected-but-not-ready', $name);
      }

      # Loaded -> continue with existing behavior
      $c->log_debug("got general of type " . blessed($general));
      my $calculate_buffs = $c->param('calculate_buffs') // 0;
      $c->stash(item => $general);

      if ($calculate_buffs) {
        my $covenantLevel  = $c->param('covenantLevel')  // 'civilization';
        my $ascendingLevel = $c->param('ascendingLevel') // 'red5';
        my @specialties    = map { $c->param($_) // 'gold' }
          qw(specialty1 specialty2 specialty3 specialty4);

        my $data_model = Game::EvonyTKR::Model::Data->new();
        if (none { $_ eq $covenantLevel }
          @{ $data_model->CovenantCategoryValues }) {
          $c->log_warn(
            "Invalid covenantLevel: $covenantLevel, defaulting.");
          $covenantLevel = 'civilization';
        }
        if (none { $_ eq $ascendingLevel }
          $data_model->AscendingAttributeLevelValues()) {
          $c->log_warn(
            "Invalid ascendingLevel: $ascendingLevel, defaulting.");
          $ascendingLevel = 'red5';
        }
        @specialties = $data_model->normalizeSpecialtyLevels(@specialties);

        my $targetType;
        if (ref $general->type eq 'ARRAY') {
          $targetType = $general->type->[0] if @{ $general->type };
        }
        else { $targetType = $general->type }
        $targetType //= '';
        $targetType =~ s/_/ /;
        $targetType =~ s/(\w)(\w+) specialist/\U$1\L$2 \UT\Lroops/;
        $targetType =~ s/Siege Troops/Siege Machines/;

        state $covenant_helper //= do {
          my $helper = eval {
            Mojo::Base->new->with_roles(
              'Game::EvonyTKR::Role::Logging',
              'Game::EvonyTKR::Role::Common',
              'Game::EvonyTKR::Role::Persistence'
            );
          };
          if ($@) {
            $c->log_error("Cannot create covenant helper: $@");
            return;
          }
          $helper;
        };

        $c->log_debug("Using $targetType as targetType for $name");
        my $summarizer = Game::EvonyTKR::Model::Buff::Summarizer::Single->new(
          general        => $general,
          isPrimary      => 1,
          targetType     => $targetType,
          activationType => 'Attacking',
          ascendingLevel => $ascendingLevel,
          covenantLevel  => $covenantLevel,
          specialty1     => $specialties[0],
          specialty2     => $specialties[1],
          specialty3     => $specialties[2],
          specialty4     => $specialties[3],
        );

        $summarizer->updateBuffs();
        $summarizer->updateDebuffs();

        $c->stash(
          'buff-summaries' => {
            marchIncrease =>
              $summarizer->buffValues->{$targetType}->{'March Size'} // 0,
            attackIncrease => $summarizer->buffValues->{$targetType}->{'Attack'}
              // 0,
            defenseIncrease =>
              $summarizer->buffValues->{$targetType}->{'Defense'} // 0,
            hpIncrease   => $summarizer->buffValues->{$targetType}->{'HP'} // 0,
            buffValues   => $summarizer->buffValues,
            debuffValues => $summarizer->debuffValues,
          },
        );
      }

      return $c->render(template => 'generals/details');
    }

   # 3) No name provided; if you have an index/list view, it can also be pending
   # If you want index to wait until anything is loaded:
    my $any_loaded = 0;    # implement your own quick probe if you cache that
    if (!$any_loaded) {
      return $render_pending->('index-waits-for-first-load');
    }

    # else render your index/list
    return $c->render(template => 'generals/index');
  }

  sub singleTable ($c) {
    return if $c->check_prereqs_or_wait($c->prereqs);

    my $distDir = Mojo::File::Share::dist_dir('Game::EvonyTKR');

    my $slug_ui   = $c->stash('uiTarget');          # from captured route
    my $slug_buff = $c->stash('buffActivation');    # from captured route

    # Lookup full route metadata
    my $routing    = $c->get_general_routing();
    my $route_meta = $routing->lookup_route($slug_ui, $slug_buff);

    unless ($route_meta) {
      $c->log_error("Invalid route combo: $slug_ui / $slug_buff");
      return $c->reply->not_found;
    }

    # Extract validated route metadata
    my $generalType    = $route_meta->{generalType};
    my $buffActivation = $route_meta->{buffActivation};
    my $uiTarget       = $route_meta->{uiTarget};

    # Get query parameters with defaults
    my $covenantLevel  = $c->param('covenantLevel')  // 'civilization';
    my $ascendingLevel = $c->param('ascendingLevel') // 'red5';
    my @specialties    = map { $c->param("specialty$_") // 'gold' } (1 .. 4);

    # Validate parameters
    my $data_model = Game::EvonyTKR::Model::Data->new;

    if (!$data_model->checkCovenantLevel($covenantLevel)) {
      $c->log_warn(
        "Invalid covenantLevel: $covenantLevel, using default 'civilization'");
      $covenantLevel = 'civilization';
    }

    if (!$data_model->checkAscendingLevel($ascendingLevel)) {
      $c->log_warn(
        "Invalid ascendingLevel: $ascendingLevel, using default 'red5'");
      $ascendingLevel = 'red5';
    }

    @specialties = $data_model->normalizeSpecialtyLevels(@specialties);

    # Stash data for template rendering
    $c->stash(
      template         => 'generals/GeneralTableSingle',
      mode             => 'single',
      generalType      => $generalType,
      buffActivation   => $buffActivation,
      uiTarget         => $uiTarget,
      PrimaryFormTitle => $generalType =~ /Mayor/i ? 'Mayor' : 'General',
    );

    my $markdown_path =
      $distDir->child("pages/Generals/$uiTarget/comparison.md");

    if (-f $markdown_path) {
      $c->log_debug("Rendering from markdown index file");
      return $c->render_markdown_page($markdown_path);
    }
    else {
      $c->log_debug("Rendering without markdown file");
      return $c->render;
    }
  }

  sub singleCatalog ($self) {
    my $distDir            = Mojo::File::Share::dist_dir('Game::EvonyTKR');
    my $slug_ui            = $self->stash('uiTarget');
    my $slug_buff          = $self->stash('buffActivation');
    my $requested_generals = [];

    if ($self->req->method eq 'POST') {
      my $json_data = $self->req->json;
      $requested_generals = $json_data->{generals} // [];
    }

    my $uidseed = join(', ', @$requested_generals) . ' ' . UUID::uuid7();
    $self->log_debug("uidseed is '$uidseed'");

    my $session_id = UUID::uuid5($self->UUID5_base, $uidseed);
    $self->log_debug("final session_id is '$session_id'");

    # Lookup route metadata
    my $routing    = $self->get_general_routing();
    my $route_meta = $routing->lookup_route($slug_ui, $slug_buff);

    unless ($route_meta) {
      $self->log_error("Invalid route combo: $slug_ui / $slug_buff");
      if ($self->app->mode eq 'development') {
        $self->log_debug("Known valid routes:");
        $routing->each_valid_route(
          sub ($key, $meta) {
            $self->log_debug("  $key => " . Data::Printer::np($meta),
              multiline => 0);
          }
        );
      }

      return $self->reply->not_found;
    }

    # Extract metadata
    my $generalType    = $route_meta->{generalType};
    my $buffActivation = $route_meta->{buffActivation};
    my $uiTarget       = $route_meta->{uiTarget};

    my @selected = $self->get_generals_by_type($generalType)->@*;

    # Return just the basic name information without computing buffs
    my @names = map { { primary => $_->name } } @selected;

    @names = sort { $a->{primary} cmp $b->{primary} } @names;

    # if there were requested primaries, filter to only include those
    if (scalar @$requested_generals) {

      my %requested = map { $_ => 1 } @$requested_generals;
      my @filtered;
      foreach my $entry (@names) {
        if (exists $requested{$entry}) {
          $self->log_debug(sprintf(
            '%s was requsted for session %s', $entry, $session_id));
          push @filtered, $entry;
        }
      }

      $session_store->{$session_id} = \@filtered;

      return $self->render(
        json => {
          sessionId => $session_id,
          selected  => \@filtered,
        }
      );
    }
    else {
      $self->log_debug(
        "no requested primaries for session '$session_id' returning full list: "
          . Data::Printer::np(@names));
      $session_store->{$session_id} = \@names;

      return $self->render(
        json => {
          sessionId => $session_id,
          selected  => \@names,
        }
      );
    }
  }

  sub single_details_stream ($c) {
    $c->res->headers->content_type('text/event-stream');
    $c->res->headers->content_encoding('utf-8');
    $c->res->headers->add('Cache-Control', 'no-cache');

    my $slug_ui    = $c->stash('uiTarget');
    my $slug_buff  = $c->stash('buffActivation');
    my $run_id     = 0+ $c->param('runId');
    my $session_id = $c->param('sessionId');
    unless (defined($session_id) && length($session_id)) {
      $c->log_error('Session ID must be present!');
      my $payload = encode_base64(encode_json({ runId => 0+ $run_id }), '');
      $c->write_sse({ type => 'complete', text => $payload });
      return;
    }
    my $selected =
      exists $session_store->{$session_id} ? $session_store->{$session_id} : [];

    $c->log_debug(sprintf(
      'single_details_stream called url: %s,'
        . ' uiTarget: %s; buffActivation: %s; run_id: %s',
      $c->req->url->path->to_string,
      $slug_ui, $slug_buff, 0+ $run_id
    ));

    $c->log_debug(sprintf(
      'session info: sessionId: "%s"; selected: %s',
      $session_id // 'Not Present',
      join ', ',
      map { $_->{primary} } @$selected
    ));

    # Lookup route metadata
    my $routing    = Game::EvonyTKR::Control::Generals::Routing->new;
    my $route_meta = $routing->lookup_route($slug_ui, $slug_buff);

    unless ($route_meta) {
      $c->log_error("Invalid single route: $slug_ui | $slug_buff");

      if ($c->app->mode eq 'development') {
        $c->log_debug("Known valid routes:");
        $routing->each_valid_route(
          sub ($key, $meta) {
            $c->log_debug("  $key => " . Data::Printer::np($meta));
          }
        );
      }
      my $payload = encode_base64(encode_json({ runId => 0+ $run_id }), '');
      $c->write_sse({ type => 'complete', text => $payload });
      return;
    }

    $c->render_later;
    $c->write_sse;
    $c->inactivity_timeout(1200);

    my $generalType    = $route_meta->{generalType};
    my $buffActivation = $route_meta->{buffActivation};
    my $uiTarget       = $route_meta->{uiTarget};

    my $validated_params = $c->validateSingleParams();
    $validated_params->{buffActivation} = $buffActivation;
    $validated_params->{route_meta}     = $route_meta;

    my @generals;

    if (@$selected) {
      # Client provided a specific list - fetch just those generals
      for my $entry (@$selected) {
        my $name = $entry->{primary};
        next unless defined($name) && length($name);

        my $general = $c->get_general($name);
        unless ($general) {
          $c->log_warn(
            "Could not find general '$name' from selection, skipping");
          next;
        }

        # Validate the general matches the requested type
        unless (any { $_ eq $generalType } $general->type->@*) {
          $c->log_warn(
            "General '$name' is not of type '$generalType', skipping");
          next;
        }

        push @generals, $general;
      }
    }
    else {
      # No selection - get all generals of this type
      @generals = $c->get_generals_by_type($generalType)->@*;
    }

    # Sort by name
    @generals = sort { $a->name cmp $b->name } @generals;

    my @job_ids;
    for my $index (0 .. $#generals) {
      my $general = $generals[$index];

      my $jid = $c->app->minion->enqueue(
        summarize_general => [
          $general->name,
          1,    # isPrimary
          $generalType,
          $validated_params->{buffActivation},
          $validated_params->{ascendingLevel},
          $validated_params->{covenantLevel},
          $validated_params->{specialties}->[0],
          $validated_params->{specialties}->[1],
          $validated_params->{specialties}->[2],
          $validated_params->{specialties}->[3],
          undef,    # books - will be computed
        ] => {
          delay    => ($index * 0.001) + rand(0.5),
          priority => 80,
          attempts => 1,
        }
      );

      $c->log_debug("Enqueued job $jid for $general->name");
      push @job_ids, $jid;
    }

    my @promises;
    foreach my $jid (@job_ids) {
      my $promise = $c->app->minion->result_p($jid)->then(sub {
        return if !$c->tx || $c->tx->is_finished;
        my $info   = shift;
        my $result = $info->{result};

        if (defined($result) && ref($result) eq 'HASH') {
          my $general = $c->get_general($result->{general});
          unless ($general) {
            $c->log_error(sprintf('unable to get general from result: %s',
              Data::Printer::np($result)));
            next;
          }
          my $buffKey = $generalType =~ s/_/ /r;
          $buffKey =~ s/(\w)(\w+) specialist/\U$1\L$2 \UT\Lroops/;
          $buffKey =~ s/Siege Troops/Siege Machines/;

          my $row = {
            primary            => $general->to_hash,
            attackbuff         => $result->{buffs}->{$buffKey}{'Attack'},
            defensebuff        => $result->{buffs}->{$buffKey}{'Defense'},
            hpbuff             => $result->{buffs}->{$buffKey}{'HP'},
            marchbuff          => $result->{buffs}->{$buffKey}{'March Size'},
            groundattackdebuff =>
              $result->{debuffs}->{'Ground Troops'}{'Attack'},
            grounddefensedebuff =>
              $result->{debuffs}->{'Ground Troops'}{'Defense'},
            groundhpdebuff      => $result->{debuffs}->{'Ground Troops'}{'HP'},
            mountedattackdebuff =>
              $result->{debuffs}->{'Mounted Troops'}{'Attack'},
            mounteddefensedebuff =>
              $result->{debuffs}->{'Mounted Troops'}{'Defense'},
            mountedhpdebuff    => $result->{debuffs}->{'Mounted Troops'}{'HP'},
            rangedattackdebuff =>
              $result->{debuffs}->{'Ranged Troops'}{'Attack'},
            rangeddefensedebuff =>
              $result->{debuffs}->{'Ranged Troops'}{'Defense'},
            rangedhpdebuff    => $result->{debuffs}->{'Ranged Troops'}{'HP'},
            siegeattackdebuff =>
              $result->{debuffs}->{'Siege Machines'}{'Attack'},
            siegedefensedebuff =>
              $result->{debuffs}->{'Siege Machines'}{'Defense'},
            siegehpdebuff => $result->{debuffs}->{'Siege Machines'}{'HP'},
          };

          my $payload =
            encode_base64(encode_json({ runId => $run_id, data => $row }), '');
          $c->write_sse({ type => 'row', text => $payload });
        }
        return $result;
      })->catch(sub {
        my $err = shift;
        $c->log_error("Job $jid failed: " . Data::Printer::np($err));
        return undef;
      });

      push @promises, $promise;
    }

    Mojo::Promise->all(@promises)->then(sub {
      $c->log_debug("All jobs complete, sending complete event");
      return if !$c->tx || $c->tx->is_finished;

      Mojo::IOLoop->timer(
        10 => sub {
          my $payload = encode_base64(encode_json({ runId => $run_id }), '');
          $c->write_sse({ type => 'complete', text => $payload });
        }
      );
    })->catch(sub {
      $c->log_error("Some jobs failed in batch");
      return undef;
    });

    $c->on(
      finish => sub {
        $c->log_debug(
          "Client disconnected, canceling " . scalar(@job_ids) . " jobs");
        foreach my $jid (@job_ids) {
          my $job = $c->app->minion->job($jid);
          if ($job) {
            my $info = $job->info;
            next unless $info;
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
        }
        if (exists $session_store->{$session_id}) {
          delete $session_store->{$session_id};
        }
      }
    );
  }

  sub validateSingleParams($c) {
    my $data_model = Game::EvonyTKR::Model::Data->new();

    my $isPrimary      = $c->param('isPrimary')      // 1;
    my $ascendingLevel = $c->param('ascendingLevel') // 'red5';
    my $covenantLevel  = $c->param('covenantLevel')  // 'civilization';
    my @specialties;
    push @specialties, $c->param('specialty1') // 'gold';
    push @specialties, $c->param('specialty2') // 'gold';
    push @specialties, $c->param('specialty3') // 'gold';
    push @specialties, $c->param('specialty4') // 'gold';

    if ($isPrimary) {
      if (!$data_model->checkAscendingLevel($ascendingLevel)) {
        $c->log_warn(
          "Invalid ascendingLevel: $ascendingLevel, using default 'red5'");
        $ascendingLevel = 'red5';
      }
    }
    else {
      $ascendingLevel = 'none';
    }

    if (!$data_model->checkCovenantLevel($covenantLevel)) {
      $c->log_warn(
        sprintf('Invalid covenantLevel: %s, using default "civilization"',
          $covenantLevel)
      );
      $covenantLevel = 'civilization';
    }

    @specialties = $data_model->normalizeSpecialtyLevels(@specialties);

    return {
      isPrimary      => $isPrimary,
      ascendingLevel => $ascendingLevel,
      covenantLevel  => $covenantLevel,
      specialties    => \@specialties,
    };
  }

}

1;

# Add these helper methods to the Generals controller:
