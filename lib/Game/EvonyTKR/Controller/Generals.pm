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
require Game::EvonyTKR::Model::General::Pair::Manager;
require Game::EvonyTKR::Model::Buff::Summarizer;
require Game::EvonyTKR::Control::Generals::Routing;
require Game::EvonyTKR::Model::Data;
require Game::EvonyTKR::Service::Cache;

require UUID;
require Data::Printer;
use namespace::clean;

package Game::EvonyTKR::Controller::Generals {
  use Mojo::Base 'Game::EvonyTKR::Controller::ControllerBase';
  use Mojo::Base 'Game::EvonyTKR::Role::Logger', -role, -signatures;
  use Mojo::Base 'Game::EvonyTKR::Controller::Role::Generals', -role,
    -signatures;
  use Mojo::IOLoop;
  use Mojo::Promise;
  use Mojo::JSON     qw(to_json encode_json);
  use MIME::Base64   qw(encode_base64);
  use List::AllUtils qw( all any none );

  use Carp;

  # Specify which collection this controller handles
  sub collection_name {'generals'}

  sub controller_name ($self) {
    return "Generals";
  }

  my $base = '/Generals';

  my $reference_base = '/Reference/Generals';

  my $session_store = {};

  my $max_concurrency = 15;

  sub getBase($self) {
    return $base;
  }
  state $cache;

  sub register($c, $app, $config = {}) {
    $c->logger->info("Registering routes for " . ref($c));
    $c->SUPER::register($app, $config);

    $cache = Game::EvonyTKR::Service::Cache->new();

    eval {
      say "calling setup_event_handlers";
      $c->setup_event_handlers($app);
      1;
    } or do {
      say "Error in setup_event_handlers: $@";
    };
    eval {
      say 'calling setup_helpers';
      $c->setup_helpers($app);
      1;
    } or do {
      say "Error in setup_helpers: $@";
    };
    say 'calling setup_routes';
    $c->setup_routes($app);

    say sprintf('%s register complete', __PACKAGE__);
  }

  sub setup_generals($c, $app) {
    my $loop;
    state $receivedGenerals = 0;
    $loop = Mojo::IOLoop->recurring(
      10 => sub {
        my $generalCount = $cache->get('generalCount') // -1;
        $c->logger->debug(sprintf(
          'setup_generals: receivedGenerals: %s; generalCount: %s',
          $receivedGenerals, $generalCount
        ));
        if ($generalCount > 0 && $receivedGenerals < $generalCount) {
          my $generals = $c->get_generals();
          if (defined($generals) && ref($generals) eq 'HASH') {
            foreach my $general (values $generals->%*) {
              # emit a signal to break out of this loop and stay async.
              $app->plugins->emit(general_loaded => { general => $general });
              $receivedGenerals++;
            }
          }
        }
        elsif ($generalCount > 0) {
          Mojo::IOLoop->remove($loop);
        }
      }
    );
    Mojo::IOLoop->start unless Mojo::IOLoop->is_running;
  }

  sub setup_helpers($c, $app) {

    $app->helper(
      general_routing => sub {
        state $routing = Game::EvonyTKR::Control::Generals::Routing->new(
          debug => $app->mode eq 'development',);
        return $routing;
      }
    );

    say "general_routing_available emitting";
    $app->plugins->emit(
      general_routing_available => { routing => $app->general_routing });
  }

  sub setup_event_handlers($c, $app) {

    #first define variables and state
    my $completion_state = {
      skill_books_loaded          => 0,
      specialties_imported        => 0,
      ascending_attributes_loaded => 0,
      general_loader_job_ready    => 0,
    };

    my $check_prerequisites = sub {
      if (List::AllUtils::none { $_ == 0 } values $completion_state->%*) {
        $c->logger->info(sprintf('%s Ready to Get Generals', __PACKAGE__));

      }
      else {
        $c->logger->info(sprintf(
          '%s not Ready to Get Generals yet: %s',
          __PACKAGE__, Data::Printer::np($completion_state, multiline => 0)
        ));
      }
    };

    # then define handlers in reverse order of use for safety

    $app->plugins->on(
      general_loader_job_ready => sub {
        $completion_state->{general_loader_job_ready} = 1;
        $check_prerequisites->();
      }
    );

    $app->plugins->on(
      general_loaded => sub {
        my ($plugin, $data) = @_;
        my $general = $data->{'general'};
        $c->logger->info(sprintf(
          '%s detected %s loaded. Building Routes.',
          __PACKAGE__, $general->name
        ));
        $c->_build_general_routes($general, $app);
      }
    );

    # last define things that will trigger them.
    # which might be more handlers, see above about
    # reverse order of use.

    $app->plugins->on(
      ascending_attributes_imported => sub {
        $completion_state->{ascending_attributes_loaded} = 1;
        $check_prerequisites->();
      }
    );

    $app->plugins->on(
      all_books_loaded => sub {
        $completion_state->{skill_books_loaded} = 1;
        $check_prerequisites->();
      }
    );

    $app->plugins->on(
      specialties_imported => sub {
        $completion_state->{specialties_imported} = 1;
        $check_prerequisites->();
      }
    );

    $app->plugins->on(
      general_routing_available => sub {
        say 'general_routing_available signal recieved';
        $c->logger->debug('general_routing_available signal recieved');
        my $delay = 2;
        Mojo::IOLoop->timer(
          $delay => sub {
            say "$delay second timer complete";
            eval {
              $c->setup_generals($app);
              1;
            } or do {
              say "problem in setup_generals";
            }
          }
        );
      }
    );

    $c->logger->debug(sprintf('all handlers registered for %s', blessed($c)));
  }

  sub setup_routes($c, $app) {
    my $controller_name = $c->controller_name();
    $c->logger->debug("got controller_name $controller_name.");

    my $mainRoutes      = $app->routes->any($base);
    my $referenceRoutes = $app->routes->any($reference_base);

    $mainRoutes->get('/')
      ->to(controller => $controller_name, action => 'index')
      ->name("${base}_index");

    $referenceRoutes->get('/')
      ->to(controller => $controller_name, action => 'index');

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
          my $slug = $app->general_routing->_slugify($ui);
          my $ok   = 0;
          for my $key (keys $app->general_routing->validRoutes->%*) {
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
          my $ok = $app->general_routing->has_route($ui, $buff) ? 1 : 0;
          $c->logger->debug("check ui='$ui' buff='$buff' -> $ok");
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
        action     => 'stream_single_details',
        )->name('Generals_dynamic_singleDetails');
    };
    if ($@) {
      my $error =
        sprintf('error building dynamic routes for Generals Controller: %s',
        $@);
      $c->logger->error($error);
      if ($app->mode eq 'development') {
        croak($error);
      }
    }
    $c->setup_navigation($app);
  }

  sub setup_navigation($c, $app) {
    $app->add_navigation_item({
      title  => 'General Details',
      path   => $reference_base,
      parent => '/Reference',
      order  => 10,
    });

    # nav items for the dynamic routes
    foreach my $route ($app->general_routing->all_valid_routes()) {
      $c->logger->debug("building nav items for "
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

  sub _build_general_routes($self, $general, $app,) {
    my $name = $general->name;

    $self->logger->debug("building Reference Routes for $name");
    my $referenceRoutes = $app->routes->any($reference_base);

    my $gr  = "/Reference/Generals/$name";
    my $grn = "${name}ReferenceRoute";
    $grn =~ s/ /_/g;

    $referenceRoutes->get("/$name" => { name => $name })
      ->to(controller => 'Generals', action => 'show')
      ->name($grn);

    $app->add_navigation_item({
      title  => "Details for $name",
      path   => $gr,
      parent => '/Reference/Generals',
      order  => 20,
    });
  }

  sub index($c) {
    my $collection = collection_name();
    $c->logger->debug("Rendering index for $collection");

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
    $c->logger->debug("Generals index method has base $base");

    my $items = $c->get_generals() // {};
    $c->logger->debug(
      sprintf('Items: %s with %s keys.', ref($items), scalar(keys %$items)));
    $c->stash(
      linkBase        => $base,
      items           => $items,
      collection_name => $collection,
      controller_name => $baseClass,
    );

    if (-f $markdown_path) {
      $c->logger->debug(
        "rendering /Generals/ with markdown index content from $markdown_path");
      # Render with markdown
      $c->stash(template => '/generals/index');

      return $c->render_markdown_file($markdown_path,
        { template => 'generals/index' });
    }
    else {
      $c->logger->debug("no markdown index content found at $markdown_path");
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
      $self->general_routing->get_routes_for_uiTarget($uiTarget);
    $self->logger->debug("found valid_routes "
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
    $self->logger->debug("looking for index at $markdown_path");

    if (-f $markdown_path) {
      # Render with markdown
      $self->stash(template => "generals/uiTarget/index");
      return $self->render_markdown_file($markdown_path,
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
      $self->general_routing->lookup_route($uiTarget, $buffActivation);

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
      return $self->render_markdown_file($markdown_path,
        { template => "generals/uiTarget/buffActivation/index_with_file" });
    }
    else {
      # Render just the dynamic content
      return $self->render(
        template => 'generals/uiTarget/buffActivation/index');
    }
  }

  sub show ($c) {
    $c->logger->debug("start of show method");
    unless ($c) {
      $c->logger->error('controller must be defined for show method to work');
      return;
    }
    unless ($c->app) {
      $c->logger->error('controller app attribute is undefined');
      return;
    }
    my $name;
    $name = $c->param('name');

    my $rp = $c->req->url->path->to_string;
    # Remove trailing slash from pages
    if ($rp =~ qr{/$}) {
      my $canonical = $rp;
      $canonical =~ s{/$}{};
      return $c->redirect_to($canonical, 301);
    }

    $c->logger->debug("show detects name $name, showing details.");
    my $calculate_buffs = $c->param('calculate_buffs') // 0;

    my $general = $c->get_general($name, $cache);
    unless ($general) {
      $c->logger->error("No general found for name $name in the 'show' route.");
      return $c->reply->not_found;
    }
    $c->logger->debug("got general of type " . blessed $general);

    if ($general) {
      $c->stash(item => $general);

      if ($calculate_buffs) {
        my $covenantLevel  = $c->param('covenantLevel')  // 'civilization';
        my $ascendingLevel = $c->param('ascendingLevel') // 'red5';
        my @specialties;
        push @specialties, $c->param('specialty1') // 'gold';
        push @specialties, $c->param('specialty2') // 'gold';
        push @specialties, $c->param('specialty3') // 'gold';
        push @specialties, $c->param('specialty4') // 'gold';
        my $data_model = Game::EvonyTKR::Model::Data->new();

        if (none { $_ eq $covenantLevel }
          @{ $data_model->CovenantCategoryValues }) {
          $c->logger->warn(
            sprintf('Invalid covenantLevel: %s , using default "civilization"',
              $covenantLevel)
          );
          $covenantLevel = 'civilization';
        }

        # Validate ascending level
        if (none { $_ eq $ascendingLevel }
          $data_model->AscendingAttributeLevelValues()) {
          $c->logger->warn(
            "Invalid ascendingLevel: $ascendingLevel, using default 'red5'");
          $ascendingLevel = 'red5';
        }

        @specialties = $data_model->normalizeSpecialtyLevels(@specialties);

        $c->logger->debug(
          "show method sees a request for display of calculated buff summaries."
        );
        my $targetType;
        if (ref $general->type eq 'ARRAY') {
          $targetType = $general->type->[0] if @{ $general->type };
        }
        else {
          $targetType = $general->type;
        }
        $targetType //= '';    # Default to empty string if undefined
        $targetType =~ s/_/ /;
        $targetType =~ s/(\w)(\w+) specialist/\U$1\L$2 \UT\Lroops/;
        $targetType =~ s/Siege Troops/Siege Machines/;

        $c->logger->debug("Using $targetType as targetType for $name");
        my $summarizer = Game::EvonyTKR::Model::Buff::Summarizer->new(
          general             => $general,
          books               => $c->app->get_generic_books(),
          covenant            => $c->app->getCovenant($general->name),
          ascendingAttributes => $general->ascendingAttribute,
          isPrimary           => 1,
          targetType          => $targetType,
          activationType      => 'Attacking',
          ascendingLevel      => $ascendingLevel,
          covenantLevel       => $covenantLevel,
          specialty1          => $specialties[0],
          specialty2          => $specialties[1],
          specialty3          => $specialties[2],
          specialty4          => $specialties[3],
        );

        $summarizer->updateBuffs();
        $summarizer->updateDebuffs();

        # Stash the full buff and debuff hashes
        # for granular access in the template
        $c->stash(
          'buff-summaries' => {
            # For backward compatibility
            marchIncrease =>
              $summarizer->buffValues->{$targetType}->{'March Size'} // 0,
            attackIncrease => $summarizer->buffValues->{$targetType}->{'Attack'}
              // 0,
            defenseIncrease =>
              $summarizer->buffValues->{$targetType}->{'Defense'} // 0,
            hpIncrease => $summarizer->buffValues->{$targetType}->{'HP'} // 0,

            # Full granular data
            buffValues   => $summarizer->buffValues,
            debuffValues => $summarizer->debuffValues,
          },
        );
      }

      if ($c && $c->app) {
        return $c->render(template => 'generals/details');
      }
      else {
        $c->logger->error('missing app!!');
      }
    }
    $c->SUPER::show();
  }

  sub singleTable ($c) {
    my $distDir = Mojo::File::Share::dist_dir('Game::EvonyTKR');

    my $slug_ui   = $c->stash('uiTarget');          # from captured route
    my $slug_buff = $c->stash('buffActivation');    # from captured route

    # Lookup full route metadata
    my $routing    = $c->general_routing;
    my $route_meta = $routing->lookup_route($slug_ui, $slug_buff);

    unless ($route_meta) {
      $c->logger->error("Invalid route combo: $slug_ui / $slug_buff");
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
      $c->logger->warn(
        "Invalid covenantLevel: $covenantLevel, using default 'civilization'");
      $covenantLevel = 'civilization';
    }

    if (!$data_model->checkAscendingLevel($ascendingLevel)) {
      $c->logger->warn(
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
      $c->logger->debug("Rendering from markdown index file");
      return $c->render_markdown_file($markdown_path);
    }
    else {
      $c->logger->debug("Rendering without markdown file");
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
    $self->logger->debug("uidseed is '$uidseed'");

    my $session_id =
      UUID::uuid5($self->app->get_root_manager()->UUID5_base, $uidseed);
    $self->logger->debug("final session_id is '$session_id'");

    # Lookup route metadata
    my $routing    = $self->general_routing;
    my $route_meta = $routing->lookup_route($slug_ui, $slug_buff);

    unless ($route_meta) {
      $self->logger->error("Invalid route combo: $slug_ui / $slug_buff");
      if ($self->app->mode eq 'development') {
        $self->logger->debug("Known valid routes:");
        $routing->each_valid_route(
          sub ($key, $meta) {
            $self->logger->debug("  $key => " . Data::Printer::np($meta),
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

    my @selected;
    while (my ($key, $general) =
      each(%{ $self->SUPER::get_shared_data('generals') })) {
      $self->logger->debug(
        "inspecting '$key', first need to see if it is a $generalType."
          . Data::Printer::np($general, multiline => 0));
      if (none { lc($_) eq $generalType } @{ $general->type }) {
        $self->logger->debug("none of "
            . $general->name
            . "'s types: "
            . Data::Printer::np($general->type, multiline => 0)
            . "match as a $generalType.");
        next;
      }
      push @selected, $general;
    }

    $self->logger->debug(
      sprintf('There are %s generals to return.', scalar(@selected)));

    # Return just the basic name information without computing buffs
    my @names = map { { primary => $_->name } } @selected;

    @names = sort { $a->{primary} cmp $b->{primary} } @names;

    # if there were requested primaries, filter to only include those
    if (scalar @$requested_generals) {

      my %requested = map { $_ => 1 } @$requested_generals;
      my @filtered;
      foreach my $entry (@names) {
        if (exists $requested{$entry}) {
          $self->logger->debug(sprintf(
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
      $self->logger->debug(
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

  sub stream_single_details ($c) {
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
      'stream_single_details called url: %s,'
        . ' uiTarget: %s; buffActivation: %s; run_id: %s',
      $c->req->url->path->to_string,
      $slug_ui, $slug_buff, 0+ $run_id
    ));

    $c->logger->debug(sprintf(
      'session info: sessionId: "%s"; selected: %s',
      $session_id // 'Not Present',
      join ', ',
      map { $_->{primary} } @$selected
    ));

    # Lookup route metadata
    my $routing    = Game::EvonyTKR::Control::Generals::Routing->new;
    my $route_meta = $routing->lookup_route($slug_ui, $slug_buff);

    unless ($route_meta) {
      $c->logger->error("Invalid single route: $slug_ui | $slug_buff");

      if ($c->app->mode eq 'development') {
        $c->logger->debug("Known valid routes:");
        $routing->each_valid_route(
          sub ($key, $meta) {
            $c->logger->debug("  $key => " . Data::Printer::np($meta));
          }
        );
      }
      my $payload = encode_json({ runId => 0+ $run_id });
      $c->write_sse({ type => 'complete', text => $payload });
      return;
    }

    $c->render_later;
    $c->write_sse;
    $c->inactivity_timeout(300);

    my $generalType    = $route_meta->{generalType};
    my $buffActivation = $route_meta->{buffActivation};
    my $uiTarget       = $route_meta->{uiTarget};
    my $rows;

    my $typeMap = {
      'Ground Specialists'  => 'ground_specialist',
      'Ranged Specialists'  => 'ranged_specialist',
      'Siege Specialists'   => 'siege_specialist',
      'Mounted Specialists' => 'mounted_specialist',
      'Wall Specialists'    => 'wall',
    };

    my @promises;
    my @subs;

    my $validated_params = $c->validateSingleParams();
    $validated_params->{buffActivation} = $buffActivation;
    $validated_params->{route_meta}     = $route_meta;
    $validated_params->{typeMap}        = $typeMap;

    my $valid = {};
    map { $valid->{ $_->{primary} } => 1 } @$selected;
    foreach my $general (sort { $a->name cmp $b->name }
      values $c->SUPER::get_shared_data('generals')->%*) {

      if (scalar(@$selected) && exists $valid->{ $general->name }) {
        push @$rows, $general;
      }
      elsif (any { $_ eq $generalType } $general->type->@*) {
        push @$rows, $general;
      }
    }

    my @batch_ranges;
    my $index      = 0;
    my $batch_size = 10;
    my $maxIndex   = scalar(@$rows) - 1;

    while ($index < $maxIndex) {
      my $end = List::Util::min($index + $batch_size - 1, $maxIndex);
      push @batch_ranges, [$index, $end];
      $index = $end + 1;
    }

    Mojo::Promise->map(
      { concurrency => $max_concurrency }
      ,    # This replaces your unlimited spawning
      sub {
        my ($start, $end) = @{ $_[0] };    # Current batch range
        $c->logger->debug("processing $start to $end");

        my $subprocess = Mojo::IOLoop::Subprocess->new;
        $subprocess->on(
          progress => sub ($subprocess, @data) {
            my ($result) = @data;
            if (!$c->tx || $c->tx->is_finished) {
              $c->logger->info(
                "transaction finished before write_sse called for $result");
              return;
            }
            $c->logger->debug("progress event detected");
            $c->write_sse({ type => 'row', text => $result });
          }
        );

        return $subprocess->run_p(sub {
          $c->logger->debug("sub process for index $start to $end");
          for my $i ($start .. $end) {
            my $general = $rows->[$i];
            $c->logger->debug(
              sprintf('processing general %s', $general->name,));
            my $summarizer = Game::EvonyTKR::Model::Buff::Summarizer->new(
              general => $general,
              books   =>
                $c->app->get_root_manager()->bookManager->get_all_books(),
              covenant => $c->app->get_root_manager()
                ->covenantManager->getCovenant($general->name),
              ascendingAttributes => $c->app->get_root_manager()
                ->ascendingAttributesManager->getAscendingAttributes(
                $general->name
                ),
              isPrimary      => $validated_params->{isPrimary},
              targetType     => $validated_params->{targetType},
              activationType => $validated_params->{buffActivation},
              ascendingLevel => $validated_params->{ascendingLevel},
              covenantLevel  => $validated_params->{covenantLevel},
              specialty1     => $validated_params->{specialties}->[0],
              specialty2     => $validated_params->{specialties}->[1],
              specialty3     => $validated_params->{specialties}->[2],
              specialty4     => $validated_params->{specialties}->[3],
            );
            # Do all the heavy computation here
            $summarizer->updateBuffs();
            $summarizer->updateDebuffs();

            my $buffKey =
              $validated_params->{route_meta}->{generalType} =~ s/_/ /r;
            $buffKey =~ s/(\w)(\w+) specialist/\U$1\L$2 \UT\Lroops/;
            $buffKey =~ s/Siege Troops/Siege Machines/;
            $c->logger->debug("buffKey is $buffKey");

            # build the row payload
            my $row = {
              primary     => $general->to_hash,
              attackbuff  => $summarizer->buffValues->{$buffKey}{'Attack'},
              defensebuff => $summarizer->buffValues->{$buffKey}{'Defense'},
              hpbuff      => $summarizer->buffValues->{$buffKey}{'HP'},
              marchbuff   => $summarizer->buffValues->{$buffKey}{'March Size'},
              groundattackdebuff =>
                $summarizer->debuffValues->{'Ground Troops'}{'Attack'},
              grounddefensedebuff =>
                $summarizer->debuffValues->{'Ground Troops'}{'Defense'},
              groundhpdebuff =>
                $summarizer->debuffValues->{'Ground Troops'}{'HP'},
              mountedattackdebuff =>
                $summarizer->debuffValues->{'Mounted Troops'}{'Attack'},
              mounteddefensedebuff =>
                $summarizer->debuffValues->{'Mounted Troops'}{'Defense'},
              mountedhpdebuff =>
                $summarizer->debuffValues->{'Mounted Troops'}{'HP'},
              rangedattackdebuff =>
                $summarizer->debuffValues->{'Ranged Troops'}{'Attack'},
              rangeddefensedebuff =>
                $summarizer->debuffValues->{'Ranged Troops'}{'Defense'},
              rangedhpdebuff =>
                $summarizer->debuffValues->{'Ranged Troops'}{'HP'},
              siegeattackdebuff =>
                $summarizer->debuffValues->{'Siege Machines'}{'Attack'},
              siegedefensedebuff =>
                $summarizer->debuffValues->{'Siege Machines'}{'Defense'},
              siegehpdebuff =>
                $summarizer->debuffValues->{'Siege Machines'}{'HP'},
            };

            # one JSON object per message; include runId inside the data payload
            my $json =
              JSON::PP->new->utf8(0)->allow_blessed->convert_blessed->canonical;

            my $payload = $json->encode({ runId => 0+ $run_id, data => $row });
            $c->logger->debug(sprintf(
              'row is %s, json is %s',
              Data::Printer::np($row, multiline => 0), $payload,
            ));
            my $result = encode_base64($payload);
            $subprocess->progress($result);
          }

        })->catch(sub {
          my $err = shift;
          $c->logger->error(sprintf(
            'error in promise for subloop %s to %s : "%s". ',
            $start, $end, $err ? $err : 'Unknown'
          ));
          return undef;    # Return something so map can continue
        });
        ;                  # Same as before
      },
      @batch_ranges        # Process each batch range
    )->then(sub {
      my $payload = encode_json({ runId => $run_id });
      $c->write_sse({ type => 'complete', text => $payload });
    })->catch(sub {
      $c->logger->error('Overall map operation failed');
      return undef;
    });

    # If the browser closes, remove the stored session
    $c->on(
      finish => sub {

        $_->kill('TERM') for @subs;
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
      # Validate ascending level
      if (!$data_model->checkAscendingLevel($ascendingLevel)) {
        $c->logger->warn(
          "Invalid ascendingLevel: $ascendingLevel, using default 'red5'");
        $ascendingLevel = 'red5';
      }
    }
    else {
      $ascendingLevel = 'none';
    }

    if (!$data_model->checkCovenantLevel($covenantLevel)) {
      $c->logger->warn(
        sprintf('Invalid covenantLevel: %s, using default "civilization"',
          $covenantLevel)
      );
      $covenantLevel = 'civilization';
    }

    @specialties = $data_model->normalizeSpecialtyLevels(@specialties);

    return {
      ascendingLevel => $ascendingLevel,
      covenantLevel  => $covenantLevel,
      specialties    => \@specialties,
    };
  }

}

1;

# Add these helper methods to the Generals controller:
