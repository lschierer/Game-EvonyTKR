use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require JSON::PP;
require YAML::PP;
require Mojo::Promise;
require List::Util;
require Game::EvonyTKR::Model::General;
require Game::EvonyTKR::Model::General::Manager;
require Game::EvonyTKR::Model::General::Pair;
require Game::EvonyTKR::Model::General::Pair::Manager;
require Game::EvonyTKR::Model::Buff::Summarizer;
require Game::EvonyTKR::Control::Generals::Routing;
require Game::EvonyTKR::Model::Data;

require UUID;
require Data::Printer;
use namespace::clean;

package Game::EvonyTKR::Controller::Generals {
  use Mojo::Base 'Game::EvonyTKR::Controller::ControllerBase';
  require Mojo::Util;
  use Mojo::IOLoop;
  use Mojo::JSON     qw(to_json encode_json);
  use MIME::Base64   qw(encode_base64);
  use List::AllUtils qw( all any none );

  use IPC::Open3;
  use Symbol 'gensym';
  use Carp;

  my $logger;

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

  sub get_manager ($self) {
    return $self->app->get_root_manager->generalManager;
  }

  my $promise_chain;

  sub register($self, $app, $config = {}) {
    $logger = Log::Log4perl->get_logger(__PACKAGE__);
    $logger->info("Registering routes for " . ref($self));
    $self->SUPER::register($app, $config);

    $app->helper(
      get_general_manager => sub {
        return $app->get_root_manager->generalManager;
      }
    );

    $app->helper(
      general_routing => sub {
        state $routing = Game::EvonyTKR::Control::Generals::Routing->new(
          debug => $app->mode eq 'development',);
        return $routing;
      }
    );
    $app->plugins->emit(
      general_routing_available => { routing => $app->general_routing });

    my $controller_name = $self->controller_name();

    $logger->debug("got controller_name $controller_name.");

    my $mainRoutes      = $app->routes->any($base);
    my $referenceRoutes = $app->routes->any($reference_base);

    $mainRoutes->get('/')
      ->to(controller => $controller_name, action => 'index')
      ->name("${base}_index");

    $referenceRoutes->get('/')
      ->to(controller => $controller_name, action => 'index');

    # Add a parent navigation item for General Details under Reference
    $app->add_navigation_item({
      title  => 'General Details',
      path   => $reference_base,
      parent => '/Reference',
      order  => 10,
    });

    my $manager = $app->get_root_manager;

    my $gm = $manager->generalManager;
    if (not defined $gm) {
      $logger->logcroak('No general manager in manager');
    }

    $app->plugins->on(
      worker_started => sub {
        my $cd = Mojo::File->new($app->config('distDir'))
          ->child('collections/data/generals/');
        my @files = $cd->list_tree->each;

        foreach my $generalFile (@files) {
          my $delay = rand(4.0);
          Mojo::IOLoop->timer(
            $delay => sub {
              my $promise = Mojo::Promise->new(sub {
                unless ($manager) {
                  $logger->error(
                    "manager is not defined when processing $generalFile");
                  return;
                }
                unless ($app) {
                  $logger->error(
                    "app is not defined when processing $generalFile");
                  return;
                }
                $logger->debug("processing $generalFile");
                return $self->_import_general($generalFile, $app, $manager);
              });
            }
          );
        }
      }
    );

    $app->plugins->on(
      general_loaded => sub {
        my ($plugin, $data) = @_;
        my $manager = $app->get_root_manager();
        my $gm      = $data->{manager};
        my $general = $data->{general};
        $self->_build_general_routes($general, $app, $controller_name,
          $referenceRoutes);
      }
    );


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
        is_valid_uiTarget => sub ($route, $c, $captures, $arg) {
          my $ui = $captures->{uiTarget};
          # make this deterministic: compare exact left side of key
          my $slug = $c->general_routing->_slugify($ui);
          my $ok   = 0;
          for my $key (keys $c->general_routing->validRoutes->%*) {
            my ($left) = split /\|/, $key, 2;
            if ($left eq $slug) { $ok = 1; last }
          }
          return $ok;
        }
      );

      # check that :uiTarget/:buffActivation is a valid route combination
      $app->routes->add_condition(
        is_valid_buffActivation => sub ($route, $c, $captures, $arg) {
          my ($ui, $buff) = @$captures{qw(uiTarget buffActivation)};
          my $ok = $c->general_routing->has_route($ui, $buff) ? 1 : 0;
          $logger->debug("check ui='$ui' buff='$buff' -> $ok");
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

      1;
    };

    # nav items for the routes we just builtin the eval block
    foreach my $route ($app->general_routing->all_valid_routes()) {
      $logger->debug("building nav items for "
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

  # when this gets called,
  # $something is some sort of anon function that has access to random things.
  sub _import_general($something, $generalFile, $app, $manager) {
    my $interval = 0.01 + rand(0.04);
    $logger->debug(
      "_import_general called for $generalFile, interval is $interval");
    Mojo::IOLoop->timer(
      $interval => sub {
        $logger->debug("timer fired for $generalFile");
        eval {
          $logger->info("importing file $generalFile");
          my $data = $generalFile->slurp('UTF-8');
          my $ho   = YAML::PP->new(
            schema       => [qw/ + Perl /],
            yaml_version => ['1.2', '1.1'],
          )->load_string($data);
          my $g = Game::EvonyTKR::Model::General->from_hash($ho, $logger);
          if (!$manager) {
            $logger->error(
              "manager is not defined in _import_general for $generalFile");
            return;
          }
          my $gm = $manager->generalManager;
          $gm->add_general($g);
          $logger->debug(
            sprintf('imported general %s from file %s', $g->name, $generalFile)
          );

          # Build routes for this general

          my $bm = $manager->bookManager();
          $g->populateBuiltInBook($bm);
          $app->plugins->emit(
            general_loaded => { manager => $gm, general => $g });
        };
        if ($@) {
          $logger->error("Error processing $generalFile: $@");
        }
      }
    );
  }

  sub _build_general_routes($self, $general, $app, $controller_name,
    $referenceRoutes) {
    my $logger = Log::Log4perl->get_logger(__PACKAGE__);
    my $name   = $general->name;

    $logger->debug("building Reference Routes for $name");

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

  sub index($self) {
    my $collection = collection_name();
    $logger->debug("Rendering index for $collection");

    my $rp = $self->req->url->path->to_string;
    # Remove trailing slash from pages
    if ($rp =~ qr{/$}) {
      my $canonical = $rp;
      $canonical =~ s{/$}{};
      return $self->redirect_to($canonical, 301);
    }

    # Check if markdown exists for this collection
    my $distDir       = Mojo::File::Share::dist_dir('Game::EvonyTKR');
    my $markdown_path = $distDir->child("pages/Generals/index.md");

    my @parts     = split(/::/, ref($self));
    my $baseClass = pop(@parts);
    my $base      = $self->getBase();
    $logger->debug("Generals index method has base $base");

    my $items = $self->get_general_manager()->get_all_generals();
    $logger->debug(
      sprintf('Items: %s with %s keys.', ref($items), scalar(keys %$items)));
    $self->stash(
      linkBase        => $base,
      items           => $items,
      collection_name => $collection,
      controller_name => $baseClass,
    );

    if (-f $markdown_path) {
      $logger->debug(
        "rendering /Generals/ with markdown index content from $markdown_path");
      # Render with markdown
      $self->stash(template => '/generals/index');

      return $self->render_markdown_file($markdown_path,
        { template => 'generals/index' });
    }
    else {
      $logger->debug("no markdown index content found at $markdown_path");
      # Render just the items
      return $self->render(template => '/generals/index');
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
    $logger->debug("found valid_routes "
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
    $logger->debug("looking for index at $markdown_path");

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

  sub show ($self) {
    $logger->debug("start of show method");
    my $name;
    $name = $self->param('name');

    my $rp = $self->req->url->path->to_string;
    # Remove trailing slash from pages
    if ($rp =~ qr{/$}) {
      my $canonical = $rp;
      $canonical =~ s{/$}{};
      return $self->redirect_to($canonical, 301);
    }

    $logger->debug("show detects name $name, showing details.");
    my $calculate_buffs = $self->param('calculate_buffs') // 0;

    my $covenantLevel  = $self->param('covenantLevel')  // 'civilization';
    my $ascendingLevel = $self->param('ascendingLevel') // 'red5';
    my @specialties;
    push @specialties, $self->param('specialty1') // 'gold';
    push @specialties, $self->param('specialty2') // 'gold';
    push @specialties, $self->param('specialty3') // 'gold';
    push @specialties, $self->param('specialty4') // 'gold';

    my $rootManager = $self->get_root_manager();
    my $data_model  = Game::EvonyTKR::Model::Data->new();

    if (none { $_ eq $covenantLevel } @{ $rootManager->CovenantCategoryValues })
    {
      $logger->warn(
        "Invalid covenantLevel: $covenantLevel, using default 'civilization'");
      $covenantLevel = 'civilization';
    }

    # Validate ascending level
    if (none { $_ eq $ascendingLevel }
      $rootManager->AscendingAttributeLevelValues()) {
      $logger->warn(
        "Invalid ascendingLevel: $ascendingLevel, using default 'red5'");
      $ascendingLevel = 'red5';
    }

    @specialties = $data_model->normalizeSpecialtyLevels(@specialties);

    my $general =
      $self->app->get_root_manager->generalManager->getGeneral($name);
    $logger->debug("got general of type " . blessed $general);

    if ($general) {
      $self->stash(item => $general);

      if ($calculate_buffs) {
        $logger->debug(
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

        $logger->debug("Using $targetType as targetType for $name");
        my $summarizer = Game::EvonyTKR::Model::Buff::Summarizer->new(
          general => $general,
          books => $self->app->get_root_manager()->bookManager->get_all_books(),
          covenant => $self->app->get_root_manager()
            ->covenantManager->getCovenant($general->name),
          ascendingAttributes => $self->app->get_root_manager()
            ->ascendingAttributesManager->getAscendingAttributes(
            $general->name
            ),
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

     # Stash the full buff and debuff hashes for granular access in the template
        $self->stash(
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

      return $self->render(template => 'generals/details');
    }
    $self->SUPER::show();
  }

  sub singleTable ($self) {
    my $distDir = Mojo::File::Share::dist_dir('Game::EvonyTKR');

    my $slug_ui   = $self->stash('uiTarget');          # from captured route
    my $slug_buff = $self->stash('buffActivation');    # from captured route

    # Lookup full route metadata
    my $routing    = $self->general_routing;
    my $route_meta = $routing->lookup_route($slug_ui, $slug_buff);

    unless ($route_meta) {
      $logger->error("Invalid route combo: $slug_ui / $slug_buff");
      return $self->reply->not_found;
    }

    # Extract validated route metadata
    my $generalType    = $route_meta->{generalType};
    my $buffActivation = $route_meta->{buffActivation};
    my $uiTarget       = $route_meta->{uiTarget};

    # Get query parameters with defaults
    my $covenantLevel  = $self->param('covenantLevel')  // 'civilization';
    my $ascendingLevel = $self->param('ascendingLevel') // 'red5';
    my @specialties    = map { $self->param("specialty$_") // 'gold' } (1 .. 4);

    # Validate parameters
    my $data_model = Game::EvonyTKR::Model::Data->new;

    if (!$data_model->checkCovenantLevel($covenantLevel)) {
      $logger->warn(
        "Invalid covenantLevel: $covenantLevel, using default 'civilization'");
      $covenantLevel = 'civilization';
    }

    if (!$data_model->checkAscendingLevel($ascendingLevel)) {
      $logger->warn(
        "Invalid ascendingLevel: $ascendingLevel, using default 'red5'");
      $ascendingLevel = 'red5';
    }

    @specialties = $data_model->normalizeSpecialtyLevels(@specialties);

    # Stash data for template rendering
    $self->stash(
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
      $logger->debug("Rendering from markdown index file");
      return $self->render_markdown_file($markdown_path);
    }
    else {
      $logger->debug("Rendering without markdown file");
      return $self->render;
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
    $logger->debug("uidseed is '$uidseed'");

    my $session_id =
      UUID::uuid5($self->app->get_root_manager()->UUID5_base, $uidseed);
    $logger->debug("final session_id is '$session_id'");

    # Lookup route metadata
    my $routing    = $self->general_routing;
    my $route_meta = $routing->lookup_route($slug_ui, $slug_buff);

    unless ($route_meta) {
      $logger->error("Invalid route combo: $slug_ui / $slug_buff");
      if ($self->app->mode eq 'development') {
        $logger->debug("Known valid routes:");
        $routing->each_valid_route(
          sub ($key, $meta) {
            $logger->debug("  $key => " . Data::Printer::np($meta),
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

    my $gm = $self->app->get_general_manager();
    my @selected;
    while (my ($key, $general) = each(%{ $gm->get_all_generals() })) {
      $logger->debug(
        "inspecting '$key', first need to see if it is a $generalType."
          . Data::Printer::np($general, multiline => 0));
      if (none { lc($_) eq $generalType } @{ $general->type }) {
        $logger->debug("none of "
            . $general->name
            . "'s types: "
            . Data::Printer::np($general->type, multiline => 0)
            . "match as a $generalType.");
        next;
      }
      push @selected, $general;
    }

    $logger->debug(
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
          $logger->debug(sprintf(
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
      $logger->debug(
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
      $logger->error('Session ID must be present!');
      my $payload = encode_json({ runId => 0+ $run_id });
      $c->write_sse({ type => 'complete', text => $payload });
      return;
    }
    my $selected =
      exists $session_store->{$session_id} ? $session_store->{$session_id} : [];

    $logger->debug(sprintf(
      'stream_single_details called url: %s,'
        . ' uiTarget: %s; buffActivation: %s; run_id: %s',
      $c->req->url->path->to_string,
      $slug_ui, $slug_buff, 0+ $run_id
    ));

    $logger->debug(sprintf(
      'session info: sessionId: "%s"; selected: %s',
      $session_id // 'Not Present',
      join ', ',
      map { $_->{primary} } @$selected
    ));

    # Lookup route metadata
    my $routing    = Game::EvonyTKR::Control::Generals::Routing->new;
    my $route_meta = $routing->lookup_route($slug_ui, $slug_buff);

    unless ($route_meta) {
      $logger->error("Invalid single route: $slug_ui | $slug_buff");

      if ($c->app->mode eq 'development') {
        $logger->debug("Known valid routes:");
        $routing->each_valid_route(
          sub ($key, $meta) {
            $logger->debug("  $key => " . Data::Printer::np($meta));
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
    my $gm             = $c->app->get_general_manager();
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
      values $gm->get_all_generals()->%*) {

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
        $logger->debug("processing $start to $end");

        my $subprocess = Mojo::IOLoop::Subprocess->new;
        $subprocess->on(
          progress => sub ($subprocess, @data) {
            my ($result) = @data;
            if (!$c->tx || $c->tx->is_finished) {
              $logger->info(
                "transaction finished before write_sse called for $result");
              return;
            }
            $logger->debug("progress event detected");
            $c->write_sse({ type => 'row', text => $result });
          }
        );

        return $subprocess->run_p(sub {
          $logger->debug("sub process for index $start to $end");
          for my $i ($start .. $end) {
            my $general = $rows->[$i];
            $logger->debug(sprintf('processing general %s', $general->name,));
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
            $logger->debug("buffKey is $buffKey");

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
            $logger->debug(sprintf(
              'row is %s, json is %s',
              Data::Printer::np($row, multiline => 0), $payload,
            ));
            my $result = encode_base64($payload);
            $subprocess->progress($result);
          }

        })->catch(sub {
          my $err = shift;
          $logger->error(sprintf(
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
      $logger->error('Overall map operation failed');
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
        $logger->warn(
          "Invalid ascendingLevel: $ascendingLevel, using default 'red5'");
        $ascendingLevel = 'red5';
      }
    }
    else {
      $ascendingLevel = 'none';
    }

    if (!$data_model->checkCovenantLevel($covenantLevel)) {
      $logger->warn(
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
