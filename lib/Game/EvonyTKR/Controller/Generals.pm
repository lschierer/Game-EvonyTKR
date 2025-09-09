use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require JSON::PP;;
require Game::EvonyTKR::Model::General;
require Game::EvonyTKR::Model::General::Manager;
require Game::EvonyTKR::Model::General::Pair;
require Game::EvonyTKR::Model::General::Pair::Manager;
require Game::EvonyTKR::Model::Buff::Summarizer;
require Game::EvonyTKR::Control::Generals::Routing;
require Game::EvonyTKR::Model::Data;
require Data::Printer;
use namespace::clean;

package Game::EvonyTKR::Controller::Generals {
  use Mojo::Base 'Game::EvonyTKR::Controller::ControllerBase';
  require Mojo::Util;
  use List::AllUtils qw( all any none );
  use Carp;

  # Specify which collection this controller handles
  sub collection_name {'generals'}

  sub controller_name ($self) {
    return "Generals";
  }

  my $base = '/Generals';

  my $reference_base = '/Reference/Generals';

  sub getReferenceBase($self) {
    return $reference_base;
  }

  sub getBase($self) {
    return $base;
  }

  sub get_manager ($self) {
    return $self->app->get_root_manager->generalManager;
  }

  my $logger;
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
      get_general_pair_manager => sub {
        return $app->get_root_manager->generalPairManager;
      }
    );

    $app->helper(
      general_routing => sub {
        state $routing = Game::EvonyTKR::Control::Generals::Routing->new(
          debug => $app->mode eq 'development',);
        return $routing;
      }
    );

    my $controller_name = $self->controller_name();

    $logger->debug("got controller_name $controller_name.");

    my $mainRoutes      = $app->routes->any($base);
    my $referenceRoutes = $app->routes->any($reference_base);

    $mainRoutes->get('/')
      ->to(controller => $controller_name, action => 'index')
      ->name("${base}_index");

    $referenceRoutes->get('/')
      ->to(controller => $controller_name, action => 'index')
      ->name("${reference_base}_index");

    # Add a parent navigation item for General Details under Reference
    $app->add_navigation_item({
      title  => 'General Details',
      path   => $reference_base,
      parent => '/Reference',
      order  => 10,
    });

    my $gm = $app->get_root_manager->generalManager;
    if (not defined $gm) {
      $logger->logcroak('No pair manager in manager');
    }

    $app->plugins->on(
      'evonytkrtips_initialized' => sub($self, $manager) {
        $logger->debug(
          "evonytkrtips_initialized sub has controller_name $controller_name.");

        if (not defined $gm) {
          $logger->logcroak("general manager must be defined");
        }

        while (my ($k, $v) = each %{ $gm->get_all_generals() }) {
          $logger->debug("building Reference Routes for $k");

          my $gr  = "/Reference/Generals/$k";
          my $grn = "${k}ReferenceRoute";
          $grn =~ s/ /_/g;

          $referenceRoutes->get("/$k" => { name => $k })
            ->to(controller => 'Generals', action => 'show')
            ->name($grn);

          $app->add_navigation_item({
            title  => "Detials for $k",
            path   => $gr,
            parent => '/Reference/Generals',
            order  => 20,
          });

        }

      }
    );
    # two routes for directory indices
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

    $mainRoutes->get('/:uiTarget/:buffActivation')
      ->requires(is_valid_buffActivation => 1)
      ->to(
      controller => 'Generals',
      action     => 'buffActivation_index',
      )->name('General_dynamic_buffActivation_index');
    # two routes for the user interface
    $mainRoutes->get('/:uiTarget/:buffActivation/comparison')->to(
      controller => 'Generals',
      action     => 'singleTable',
    )->name('General_dynamic_singleTable');
    $mainRoutes->get('/:uiTarget/:buffActivation/pair-comparison')->to(
      controller => 'Generals',
      action     => 'pairTable',
    )->name('General_dynamic_pairTable');

    # two routes to generate the lists of names
    $mainRoutes->get('/:uiTarget/:buffActivation/data.json')->to(
      controller => 'Generals',
      action     => 'singleData',
    )->name('Generals_dynamic_singleData');
    $mainRoutes->get('/:uiTarget/:buffActivation/pair/data.json')->to(
      controller => 'Generals',
      action     => 'pairData',
    )->name('Generals_dynamic_pairData');

    # two routes to generate data on a single row within the table
    $mainRoutes->get('/:uiTarget/:buffActivation/:isPrimary/row.json')->to(
      controller => 'Generals',
      action     => 'singleRow',
    )->name('Generals_dynamic_singleRow');
    $mainRoutes->get('/:uiTarget/:buffActivation/:run_id/pair-details-stream')->to(
      controller => 'Generals',
      action     => 'stream_pair_details',
    )->name('Generals_dynamic_pairDetails');

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
      }
    }

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
    $logger->info("looking for index at $markdown_path");

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
        $logger->debug("Using $targetType as targetType for $name");
        my $summarizer = Game::EvonyTKR::Model::Buff::Summarizer->new(
          rootManager    => $self->app->get_root_manager(),
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

     # Stash the full buff and debuff hashes for granular access in the template
        $self->stash(
          'buff-summaries' => {
            # For backward compatibility
            marchIncrease => $summarizer->buffValues->{$targetType}->{'March Size'},
            attackIncrease => $summarizer->buffValues->{$targetType}->{'Attack'},
            defenseIncrease => $summarizer->buffValues->{$targetType}->{'Defense'},
            hpIncrease => $summarizer->buffValues->{$targetType}->{'HP'},

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
      template       => 'generals/GeneralTableSingle',
      mode           => 'single',
      generalType    => $generalType,
      buffActivation => $buffActivation,
      uiTarget       => $uiTarget,
      covenantLevel  => $covenantLevel,
      ascendingLevel => $ascendingLevel,
      specialties    => \@specialties,
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

  sub pairTable ($self) {
    my $distDir = Mojo::File::Share::dist_dir('Game::EvonyTKR');

    my $slug_ui   = $self->stash('uiTarget');
    my $slug_buff = $self->stash('buffActivation');

    # Lookup route metadata
    my $routing    = Game::EvonyTKR::Control::Generals::Routing->new;
    my $route_meta = $routing->lookup_route($slug_ui, $slug_buff);

    unless ($route_meta) {
      $logger->error("Invalid pair route: $slug_ui | $slug_buff");

      if ($self->app->mode eq 'development') {
        $logger->debug("Known valid routes:");
        $routing->each_valid_route(
          sub ($key, $meta) {
            $logger->debug("  $key => " . Data::Printer::np($meta));
          }
        );
      }

      return $self->render_not_found;
    }

    # Extract metadata
    my $generalType    = $route_meta->{generalType};
    my $buffActivation = $route_meta->{buffActivation};
    my $uiTarget       = $route_meta->{uiTarget};

    # Fetch query params with defaults
    my $ascendingLevel       = $self->param('ascendingLevel') // 'red5';
    my $primaryCovenantLevel = $self->param('primaryCovenantLevel')
      // 'civilization';
    my $secondaryCovenantLevel = $self->param('secondaryCovenantLevel')
      // 'civilization';

    my @primarySpecialties =
      map { $self->param("primarySpecialty$_") // 'gold' } (1 .. 4);
    my @secondarySpecialties =
      map { $self->param("secondarySpecialty$_") // 'gold' } (1 .. 4);

    # Validate
    my $data_model = Game::EvonyTKR::Model::Data->new;

    unless ($data_model->validateBuffActivation($buffActivation)) {
      $logger->warn(
        "Invalid Buff Activation: $buffActivation, using 'Overall'");
      $buffActivation = 'Overall';
    }

    unless ($data_model->checkAscendingLevel($ascendingLevel)) {
      $logger->warn("Invalid ascendingLevel: $ascendingLevel, using 'red5'");
      $ascendingLevel = 'red5';
    }

    unless ($data_model->checkCovenantLevel($primaryCovenantLevel)) {
      $logger->warn(sprintf('Invalid primaryCovenantLevel: %s, using "civilization"', $primaryCovenantLevel));
      $primaryCovenantLevel = 'civilization';
    }

    unless ($data_model->checkCovenantLevel($secondaryCovenantLevel)) {
      $logger->warn(sprintf('Invalid secondaryCovenantLevel: %s, using "civilization"', $secondaryCovenantLevel));
      $secondaryCovenantLevel = 'civilization';
    }

    @primarySpecialties =
      $data_model->normalizeSpecialtyLevels(@primarySpecialties);
    @secondarySpecialties =
      $data_model->normalizeSpecialtyLevels(@secondarySpecialties);

    $self->stash(
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
      $logger->debug("Rendering from markdown index file");
      return $self->render_markdown_file($markdown_path);
    }

    $logger->debug("Rendering without markdown file");
    return $self->render;
  }

  sub singleData ($self) {
    my $distDir   = Mojo::File::Share::dist_dir('Game::EvonyTKR');
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

    my $gm = $self->app->get_general_manager();
    my @selected;
    while (my ($key, $general) = each(%{ $gm->get_all_generals() })) {
      $logger->debug(
        "inspecting '$key', first need to see if it is a $generalType."
          . Data::Printer::np($general));
      if (none { lc($_) eq $generalType } @{ $general->type }) {
        $logger->debug("none of "
            . $general->name
            . "'s types: "
            . Data::Printer::np($general->type)
            . "match as a $generalType.");
        next;
      }

      my $result = { primary => $general, };
      push @selected, $result;
    }
    if (scalar @selected) {
      return $self->render(json => \@selected);
    }
    else {
      return $self->render(json => {});
    }
  }

  sub pairData ($self) {
    my $slug_ui   = $self->stash('uiTarget');
    my $slug_buff = $self->stash('buffActivation');

    # Lookup route metadata
    my $routing    = Game::EvonyTKR::Control::Generals::Routing->new;
    my $route_meta = $routing->lookup_route($slug_ui, $slug_buff);

    unless ($route_meta) {
      $logger->error("Invalid pair route: $slug_ui | $slug_buff");

      if ($self->app->mode eq 'development') {
        $logger->debug("Known valid routes:");
        $routing->each_valid_route(
          sub ($key, $meta) {
            $logger->debug("  $key => " . Data::Printer::np($meta));
          }
        );
      }

      return $self->render_not_found;
    }

    # Extract metadata
    my $generalType    = $route_meta->{generalType};
    my $buffActivation = $route_meta->{buffActivation};
    my $uiTarget       = $route_meta->{uiTarget};

    my $pm    = $self->app->get_general_pair_manager();
    my @pairs = @{ $pm->get_pairs_by_type($generalType) };

    $logger->debug(sprintf('There are %s pairs to return.', scalar(@pairs)));

    # Return just the basic pair information without computing buffs
    my @json_data = map { {
      primary   => { name => $_->primary->name },
      secondary => { name => $_->secondary->name }
    } } @pairs;

    @json_data = sort {
      # First compare primary->name
      my $primary_cmp = $a->{primary}->{name} cmp $b->{primary}->{name};

      # If primary names are the same, compare secondary->name
      if ($primary_cmp == 0) {
        return $a->{secondary}->{name} cmp $b->{secondary}->{name};
      }

      # Otherwise, return the primary name comparison result
      return $primary_cmp;
    } @json_data;

    $self->render(json => { data => \@json_data });
  }

  sub singleRow ($self) {
    my $name      = $self->param('name');
    my $isPrimary = $self->param('isPrimary') // 1;         # Default to primary

    if ($isPrimary eq 'primary')   { $isPrimary = 1; }
    if ($isPrimary eq 'secondary') { $isPrimary = 0; }

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

    $logger->debug(sprintf('Computing general buffs for  %s, (isPrimary=%s, uiTarget=%s)',
    $name, $isPrimary, $uiTarget));

    # Get query parameters with defaults
    my $ascendingLevel = $self->param('ascendingLevel') // 'red5';
    my $covenantLevel  = $self->param('covenantLevel')
      // $self->param('primaryCovenantLevel') // 'civilization';
    my @specialties;
    push @specialties,
      $self->param('primarySpecialty1') // $self->param('specialty1') // 'gold';
    push @specialties,
      $self->param('primarySpecialty2') // $self->param('specialty2') // 'gold';
    push @specialties,
      $self->param('primarySpecialty3') // $self->param('specialty3') // 'gold';
    push @specialties,
      $self->param('primarySpecialty4') // $self->param('specialty4') // 'gold';

    # Validate parameters using enums from Game::EvonyTKR::Model::Data
    my $data_model = Game::EvonyTKR::Model::Data->new();

    # Validate ascending level
    if (!$data_model->checkAscendingLevel($ascendingLevel)) {
      $logger->warn(
        "Invalid ascendingLevel: $ascendingLevel, using default 'red5'");
      $ascendingLevel = 'red5';
    }
    else {
      $logger->debug(
        "mayor_comparison_json using ascendingLevel $ascendingLevel");
    }

    # Validate specialty levels
    @specialties = $data_model->normalizeSpecialtyLevels(@specialties);

    my $general =
      $self->app->get_root_manager->generalManager->getGeneral($name);

    if (!$general) {
      return $self->render(
        json   => { error => "General not found" },
        status => 404
      );
    }

    my $summarizer = Game::EvonyTKR::Model::Buff::Summarizer->new(
      rootManager    => $self->app->get_root_manager(),
      general        => $general,
      isPrimary      => $isPrimary,
      targetType     => $route_meta->{generalType},
      activationType => $buffActivation,
      ascendingLevel => $ascendingLevel,
      covenantLevel  => $covenantLevel,
      specialty1     => $specialties[0],
      specialty2     => $specialties[1],
      specialty3     => $specialties[2],
      specialty4     => $specialties[3],
    );

    $summarizer->updateBuffs();
    $summarizer->updateDebuffs();

    unless ($generalType =~ /(Ground|Mounted|Ranged|Siege)/i) {
      $generalType = 'Overall';
    }

    my $result = {
      marchbuff             => $summarizer->buffValues->{$generalType}->{'March Size'},
      attackbuff            => $summarizer->buffValues->{$generalType}->{'Attack'},
      defensebuff           => $summarizer->buffValues->{$generalType}->{'Defense'},
      hpbuff                => $summarizer->buffValues->{$generalType}->{'HP'},
      groundattackdebuff    => $summarizer->debuffValues->{'Ground Troops'}->{'Attack'},
      grounddefensedebuff   => $summarizer->debuffValues->{'Ground Troops'}->{'Defense'},
      groundhpdebuff        => $summarizer->debuffValues->{'Ground Troops'}->{'HP'},
      mountedattackdebuff   => $summarizer->debuffValues->{'Mounted Troops'}->{'Attack'},
      mounteddefensedebuff  => $summarizer->debuffValues->{'Mounted Troops'}->{'Defense'},
      mountedhpdebuff       => $summarizer->debuffValues->{'Mounted Troops'}->{'HP'},
      rangedattackdebuff    => $summarizer->debuffValues->{'Ranged Troops'}->{'Attack'},
      rangeddefensedebuff   => $summarizer->debuffValues->{'Ranged Troops'}->{'Defense'},
      rangedhpdebuff        => $summarizer->debuffValues->{'Ranged Troops'}->{'HP'},
      siegeattackdebuff     => $summarizer->debuffValues->{'Siege Machines'}->{'Attack'},
      siegedefensedebuff    => $summarizer->debuffValues->{'Siege Machines'}->{'Defense'},
      siegehpdebuff         => $summarizer->debuffValues->{'Siege Machines'}->{'HP'},
    };
    if ($isPrimary) {
      $result->{primary} = $general;
    }
    else {
      $result->{secondary} = $general;
    }
    return $self->render(json => $result);
  }

  sub stream_pair_details ($c) {
    my $json = JSON::PP->new->allow_blessed->convert_blessed->canonical->utf8();
    my $slug_ui       = $c->stash('uiTarget');
    my $slug_buff     = $c->stash('buffActivation');
    my $run_id    = $c->stash('run_id');

    $logger->info(sprintf('stream_pair_details called uiTarget: %s; buffActivation: %s; run_id: %s',
    $slug_ui, $slug_buff, $run_id));

    # Lookup route metadata
    my $routing    = Game::EvonyTKR::Control::Generals::Routing->new;
    my $route_meta = $routing->lookup_route($slug_ui, $slug_buff);

    unless ($route_meta) {
      $logger->error("Invalid pair route: $slug_ui | $slug_buff");

      if ($c->app->mode eq 'development') {
        $logger->debug("Known valid routes:");
        $routing->each_valid_route(
          sub ($key, $meta) {
            $logger->debug("  $key => " . Data::Printer::np($meta));
          }
        );
      }
      return $c->render_not_found;
    }

    my $generalType    = $route_meta->{generalType};
    my $buffActivation = $route_meta->{buffActivation};
    my $uiTarget       = $route_meta->{uiTarget};
    my $pm    = $c->app->get_general_pair_manager();
    my $pairs;
    @$pairs = $pm->get_pairs_by_type($generalType)->@*;
    @$pairs = sort {
      my $pc = $a->primary->name cmp $b->primary->name;
      if($pc == 0){
        return $a->secondary->name cmp $b->secondary->name;
      }
      return $pc;
    } $pairs->@*;

    $logger->info(sprintf('There are %s pairs compute details for.', scalar(@$pairs)));

    my $ascendingLevel       = $c->param('ascendingLevel') // 'red5';
    my $primaryCovenantLevel = $c->param('primaryCovenantLevel')
      // 'civilization';
    my @primarySpecialties;
    push @primarySpecialties, $c->param('primarySpecialty1') // 'gold';
    push @primarySpecialties, $c->param('primarySpecialty2') // 'gold';
    push @primarySpecialties, $c->param('primarySpecialty3') // 'gold';
    push @primarySpecialties, $c->param('primarySpecialty4') // 'gold';
    my $secondaryCovenantLevel = $c->param('secondaryCovenantLevel')
      // 'civilization';
    my @secondarySpecialties;
    push @secondarySpecialties, $c->param('secondarySpecialty1') // 'gold';
    push @secondarySpecialties, $c->param('secondarySpecialty2') // 'gold';
    push @secondarySpecialties, $c->param('secondarySpecialty3') // 'gold';
    push @secondarySpecialties, $c->param('secondarySpecialty4') // 'gold';

    # Validate parameters using enums from Game::EvonyTKR::Model::Data
    my $data_model = Game::EvonyTKR::Model::Data->new();

    # Validate ascending level
    if (!$data_model->checkAscendingLevel($ascendingLevel)) {
      $logger->warn(
        "Invalid ascendingLevel: $ascendingLevel, using default 'red5'");
      $ascendingLevel = 'red5';
    }

    if (!$data_model->checkCovenantLevel($primaryCovenantLevel)) {
      $logger->warn(sprintf('Invalid covenantLevel: %s, using default "civilization"', $primaryCovenantLevel));
      $primaryCovenantLevel = 'civilization';
    }

    @primarySpecialties =
      $data_model->normalizeSpecialtyLevels(@primarySpecialties);

    if (!$data_model->checkCovenantLevel($secondaryCovenantLevel)) {
    $logger->warn(sprintf('Invalid covenantLevel: %s, using default "civilization"', $secondaryCovenantLevel));
      $secondaryCovenantLevel = 'civilization';
    }

    @secondarySpecialties =
      $data_model->normalizeSpecialtyLevels(@secondarySpecialties);

    my $typeMap = {
      'Ground Specialists'  => 'ground_specialist',
      'Ranged Specialists'  => 'ranged_specialist',
      'Siege Specialists'   => 'siege_specialist',
      'Mounted Specialists' => 'mounted_specialist',
      'Wall Specialists'    => 'wall',
    };
    $c->render_later;
    $c->res->headers->content_type('text/event-stream');
    $c->res->headers->add('Cache-Control' => 'no-cache');
    $c->res->headers->add('Connection' => 'keep-alive');
    $c->inactivity_timeout(300);
    # Change content type and finalize response headers
    $c->write_sse;

    my $index = 0;
    my $loop = Mojo::IOLoop->new;
    my $loopId;
   $loopId = $loop->recurring(0.05 => sub {
      if ($index >= scalar(@$pairs)) {
        $logger->info("$index is larger than ". scalar(@$pairs));
        $loop->emit(complete => 1);
        $loop->remove($loopId);
        return;
      }
      my $pair = $pairs->[$index++];
      my $sprintfTemplate =
          'Computing Pair Buffs for %s: %s/%s Buff Activation %s, '
        . 'targetType %s ascendingLevel %s primary CovenantLevel %s '
        . 'primary Specialties %s secondary CovenantLevel %s '
        . 'secondary Specialties %s';
      $logger->info(sprintf($sprintfTemplate, $index,
        $pair->primary->name,            $pair->secondary->name,
        $buffActivation,                 $route_meta->{generalType},
        $ascendingLevel,                 $primaryCovenantLevel,
        join(', ', @primarySpecialties), $secondaryCovenantLevel,
        join(', ', @secondarySpecialties)));

      $pair->updateBuffsAndDebuffs(
        $route_meta->{generalType}, $ascendingLevel,
        $primaryCovenantLevel,      \@primarySpecialties,
        $secondaryCovenantLevel,    \@secondarySpecialties,
        $buffActivation
      );

      my $buffKey = $route_meta->{generalType} =~ s/_/ /r;
      $buffKey =~ s/(\w)(\w+) specialist/\U$1\L$2 \UT\Lroops/;
      $buffKey =~ s/Siege Troops/Siege Machines/;
      $logger->debug("buffKey is $buffKey");

      my $row_data = {
        "primary"            => $pair->primary,
        "secondary"          => $pair->secondary,
        "attackbuff"         => $pair->buffValues->{$buffKey}->{'Attack'},
        "defensebuff"        => $pair->buffValues->{$buffKey}->{'Defense'},
        "hpbuff"             => $pair->buffValues->{$buffKey}->{'HP'},
        "marchbuff"          => $pair->buffValues->{$buffKey}->{'March Size'},
        "groundattackdebuff" =>
          $pair->debuffValues->{'Ground Troops'}->{'Attack'},
        "grounddefensedebuff" =>
          $pair->debuffValues->{'Ground Troops'}->{'Defense'},
        "groundhpdebuff"      => $pair->debuffValues->{'Ground Troops'}->{'HP'},
        "mountedattackdebuff" =>
          $pair->debuffValues->{'Mounted Troops'}->{'Attack'},
        "mounteddefensedebuff" =>
          $pair->debuffValues->{'Mounted Troops'}->{'Defense'},
        "mountedhpdebuff"    => $pair->debuffValues->{'Mounted Troops'}->{'HP'},
        "rangedattackdebuff" =>
          $pair->debuffValues->{'Ranged Troops'}->{'Attack'},
        "rangeddefensedebuff" =>
          $pair->debuffValues->{'Ranged Troops'}->{'Defense'},
        "rangedhpdebuff"    => $pair->debuffValues->{'Ranged Troops'}->{'HP'},
        "siegeattackdebuff" =>
          $pair->debuffValues->{'Siege Machines'}->{'Attack'},
        "siegedefensedebuff" =>
          $pair->debuffValues->{'Siege Machines'}->{'Defense'},
        "siegehpdebuff" => $pair->debuffValues->{'Siege Machines'}->{'HP'},
      };
      my $encoded = $json->encode($row_data);
      $logger->info("encoded string is $encoded");

      $loop->emit(result => $row_data);
    });

    $loop->on(error => sub ($e, $err) {
      $logger->error('SSE Loop Error: $err') if $err;
    });
    $loop->on(complete => sub{
      $c->write_sse(data => {complete => 1});
      $loop->reset;
    });
    $loop->on(result => sub ($subprocess, $result) {
      $logger->info('SSE loop result emitter firing for js: ' . Data::Printer::np($result));
      $c->write_sse({
        runId => $run_id,
        data  => $result->{result}
      });
    });

    $loop->on(finish => sub {
      $logger->info('SSE loop complete');
    });
    $loop->start;
    $logger->info(sprintf('stream_pair_details uiTarget: %s; buffActivation: %s; run_id: %s complete.',$slug_ui, $slug_buff, $run_id));
  }

  sub pairRow ($self) {
    my $primaryName   = Mojo::Util::url_unescape($self->param('primary'));
    my $secondaryName = Mojo::Util::url_unescape($self->param('secondary'));
    my $slug_ui       = $self->stash('uiTarget');
    my $slug_buff     = $self->stash('buffActivation');

    # Lookup route metadata
    my $routing    = Game::EvonyTKR::Control::Generals::Routing->new;
    my $route_meta = $routing->lookup_route($slug_ui, $slug_buff);

    unless ($route_meta) {
      $logger->error("Invalid pair route: $slug_ui | $slug_buff");

      if ($self->app->mode eq 'development') {
        $logger->debug("Known valid routes:");
        $routing->each_valid_route(
          sub ($key, $meta) {
            $logger->debug("  $key => " . Data::Printer::np($meta));
          }
        );
      }

      return $self->render_not_found;
    }

    # Extract metadata
    my $generalType    = $route_meta->{generalType};
    my $buffActivation = $route_meta->{buffActivation};
    my $uiTarget       = $route_meta->{uiTarget};

    $logger->debug(
"Computing pair buffs for primary '$primaryName' secondary '$secondaryName', uiTarget '$uiTarget'"
    );

    if (any { !defined($_) || $_ eq '' }
      ($primaryName, $secondaryName, $uiTarget)) {
      $logger->error("Missing parameter");
      return $self->render(
        json   => { error => "Missing parameter" },
        status => 400
      );
    }

    my $ascendingLevel       = $self->param('ascendingLevel') // 'red5';
    my $primaryCovenantLevel = $self->param('primaryCovenantLevel')
      // 'civilization';
    my @primarySpecialties;
    push @primarySpecialties, $self->param('primarySpecialty1') // 'gold';
    push @primarySpecialties, $self->param('primarySpecialty2') // 'gold';
    push @primarySpecialties, $self->param('primarySpecialty3') // 'gold';
    push @primarySpecialties, $self->param('primarySpecialty4') // 'gold';
    my $secondaryCovenantLevel = $self->param('secondaryCovenantLevel')
      // 'civilization';
    my @secondarySpecialties;
    push @secondarySpecialties, $self->param('secondarySpecialty1') // 'gold';
    push @secondarySpecialties, $self->param('secondarySpecialty2') // 'gold';
    push @secondarySpecialties, $self->param('secondarySpecialty3') // 'gold';
    push @secondarySpecialties, $self->param('secondarySpecialty4') // 'gold';

    # Validate parameters using enums from Game::EvonyTKR::Model::Data
    my $data_model = Game::EvonyTKR::Model::Data->new();

    # Validate ascending level
    if (!$data_model->checkAscendingLevel($ascendingLevel)) {
      $logger->warn(
        "Invalid ascendingLevel: $ascendingLevel, using default 'red5'");
      $ascendingLevel = 'red5';
    }

    if (!$data_model->checkCovenantLevel($primaryCovenantLevel)) {
      $logger->warn(
"Invalid covenantLevel: $primaryCovenantLevel, using default 'civilization'"
      );
      $primaryCovenantLevel = 'civilization';
    }

    @primarySpecialties =
      $data_model->normalizeSpecialtyLevels(@primarySpecialties);

    if (!$data_model->checkCovenantLevel($secondaryCovenantLevel)) {
      $logger->warn(
"Invalid covenantLevel: $secondaryCovenantLevel, using default 'civilization'"
      );
      $secondaryCovenantLevel = 'civilization';
    }

    @secondarySpecialties =
      $data_model->normalizeSpecialtyLevels(@secondarySpecialties);

    my $typeMap = {
      'Ground Specialists'  => 'ground_specialist',
      'Ranged Specialists'  => 'ranged_specialist',
      'Siege Specialists'   => 'siege_specialist',
      'Mounted Specialists' => 'mounted_specialist',
      'Wall Specialists'    => 'wall',
    };

    my $pm = $self->app->get_general_pair_manager();

    my ($pair) = grep {
      $_->primary->name eq $primaryName && $_->secondary->name eq $secondaryName
    } $pm->get_pairs_by_type($route_meta->{generalType})->@*;

    unless ($pair) {
      $logger->warn("Pair not found: $primaryName / $secondaryName");

      return $self->render(
        json   => { error => "Pair not found" },
        status => 404
      );
    }
    my $sprintfTemplate =
        'Computing Pair Buffs for %s/%s Buff Activation %s, '
      . 'targetType %s ascendingLevel %s primary CovenantLevel %s '
      . 'primary Specialties %s secondary CovenantLevel %s '
      . 'secondary Specialties %s';
    $logger->info(sprintf($sprintfTemplate,
      $pair->primary->name,            $pair->secondary->name,
      $buffActivation,                 $route_meta->{generalType},
      $ascendingLevel,                 $primaryCovenantLevel,
      join(', ', @primarySpecialties), $secondaryCovenantLevel,
      join(', ', @secondarySpecialties)));

    $pair->updateBuffsAndDebuffs(
      $route_meta->{generalType}, $ascendingLevel,
      $primaryCovenantLevel,      \@primarySpecialties,
      $secondaryCovenantLevel,    \@secondarySpecialties,
      $buffActivation
    );

    my $buffKey = $route_meta->{generalType} =~ s/_/ /r;
    $buffKey =~ s/(\w)(\w+) specialist/\U$1\L$2 \UT\Lroops/;
    $buffKey =~ s/Siege Troops/Siege Machines/;
    $logger->debug("buffKey is $buffKey");

    return $self->render(
      json => {
        "primary"            => $pair->primary,
        "secondary"          => $pair->secondary,
        "attackbuff"         => $pair->buffValues->{$buffKey}->{'Attack'},
        "defensebuff"        => $pair->buffValues->{$buffKey}->{'Defense'},
        "hpbuff"             => $pair->buffValues->{$buffKey}->{'HP'},
        "marchbuff"          => $pair->buffValues->{$buffKey}->{'March Size'},
        "groundattackdebuff" =>
          $pair->debuffValues->{'Ground Troops'}->{'Attack'},
        "grounddefensedebuff" =>
          $pair->debuffValues->{'Ground Troops'}->{'Defense'},
        "groundhpdebuff"      => $pair->debuffValues->{'Ground Troops'}->{'HP'},
        "mountedattackdebuff" =>
          $pair->debuffValues->{'Mounted Troops'}->{'Attack'},
        "mounteddefensedebuff" =>
          $pair->debuffValues->{'Mounted Troops'}->{'Defense'},
        "mountedhpdebuff"    => $pair->debuffValues->{'Mounted Troops'}->{'HP'},
        "rangedattackdebuff" =>
          $pair->debuffValues->{'Ranged Troops'}->{'Attack'},
        "rangeddefensedebuff" =>
          $pair->debuffValues->{'Ranged Troops'}->{'Defense'},
        "rangedhpdebuff"    => $pair->debuffValues->{'Ranged Troops'}->{'HP'},
        "siegeattackdebuff" =>
          $pair->debuffValues->{'Siege Machines'}->{'Attack'},
        "siegedefensedebuff" =>
          $pair->debuffValues->{'Siege Machines'}->{'Defense'},
        "siegehpdebuff" => $pair->debuffValues->{'Siege Machines'}->{'HP'},
      }
    );
  }

}

1;
