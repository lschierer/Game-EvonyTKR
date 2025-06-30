use v5.40.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Game::EvonyTKR::Model::General;
require Game::EvonyTKR::Model::General::Manager;
require Game::EvonyTKR::Model::General::Pair;
require Game::EvonyTKR::Model::General::Pair::Manager;
require Game::EvonyTKR::Model::Buff::Summarizer;
require Game::EvonyTKR::Control::Generals::Routing;
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

  sub register($self, $app, $config = {}) {
    my $logger = Log::Log4perl->get_logger(__PACKAGE__);
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
        state $routing = Game::EvonyTKR::Control::General::Routing->new(
          debug => $app->mode eq 'development',);
        return $routing;
      }
    );

    my @parts     = split(/::/, ref($self));
    my $baseClass = pop(@parts);

    my $controller_name =
        $self->can('controller_name')
      ? $self->controller_name()
      : $baseClass;

    $logger->debug("got controller_name $controller_name.");

    my $mainRoutes      = $app->routes->any($base);
    my $referenceRoutes = $app->routes->any($reference_base);

    $mainRoutes->get('/')
      ->to(controller => $controller_name, action => 'index')
      ->name("${base}_index");

    $referenceRoutes->get('/')
      ->to(controller => $controller_name, action => 'index')
      ->name("${reference_base}_index");

    # Add a parent navigation item for Generals
    $app->add_navigation_item({
      title => 'Generals',
      path  => '/Generals',
      order => 10,
    });

    # Add a parent navigation item for Generals
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
          $self->logger->logcroak("general manager must be defined");
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
    $mainRoutes->get('/:uiTarget/:buffActivation/pair-row.json')->to(
      controller => 'Generals',
      action     => 'pairRow',
    )->name('Generals_dynamic_pairRow');

  }

  sub index($self) {
    my $logger     = Log::Log4perl->get_logger(__PACKAGE__);
    my $collection = collection_name();
    $logger->debug("Rendering index for $collection");

    # Check if markdown exists for this collection
    my $distDir       = Mojo::File::Share::dist_dir('Game::EvonyTKR');
    my $markdown_path = $distDir->child("pages/$collection/index.md");

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
      # Render with markdown
      $self->stash(template => '/generals/index');

      return $self->render_markdown_file($markdown_path,
        { template => 'generals/index' });
    }
    else {
      # Render just the items
      return $self->render(template => '/generals/index');
    }
  }

  sub show ($self) {
    my $logger = Log::Log4perl->get_logger(ref($self));
    $logger->debug("start of show method");
    my $name;
    $name = $self->param('name');

    $logger->debug("show detects name $name, showing details.");
    my $calculate_buffs = $self->param('calculate_buffs') // 0;

    my $covenantLevel  = $self->param('covenantLevel')  // 'civilization';
    my $ascendingLevel = $self->param('ascendingLevel') // 'red5';
    my @specialties;
    push @specialties, $self->param('specialty1') // 'gold';
    push @specialties, $self->param('specialty2') // 'gold';
    push @specialties, $self->param('specialty3') // 'gold';
    push @specialties, $self->param('specialty4') // 'gold';

    # Validate parameters using enums from Game::EvonyTKR::Model::Data
    my $data_model = Game::EvonyTKR::Model::Data->new();

    if (!$data_model->checkCovenantLevel($covenantLevel)) {
      $logger->warn(
        "Invalid covenantLevel: $covenantLevel, using default 'civilization'");
      $covenantLevel = 'civilization';
    }

    # Validate ascending level
    if (!$data_model->checkAscendingLevel($ascendingLevel)) {
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
            marchIncrease      => $summarizer->marchIncrease,
            attackIncrease     => $summarizer->attackIncrease,
            defenseIncrease    => $summarizer->defenseIncrease,
            hpIncrease         => $summarizer->hpIncrease,
            reducegroundattack => $summarizer->reducegroundattack,
            reducegroundhp     => $summarizer->reducegroundhp,

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
    my $logger  = Log::Log4perl->get_logger(__PACKAGE__);
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
    my $logger  = Log::Log4perl->get_logger(__PACKAGE__);
    my $distDir = Mojo::File::Share::dist_dir('Game::EvonyTKR');

    my $slug_ui   = $self->stash('uiTarget');
    my $slug_buff = $self->stash('buffActivation');

    # Lookup route metadata
    my $routing    = Game::EvonyTKR::Control::General::Routing->new;
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
      $logger->warn(
"Invalid primaryCovenantLevel: $primaryCovenantLevel, using 'civilization'"
      );
      $primaryCovenantLevel = 'civilization';
    }

    unless ($data_model->checkCovenantLevel($secondaryCovenantLevel)) {
      $logger->warn(
"Invalid secondaryCovenantLevel: $secondaryCovenantLevel, using 'civilization'"
      );
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
    my $logger    = Log::Log4perl->get_logger(__PACKAGE__);
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
    my $logger    = Log::Log4perl->get_logger(__PACKAGE__);
    my $slug_ui   = $self->stash('uiTarget');
    my $slug_buff = $self->stash('buffActivation');

    # Lookup route metadata
    my $routing    = Game::EvonyTKR::Control::General::Routing->new;
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

    $self->render(json => { data => \@json_data });
  }

  sub singleRow ($self) {
    my $logger    = Log::Log4perl->get_logger(__PACKAGE__);
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

    $logger->debug(
"Computing general buffs for $name (isPrimary=$isPrimary, uiTarget=$uiTarget)"
    );

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

    my $result = {
      marchbuff => $summarizer->getBuffForTypeAndKey(
        $route_meta->{generalType},
        'March Size Capacity'
      ),
      attackbuff =>
        $summarizer->getBuffForTypeAndKey($route_meta->{generalType}, 'Attack'),
      defensebuff => $summarizer->getBuffForTypeAndKey(
        $route_meta->{generalType}, 'Defense'
      ),
      hpbuff =>
        $summarizer->getBuffForTypeAndKey($route_meta->{generalType}, 'HP'),
      groundattackdebuff =>
        $summarizer->getBuffForTypeAndKey('Ground Troops', 'Attack'),
      grounddefensedebuff =>
        $summarizer->getBuffForTypeAndKey('Ground Troops', 'Defense'),
      groundhpdebuff =>
        $summarizer->getBuffForTypeAndKey('Ground Troops', 'HP'),
      mountedattackdebuff =>
        $summarizer->getBuffForTypeAndKey('Mounted Troops', 'Attack'),
      mounteddefensedebuff =>
        $summarizer->getBuffForTypeAndKey('Mounted Troops', 'Defense'),
      mountedhpdebuff =>
        $summarizer->getBuffForTypeAndKey('Mounted Troops', 'HP'),
      rangedattackdebuff =>
        $summarizer->getBuffForTypeAndKey('Ranged Troops', 'Attack'),
      rangeddefensedebuff =>
        $summarizer->getBuffForTypeAndKey('Ranged Troops', 'Defense'),
      rangedhpdebuff =>
        $summarizer->getBuffForTypeAndKey('Ranged Troops', 'HP'),
      siegeattackdebuff =>
        $summarizer->getBuffForTypeAndKey('Siege Machines', 'Attack'),
      siegedefensedebuff =>
        $summarizer->getBuffForTypeAndKey('Siege Machines', 'Defense'),
      siegehpdebuff =>
        $summarizer->getBuffForTypeAndKey('Siege Machines', 'HP'),
    };
    if ($isPrimary) {
      $result->{primary} = $general;
    }
    else {
      $result->{secondary} = $general;
    }
    return $self->render(json => $result);
  }

  sub pairRow ($self) {
    my $logger        = Log::Log4perl->get_logger(__PACKAGE__);
    my $primaryName   = Mojo::Util::url_unescape($self->param('primary'));
    my $secondaryName = Mojo::Util::url_unescape($self->param('secondary'));
    my $slug_ui       = $self->stash('uiTarget');
    my $slug_buff     = $self->stash('buffActivation');

    # Lookup route metadata
    my $routing    = Game::EvonyTKR::Control::General::Routing->new;
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
    $logger->info(sprintf(
'Computing Pair Buffs for %s/%s Buff Activation %s, targetType %s ascendingLevel %s primary CovenantLevel %s primary Specialties %s secondary CovenantLevel %s secondary Specialties %s',
      $pair->primary->name,            $pair->secondary->name,
      $buffActivation,                 $route_meta->{generalType},
      $ascendingLevel,                 $primaryCovenantLevel,
      join(', ', @primarySpecialties), $secondaryCovenantLevel,
      join(', ', @secondarySpecialties),
    ));
    $pair->updateBuffsAndDebuffs(
      $route_meta->{generalType}, $ascendingLevel,
      $primaryCovenantLevel,      \@primarySpecialties,
      $secondaryCovenantLevel,    \@secondarySpecialties,
      $buffActivation
    );

    return $self->render(json => $pair);
  }

}

1;
