use v5.40.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Game::EvonyTKR::Model::General;
require Game::EvonyTKR::Model::General::Manager;
require Game::EvonyTKR::Model::Buff::Summarizer;
require Data::Printer;
use namespace::clean;

package Game::EvonyTKR::Controller::Generals {
  use Mojo::Base 'Game::EvonyTKR::Controller::CollectionBase';
  use List::AllUtils qw( all any none );
  use Carp;

  # Specify which collection this controller handles
  sub collection_name {'generals'}

  my $base = '/Generals';

  sub getBase($self) {
    return $base;
  }

  sub controller_name ($self) {
    return "Generals";
  }

  sub get_manager ($self) {
    return $self->app->get_root_manager->generalManager;
  }

  sub register($self, $app, $config = {}) {
    my $logger = Log::Log4perl->get_logger(__PACKAGE__);
    $logger->info("Registering routes for " . ref($self));
    $self->SUPER::register($app, $config);

    $app->hook(before_dispatch => sub {
         my $c = shift;
         $logger->debug("Request path: " . $c->req->url->path);
         $logger->debug("Matched route name: " . ($c->match->endpoint ? $c->match->endpoint->name : 'none'));
         $logger->debug("Stash values: " . Data::Printer::np($c->stash));
       });


    $app->helper(
      get_general_manager => sub {
        return $app->get_root_manager->generalManager;
      }
    );

    my @parts     = split(/::/, ref($self));
    my $baseClass = pop(@parts);

    my $controller_name =
        $self->can('controller_name')
      ? $self->controller_name()
      : $baseClass;

    $logger->debug("got controller_name $controller_name.");

    my $r      = $app->routes;
    my $routes = $r->any("$base");

    $routes->get('/')
      ->to(controller => $controller_name, action => 'index')
      ->name("${base}_index");

    $routes->any('/details')->to(
      cb => sub ($c) {
        $c->redirect_to('/Generals');
      }
    );

    # Add a parent navigation item for Generals
    $app->add_navigation_item({
      title => 'Generals',
      path  => '/Generals',
      order => 10,
    });

    # Add a parent navigation item for Generals
    $app->add_navigation_item({
      title  => 'General Details',
      path   => '/Generals/details',
      parent => '/Generals',
      order  => 10,
    });

    $app->plugins->on(
      'evonytkrtips_initialized' => sub($self, $manager) {
        $logger->debug(
          "evonytkrtips_initialized sub has controller_name $controller_name.");

        if (not defined $manager) {
          $logger->logcroak('No Manager Defined');
        }

        my $pm = $manager->generalPairManager;
        if (not defined $pm) {
          $logger->logcroak('No pair manager in manager');
        }

        my $generals =
          $app->get_root_manager->generalManager->get_all_generals();
        foreach my $general (values %$generals) {
          $app->add_navigation_item({
            title  => "Details for " . $general->name,
            path   => "/Generals/details/" . $general->name,
            parent => 'Generals/details/',
            order  => 10,
          });
        }

        $routes->get(
          '/Mayors/comparison' => {
            generalType => 'mayor',
            linkTarget  => 'Mayor',
            isPrimary   => 1
          }
          )
          ->to(controller => $controller_name, action => 'mayor_comparison')
          ->name("${base}_mayor_comparison");

        $app->add_navigation_item({
          title  => "Comparison of Mayor's Buffs",
          path   => "/Generals/Mayors/comparison",
          parent => '/Generals/Mayors',
          order  => 10,
        });

        $routes->get(
          '/Mayors/comparison/data.json' => {
            generalType => 'Mayor',
            linkTarget  => 'Mayor',
            isPrimary   => 1
          }
        )->to(
          controller => $controller_name,
          action     => 'mayor_comparison_json'
        )->name("${base}_mayor_comparison_json");

        my @generalTypes = $pm->get_pair_types();
        $logger->debug("got generalTypes " . Data::Printer::np(@generalTypes));
        foreach my $type (@generalTypes) {
          my $linkTarget = $type =~ s/_/ /gr;
          $linkTarget =~ s/(\w)(\w+)( specialist)?/\U$1\L$2 \US\Lpecialist/;

          # For primary generals
          $routes->get(
            "/$linkTarget/primary/:name" => {
              generalType => $type,
              linkTarget  => $linkTarget,
              isPrimary   => 1
            }
            )
            ->to(controller => $controller_name, action => 'general_buffs')
            ->name("${base}_${type}_primary");

          # For secondary generals
          $routes->get(
            "/$linkTarget/secondary/:name" => {
              generalType => $type,
              linkTarget  => $linkTarget,
              isPrimary   => 0
            }
            )
            ->to(controller => $controller_name, action => 'general_buffs')
            ->name("${base}_${type}_secondary");

        }
      }
    );
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
    my @specialities;
    push @specialities, $self->param('speciality1') // 'gold';
    push @specialities, $self->param('speciality2') // 'gold';
    push @specialities, $self->param('speciality3') // 'gold';
    push @specialities, $self->param('speciality4') // 'gold';

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

    @specialities = $data_model->normalizeSpecialityLevels(@specialities);

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
          speciality1    => $specialities[0],
          speciality2    => $specialities[1],
          speciality3    => $specialities[2],
          speciality4    => $specialities[3],
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

      return $self->render(template => $self->details_template);
    }
    $self->SUPER::show();
  }

  sub mayor_comparison ($self) {
    my $logger      = Log::Log4perl->get_logger(__PACKAGE__);
    my $distDir     = Mojo::File::Share::dist_dir('Game::EvonyTKR');
    my $generalType = $self->stash('generalType');
    my $linkTarget  = $self->stash('linkTarget');

    # Get query parameters with defaults
    my $covenantLevel  = $self->param('covenantLevel')  // 'civilization';
    my $ascendingLevel = $self->param('ascendingLevel') // 'red5';
    my @specialities;
    push @specialities, $self->param('speciality1') // 'gold';
    push @specialities, $self->param('speciality2') // 'gold';
    push @specialities, $self->param('speciality3') // 'gold';
    push @specialities, $self->param('speciality4') // 'gold';

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

    @specialities = $data_model->normalizeSpecialityLevels(@specialities);

    # Stash data for the template
    $self->stash(
      template       => 'generals/GeneralTableSingle',
      mode           => 'single',
      covenantLevel  => $covenantLevel,
      ascendingLevel => $ascendingLevel,
      specialities   => \@specialities,

     # Add enum values for which I do not yet have managers that provide helpers
      covenantLevelValues => $data_model->covenantLevels,
    );

    my $markdown_path = $distDir->child("pages/generals/Mayor/comparison.md");

    if (-f $markdown_path) {
      $logger->debug("Rendering from markdown index file");
      return $self->SUPER::render_markdown_file($self, $markdown_path);
    }
    else {
      $logger->debug("Rendering without markdown file");
      return $self->render;
    }
  }

  sub mayor_comparison_json ($self) {
    my $logger      = Log::Log4perl->get_logger(__PACKAGE__);
    my $distDir     = Mojo::File::Share::dist_dir('Game::EvonyTKR');
    my $generalType = $self->stash('generalType');
    my $linkTarget  = $self->stash('linkTarget');

    my $gm                   = $self->app->get_general_manager();
    my @generalBuffSummaries;
    while (my ($key, $general) = each(%{ $gm->get_all_generals() })) {
      $logger->debug("inspecting '$key', first need to see if it is a mayor."
          . Data::Printer::np($general));
      if (none { lc($_) eq 'mayor' } @{ $general->type }) {
        $logger->debug("none of "
            . $general->name
            . "'s types: "
            . Data::Printer::np($general->type)
            . "match as a mayor.");
        next;
      }

      my $result = {
        primary              => $general,
      };
      push @generalBuffSummaries, $result;
    }
    if (scalar @generalBuffSummaries) {
      return $self->render(json => \@generalBuffSummaries);
    }
    else {
      return $self->render(json => {});
    }
  }

  sub general_buffs ($self) {
    my $logger     = Log::Log4perl->get_logger(__PACKAGE__);
    my $name       = $self->param('name');
    my $isPrimary  = $self->param('isPrimary')  // 1;    # Default to primary
    my $targetType = $self->param('targetType') // '';

    $logger->debug(
"Computing general buffs for $name (isPrimary=$isPrimary, targetType=$targetType)"
    );

    # Get query parameters with defaults
    my $ascendingLevel = $self->param('ascendingLevel') // 'red5';
    my $covenantLevel  = $self->param('covenantLevel')  // 'civilization';
    my @specialities;
    push @specialities, $self->param('speciality1') // 'gold';
    push @specialities, $self->param('speciality2') // 'gold';
    push @specialities, $self->param('speciality3') // 'gold';
    push @specialities, $self->param('speciality4') // 'gold';

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

    # Validate speciality levels
    @specialities = $data_model->normalizeSpecialityLevels(@specialities);

    my $general =
      $self->app->get_root_manager->generalManager->getGeneral($name);

    if (!$general) {
      return $self->render(
        json   => { error => "General not found" },
        status => 404
      );
    }

    # If targetType not provided but general has a type, use that
    if (!$targetType && $general->type) {
      if (ref $general->type eq 'ARRAY') {
        $targetType = $general->type->[0] if @{ $general->type };
      }
      else {
        $targetType = $general->type;
      }
    }

    my $summarizer = Game::EvonyTKR::Model::Buff::Summarizer->new(
      rootManager => $self->app->get_root_manager(),
      general     => $general,
      isPrimary   => $isPrimary,
      targetType  => $targetType,
      ascendingLevel => $ascendingLevel,
      covenantLevel  => $covenantLevel,
      speciality1    => $specialities[0],
      speciality2    => $specialities[1],
      speciality3    => $specialities[2],
      speciality4    => $specialities[3],
    );

    $summarizer->updateBuffs();
    $summarizer->updateDebuffs();

    my $result = {
      marchbuff            => $summarizer->marchIncrease,
      attackbuff           => $summarizer->attackIncrease,
      defensebuff          => $summarizer->defenseIncrease,
      hpbuff               => $summarizer->hpIncrease,
      groundattackdebuff   => $summarizer->reducegroundattack,
      grounddefensedebuff  => $summarizer->reducegrounddefense,
      groundhpdebuff       => $summarizer->reducegroundhp,
      mountedattackdebuff  => $summarizer->reducemountedattack,
      mounteddefensedebuff => $summarizer->reducemounteddefense,
      mountedhpdebuff      => $summarizer->reducemountedhp,
      rangedattackdebuff   => $summarizer->reducerangedattack,
      rangeddefensedebuff  => $summarizer->reducerangeddefense,
      rangedhpdebuff       => $summarizer->reducerangedhp,
      siegeattackdebuff    => $summarizer->reducesiegeattack,
      siegedefensedebuff   => $summarizer->reducesiegedefense,
      siegehpdebuff        => $summarizer->reducesiegehp,
    };
    if ($isPrimary) {
      $result->{primary} = $general;
    }
    else {
      $result->{secondary} = $general;
    }
    return $self->render(json => $result);
  }

}

1;
