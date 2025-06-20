use v5.40.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Game::EvonyTKR::Model::General;
require Game::EvonyTKR::Model::General::Manager;
require Game::EvonyTKR::Model::Buff::Summarizer;
require Data::Printer;
use namespace::clean;

package Game::EvonyTKR::Plugins::Generals {
  use Mojo::Base 'Game::EvonyTKR::Plugins::CollectionBase';

  # Specify which collection this controller handles
  sub collection_name {'generals'}

  my $base = '/Generals/';

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

    $app->helper(
      get_general_manager => sub {
        return $self->app->get_root_manager->generalManager;
      }
    );

    my @parts     = split(/::/, ref($self));
    my $baseClass = pop(@parts);

    my $controller_name =
        $self->can('controller_name')
      ? $self->controller_name()
      : $baseClass;

    $logger->debug("got controller_name $controller_name.");

    my $r = $app->routes;
    my $routes = $r->any("$base");

    $routes->get('/')
          ->to(controller => $controller_name, action => 'index')
          ->name("${base}_index");


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

      my @generalTypes = $pm->get_pair_types();
      $logger->debug("got generalTypes " . Data::Printer::np(@generalTypes));
      foreach my $type (@generalTypes) {
        my $linkTarget = $type =~ s/_/ /gr;
        $linkTarget =~ s/(\w)(\w+)( specialist)?/\U$1\L$2 \US\Lpecialist/;

        # For primary generals
        $routes->get("/$linkTarget/primary/:name" =>
            { generalType => $type, linkTarget => $linkTarget, isPrimary => 1 })
          ->to(controller => $controller_name, action => 'general_buffs')
          ->name("${base}_${type}_primary");

        # For secondary generals
        $routes->get("/$linkTarget/secondary/:name" =>
            { generalType => $type, linkTarget => $linkTarget, isPrimary => 0 })
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
    my $calculate_buffs = $self->param('calculate_buffs') // 0;

    $logger->debug("show detects name $name, showing details.");

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
          rootManager => $self->app->get_root_manager(),
          general     => $general,
          isPrimary   => 1,
          targetType  => $targetType,

        );
        $summarizer->updateBuffs();
        $summarizer->updateDebuffs();

        $self->stash(
        'buff-summaries' => {
          # Buff values
          marchIncrease   => $summarizer->marchIncrease,
          attackIncrease  => $summarizer->attackIncrease,
          defenseIncrease => $summarizer->defenseIncrease,
          hpIncrease      => $summarizer->hpIncrease,

          # Ground troop debuffs
          reducegroundattack  => $summarizer->reducegroundattack,
          reducegrounddefense => $summarizer->reducegrounddefense,
          reducegroundhp      => $summarizer->reducegroundhp,

          # Mounted troop debuffs
          reducemountedattack  => $summarizer->reducemountedattack,
          reducemounteddefense => $summarizer->reducemounteddefense,
          reducemountedhp      => $summarizer->reducemountedhp,

          # Ranged troop debuffs
          reducerangedattack  => $summarizer->reducerangedattack,
          reducerangeddefense => $summarizer->reducerangeddefense,
          reducerangedhp      => $summarizer->reducerangedhp,

          # Siege machine debuffs
          reducesiegeattack  => $summarizer->reducesiegeattack,
          reducesiegedefense => $summarizer->reducesiegedefense,
          reducesiegehp      => $summarizer->reducesiegehp,
        },
      );
      }

      return $self->render(template => $self->details_template);
    }
    $self->SUPER::show();
  }

sub general_buffs ($self) {
    my $logger = Log::Log4perl->get_logger(__PACKAGE__);
    my $name = $self->param('name');
    my $isPrimary = $self->param('isPrimary') // 1;  # Default to primary
    my $targetType = $self->param('targetType') // '';

    $logger->debug("Computing general buffs for $name (isPrimary=$isPrimary, targetType=$targetType)");

    my $general = $self->app->get_root_manager->generalManager->getGeneral($name);

    if (!$general) {
      return $self->render(json => { error => "General not found" }, status => 404);
    }

    # If targetType not provided but general has a type, use that
    if (!$targetType && $general->type) {
      if (ref $general->type eq 'ARRAY') {
        $targetType = $general->type->[0] if @{ $general->type };
      } else {
        $targetType = $general->type;
      }
    }

    my $summarizer = Game::EvonyTKR::Model::Buff::Summarizer->new(
      rootManager    => $self->app->get_root_manager(),
      general        => $general,
      isPrimary      => $isPrimary,
      targetType     => $targetType,
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

    return $self->render(json => $result);
  }

}

1;
