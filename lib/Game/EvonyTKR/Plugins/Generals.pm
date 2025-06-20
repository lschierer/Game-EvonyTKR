use v5.40.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Game::EvonyTKR::Model::General;
require Game::EvonyTKR::Model::General::Manager;
require Game::EvonyTKR::Model::Buff::Summarizer;
use namespace::clean;

package Game::EvonyTKR::Plugins::Generals {
  use Mojo::Base 'Game::EvonyTKR::Plugins::CollectionBase';

  # Specify which collection this controller handles
  sub collection_name {'generals'}

  # Override loadItem to add any generals-specific processing

  sub register($self, $app, $config = {}) {
    my $logger = Log::Log4perl->get_logger(__PACKAGE__);
    $logger->info("Registering routes for " . ref($self));
    $self->SUPER::register($app, $config);

    $app->helper(
      get_general_manager => sub {
        return $self->app->get_root_manager->generalManager;
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
            marchIncrease      => $summarizer->marchIncrease,
            attackIncrease     => $summarizer->attackIncrease,
            defenseIncrease    => $summarizer->defenseIncrease,
            hpIncrease         => $summarizer->hpIncrease,
            reducegroundattack => $summarizer->reducegroundattack,
            reducegroundhp     => $summarizer->reducegroundhp,
          },
        );
      }

      return $self->render(template => $self->details_template);
    }
    $self->SUPER::show();
  }
}

1;
