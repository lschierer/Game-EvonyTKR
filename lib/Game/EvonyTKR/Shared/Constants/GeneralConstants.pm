use v5.42.0;
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require X500::DN;
require X500::RDN;
require Hash::Util;

package Game::EvonyTKR::Shared::Constants::GeneralConstants {
  use Mojo::Base 'Game::EvonyTKR::Util::Common', -base, -signatures;
  use Const::Fast;
  use Carp;
  use UUID qw(uuid5);

  const our %generalKeys => (
    ground_specialist  => 1,
    mounted_specialist => 1,
    ranged_specialist  => 1,
    siege_specialist   => 1,
    mayor              => 1,
    officer            => 1,
    wall               => 1,
  );

  sub GeneralKeys {
    return sort keys %generalKeys;
  }

  const our %GeneralTypes2TroopTypes => (
    ground_specialist  => 'Ground Troops',
    mounted_specialist => 'Mounted Troops',
    ranged_specialist  => 'Ranged Troops',
    siege_specialist   => 'Siege Machines',
    mayor              => 'ALL',
    officer            => 'ALL',
    wall               => 'ALL',
  );

  has 'UUID5_Generals' => sub {
    my $self = shift;
    state $uuids = {};

    unless (scalar keys %$uuids > 0) {
      my $UUID5_Generals_base = uuid5($self->UUID5_base, 'Generals');
      foreach my $gk (@{ $self->GeneralKeys }) {
        $uuids->{$gk} = uuid5($UUID5_Generals_base, $gk);
      }
    }

    return $uuids;
  };

}
1;
__END__
