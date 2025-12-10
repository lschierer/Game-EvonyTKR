use v5.42.0;
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require X500::DN;
require X500::RDN;
require Hash::Util;

package Game::EvonyTKR::Role::Constants::GeneralConstants {
  use Mojo::Base -role, -signatures;
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

  has 'GeneralKeys' => sub ($self) {
    my @gk;
    push @gk, sort keys %generalKeys;
    $self->log_debug(sprintf('there are %s keys from generalKeys', scalar @gk));
    return \@gk;
  };

  has 'ValidateGeneralType' => sub($self, $tt) {
    if (ref($tt) eq 'ARRAY') {
      my $valid = 0;
      foreach my $stt ($tt->@*) {
        $valid = $self->ValidateGeneralType($stt);
        last if ($valid == 0);
      }
      return $valid;
    }
    elsif (ref($tt)) {
      $self->log_error(sprintf(
        'General Type Must be an Array or a Scalar, not %s', ref($tt)));
      return 0;
    }
    else {
      if (none { $_ eq $tt } $self->GeneralKeys()) {
        $self->log_error(sprintf(
          'General Type must be one of %s, not %s',
          join ', ', $self->GeneralKeys(), $tt
        ));
        return 0;
      }
      return 1;
    }
  };

  const our %GeneralTypes2TroopTypes => (
    ground_specialist  => 'Ground Troops',
    mounted_specialist => 'Mounted Troops',
    ranged_specialist  => 'Ranged Troops',
    siege_specialist   => 'Siege Machines',
    mayor              => 'ALL',
    officer            => 'ALL',
    wall               => 'ALL',
  );

  has 'UUID5_Generals' => sub ($self) {
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
