use v5.42.0;
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require X500::DN;
require X500::RDN;
require Hash::Util;

package Game::EvonyTKR::Role::Constants::GeneralConstants {
  use Moo::Role;
  use Const::Fast;
  use Carp;
  use UUID qw(uuid5);
  use List::AllUtils qw(none all any);

  const our %generalKeys => (
    ground_specialist  => 'Infantry Specialists',
    mounted_specialist => 'Cavalry Specialists',
    ranged_specialist  => 'Archer Specialists',
    siege_specialist   => 'Siege Specialists',
    mayor              => 'Mayor Specialists',
    officer            => 'Officer Specialists',
    wall               => 'Wall Specialists',
  );

  has GeneralKeys => (
    is => 'ro',
    lazy => 1,
    default => sub {
      my ($self) = @_;
      my @gk;
      push @gk, sort keys %generalKeys;
      $self->log_debug(sprintf('there are %s keys from generalKeys', scalar @gk));
      return \@gk;
    }
  );

  sub ValidateGeneralType ($self, $tt) {
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
      if (none { $_ eq $tt } $self->GeneralKeys->@*) {
        $self->log_error(sprintf(
          'General Type must be one of %s, not %s',
          join(', ', $self->GeneralKeys->@*), $tt
        ));
        return 0;
      }
      return 1;
    }
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

  has UUID5_Generals => (
    is => 'ro',
    lazy => 1,
    default => sub {
      my ($self) = @_;
      my $uuids = {};
      my $UUID5_Generals_base = uuid5($self->UUID5_base, 'Generals');
      foreach my $gk (@{ $self->GeneralKeys }) {
        $uuids->{$gk} = uuid5($UUID5_Generals_base, $gk);
      }
      return $uuids;
    }
  );

}
1;
__END__
