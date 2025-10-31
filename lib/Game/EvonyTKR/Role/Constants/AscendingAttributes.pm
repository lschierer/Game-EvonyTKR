use v5.42.0;
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require Hash::Util;

package Game::EvonyTKR::Role::Constants::AscendingAttributes {
  use Mojo::Base -role, -signatures;
  use Const::Fast;
  use Carp;

  sub redAscendingLevelNames {
    const my %tmp => (
      red1 => '1 Red Star',
      red2 => '2 Red Stars',
      red3 => '3 Red Stars',
      red4 => '4 Red Stars',
      red5 => '5 Red Stars',
    );
    return \%tmp;
  }

  sub purpleAscendingLevelNames {
    const my %tmp => (
      purple1 => '1 Purple Star',
      purple2 => '2 Purple Stars',
      purple3 => '3 Purple Stars',
      purple4 => '4 Purple Stars',
      purple5 => '5 Purple Stars',
    );
    return \%tmp;
  }

  sub AscendingAttributeLevelName ($self, $level) {
    if ($level =~ /red/) {
      if (exists $self->redAscendingLevelNames->{$level}) {
        return $self->redAscendingLevelNames->{$level};
      }
    }
    else {
      if (exists $self->purpleAscendingLevelNames->{$level}) {
        return $self->purpleAscendingLevelNames->{$level};
      }
    }
    return 'None';
  }

  sub AscendingAttributeLevelValues ($self, $isRed = 1) {
    my @result;
    push @result, 'none';
    if ($isRed) {
      push @result, sort(keys $self->redAscendingLevelNames->%*);
    }
    else {
      push @result, sort(keys $self->purpleAscendingLevelNames->%*);
    }
    return @result;
  }

  sub AscendingAttributeLevelNames ($self, $isRed = 1) {
    my @result = ('None');
    my @valid  = $self->AscendingAttributeLevelValues($isRed);
    my $aan    = $isRed ? redAscendingLevelNames : purpleAscendingLevelNames;

    foreach my $index (1 .. 5) {
      my $key = $valid[$index];
      push @result, $aan->{$key};
    }

    return @result;
  }
}
1;
__END__
