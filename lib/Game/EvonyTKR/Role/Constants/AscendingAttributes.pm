use v5.42.0;
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require Hash::Util;

package Game::EvonyTKR::Role::Constants::AscendingAttributes {
  use Mojo::Base -role, -signatures;
  use Const::Fast;
  use Carp;
  use List::AllUtils qw(any);

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

  sub is_valid_level($self, $levelname, $isRed = undef) {
    if (not defined($isRed) || $isRed == 0) {
      if (
        any { $_ =~ /$levelname/i }
        $self->AscendingAttributeLevelNames()
      ) {
        return 1;
      }
    }
    elsif (not defined($isRed) || $isRed == 1) {
      if (
        any { $_ =~ /$levelname/i }
        $self->AscendingAttributeLevelNames(1)
      ) {
        return 1;
      }
    }

    return 0;
  }

  sub can_afford_ascending_level($self, $requestedLevel) {
    my @valv;
    map { push @valv, $_ } $self->AscendingAttributeLevelValues();
    map { push @valv, $_ } $self->AscendingAttributeLevelValues(0);
    if (any { $requestedLevel eq $_ } @valv) {
      my %ranks = (
        none    => 0,
        purple1 => 1,
        purple2 => 2,
        purple3 => 3,
        purple4 => 4,
        purple5 => 5,
        red1    => 6,
        red2    => 7,
        red3    => 8,
        red4    => 9,
        red5    => 10
      );
      my $mr = $ranks{ $self->stars };
      my $rr = $ranks{$requestedLevel};
      return $rr <= $mr;
    }
    return 0;
  }
}
1;
__END__
