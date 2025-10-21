use v5.42.0;
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require Hash::Util;

package Game::EvonyTKR::Shared::Constants::AscendingAttributes {
  use Mojo::Base -role, -signatures;
  use Const::Fast;
  use Carp;

  const our %redAscendingLevelNames => (
    red1 => '1 Red Star',
    red2 => '2 Red Stars',
    red3 => '3 Red Stars',
    red4 => '4 Red Stars',
    red5 => '5 Red Stars',
  );

  const our %purpleAscendingLevelNames => (
    purple1 => '1 Purple Star',
    purple2 => '2 Purple Stars',
    purple3 => '3 Purple Stars',
    purple4 => '4 Purple Stars',
    purple5 => '5 Purple Stars',
  );

  sub AscendingAttributeLevelName ($self, $level) {
    if ($level =~ /red/) {
      if (exists $redAscendingLevelNames{$level}) {
        return $redAscendingLevelNames{$level};
      }
    }
    else {
      if (exists $purpleAscendingLevelNames{$level}) {
        return $purpleAscendingLevelNames{$level};
      }
    }
    return 'None';
  }

  sub AscendingAttributeLevelValues ($self, $isRed = 1) {
    my @result;
    push @result, 'none';
    if ($isRed) {
      push @result, sort(keys %redAscendingLevelNames);
    }
    else {
      push @result, sort(keys %purpleAscendingLevelNames);
    }
    return @result;
  }

  sub AscendingAttributeLevelNames ($self, $isRed = 1) {
    my @result = ('None');
    my @valid  = $self->AscendingAttributeLevelValues($isRed);
    my $aan = $isRed ? \%redAscendingLevelNames : \%purpleAscendingLevelNames;

    foreach my $index (1 .. 5) {
      my $key = $valid[$index];
      push @result, $aan->{$key};
    }

    return @result;
  }
}
1;
__END__
