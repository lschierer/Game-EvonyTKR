use v5.42.0;
use utf8::all;

package Game::EvonyTKR::Role::Constants::MonsterConstants {
  use Moo::Role;
  use Const::Fast;

  has TierValues => (
    is      => 'ro',
    default => sub {
      const my $array => [
        'T1',  'T2',  'T3',  'T4',  'T5',  'T6',  'T7', 'T8', 'T9', 'T10',
        'T11', 'T12', 'T13', 'T14', 'T15', 'T16', 'T17'
      ];
      return $array;
    },
  );

  has TierIndex => (
    is      => 'ro',
    lazy    => 1,
    default => sub ($self) {
      my %index;
      my $i = 0;
      for my $tier ($self->TierValues->@*) {
        $index{$tier} = $i++;
      }
      return \%index;
    },
  );

  has MonsterTroopTypes => (
    is      => 'ro',
    default => sub {
      const my $array => [qw(ground archer mounted siege)];
      return $array;
    },
  );

  has MonsterTroopTypeLabels => (
    is      => 'ro',
    default => sub {
      const my $hash => {
        ground  => 'Ground',
        archer  => 'Archer',
        mounted => 'Mounted',
        siege   => 'Siege',
      };
      return $hash;
    },
  );

  has MonsterTypes => (
    is      => 'ro',
    default => sub {
      const my $hash => {
        boss       => 'Boss (can rally)',
        common     => 'Common (cannot rally)',
        summon     => 'Summon',
        collection => 'Collection',
        resource   => 'Resource',
        pyramid    => 'Pyramid',
      };
      return $hash;
    },
  );

  has MarchTypes => (
    is      => 'ro',
    default => sub {
      const my $array => [qw(solo rally)];
      return $array;
    },
  );

  has MarchTypeLabels => (
    is      => 'ro',
    default => sub {
      const my $hash => {
        solo  => 'Solo March',
        rally => 'Rally',
      };
      return $hash;
    },
  );

  # World boss order numbers for special handling
  has WorldBossOrders => (
    is      => 'ro',
    default => sub {
      const my $hash => {
        321 => 'Lord of Lava',
        322 => 'Thunder Scorpion',
        323 => 'Behemoth King',
        324 => 'Bird of Hurricane',
      };
      return $hash;
    },
  );

  # Alliance boss order numbers (693-695 based on formulas)
  has AllianceBossOrders => (
    is      => 'ro',
    default => sub {
      const my $array => [693, 694, 695];
      return $array;
    },
  );

  sub is_world_boss ($self, $order) {
    return exists $self->WorldBossOrders->{$order};
  }

  sub is_alliance_boss ($self, $order) {
    return grep { $_ == $order } $self->AllianceBossOrders->@*;
  }

  sub tier_to_number ($self, $tier) {
    if ($tier =~ /^T(\d+)$/i) {
      return $1;
    }
    return 0;
  }

  sub number_to_tier ($self, $num) {
    return "T$num" if $num >= 1 && $num <= 17;
    return undef;
  }
}
1;
__END__

=head1 NAME

Game::EvonyTKR::Role::Constants::MonsterConstants - Constants for monster simulator

=head1 DESCRIPTION

Provides constant values for tiers, troop types, monster types, and special
monster classifications (world bosses, alliance bosses).

=cut
