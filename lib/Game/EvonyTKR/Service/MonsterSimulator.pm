package Game::EvonyTKR::Service::MonsterSimulator;
use v5.42.0;
use utf8::all;
use Mooish::Base -standard;
with 'WebFramework::Role::Logger';
with 'Game::EvonyTKR::Role::Common';

use experimental qw(signatures);

with 'Game::EvonyTKR::Role::Constants::MonsterConstants';

has monsters_loader => (
  is       => 'ro',
  required => 1,
);

has reference_data => (
  is       => 'ro',
  required => 1,
);

# Input parameters for a simulation
# Returns a result hash with calculated values

sub simulate ($self, $params) {
  my $monster_order = $params->{monster_order};
  my $tier          = $params->{tier} // 'T15';
  my $troop_type    = lc($params->{troop_type} // 'mounted');
  my $march_type    = lc($params->{march_type} // 'solo');
  my $troop_count   = $params->{troop_count} // 500_000;

  # Buffs (percentages as decimals, e.g., 0.5 = 50%)
  my $buffs           = $params->{buffs} // {};
  my $attack_buff_pct = {
    basic   => $buffs->{attack}{basic}   // 0,
    march   => $buffs->{attack}{march}   // 0,
    monster => $buffs->{attack}{monster} // 0,
    misc    => $buffs->{attack}{misc}    // 0,
    rally   => $buffs->{attack}{rally}   // 0,
  };
  my $defense_buff_pct = {
    basic   => $buffs->{defense}{basic}   // 0,
    march   => $buffs->{defense}{march}   // 0,
    monster => $buffs->{defense}{monster} // 0,
    misc    => $buffs->{defense}{misc}    // 0,
    rally   => $buffs->{defense}{rally}   // 0,
  };
  my $hp_buff_pct = {
    basic   => $buffs->{hp}{basic}   // 0,
    march   => $buffs->{hp}{march}   // 0,
    monster => $buffs->{hp}{monster} // 0,
    misc    => $buffs->{hp}{misc}    // 0,
    rally   => $buffs->{hp}{rally}   // 0,
  };

  # Flat buffs
  my $attack_buff_flat  = $buffs->{attack}{flat}  // 0;
  my $defense_buff_flat = $buffs->{defense}{flat} // 0;
  my $hp_buff_flat      = $buffs->{hp}{flat}      // 0;

  # Debuffs applied to monster
  my $debuffs                = $params->{debuffs}          // {};
  my $monster_attack_debuff  = $debuffs->{monster_attack}  // 0;
  my $monster_defense_debuff = $debuffs->{monster_defense} // 0;

  # Troop debuff (enemy reduces your stats)
  my $troop_attack_debuff  = $debuffs->{troop_attack}  // 0;
  my $troop_defense_debuff = $debuffs->{troop_defense} // 0;
  my $troop_hp_debuff      = $debuffs->{troop_hp}      // 0;

  # Get monster
  my $monster = $self->monsters_loader->get_by_order($monster_order);
  unless ($monster) {
    return { error => "Monster not found: order $monster_order" };
  }

  # Calculate total buff percentages based on march type
  my $total_attack_buff_pct;
  my $total_defense_buff_pct;
  my $total_hp_buff_pct;

  if ($march_type eq 'rally') {
    # Rally includes rally buffs
    $total_attack_buff_pct =
      $attack_buff_pct->{basic} +
      $attack_buff_pct->{march} +
      $attack_buff_pct->{monster} +
      $attack_buff_pct->{misc} +
      $attack_buff_pct->{rally};
    $total_defense_buff_pct =
      $defense_buff_pct->{basic} +
      $defense_buff_pct->{march} +
      $defense_buff_pct->{monster} +
      $defense_buff_pct->{misc} +
      $defense_buff_pct->{rally};
    $total_hp_buff_pct =
      $hp_buff_pct->{basic} +
      $hp_buff_pct->{march} +
      $hp_buff_pct->{monster} +
      $hp_buff_pct->{misc} +
      $hp_buff_pct->{rally};
  }
  else {
    # Solo march - no rally buffs
    $total_attack_buff_pct =
      $attack_buff_pct->{basic} +
      $attack_buff_pct->{march} +
      $attack_buff_pct->{monster} +
      $attack_buff_pct->{misc};
    $total_defense_buff_pct =
      $defense_buff_pct->{basic} +
      $defense_buff_pct->{march} +
      $defense_buff_pct->{monster} +
      $defense_buff_pct->{misc};
    $total_hp_buff_pct =
      $hp_buff_pct->{basic} +
      $hp_buff_pct->{march} +
      $hp_buff_pct->{monster} +
      $hp_buff_pct->{misc};
  }

  # Apply troop debuffs (reduce our buffs)
  $total_attack_buff_pct  -= $troop_attack_debuff;
  $total_defense_buff_pct -= $troop_defense_debuff;
  $total_hp_buff_pct      -= $troop_hp_debuff;

  # Get base stats from reference tables
  my $base_attack = $self->reference_data->get_base_attack($tier, $troop_type);
  my $base_defense =
    $self->reference_data->get_base_defense($tier, $troop_type);
  my $base_hp = $self->reference_data->get_base_hp($tier, $troop_type);

  # Calculate final player stats per troop
  # Formula: base * (1 + buff_pct) + flat_buff
  my $player_attack =
    $base_attack * (1 + $total_attack_buff_pct) + $attack_buff_flat;
  my $player_defense =
    $base_defense * (1 + $total_defense_buff_pct) + $defense_buff_flat;
  my $player_hp = $base_hp * (1 + $total_hp_buff_pct) + $hp_buff_flat;

  # Get troop modifier based on monster type
  my $troop_modifier =
    $self->_calculate_troop_modifier($monster, $tier, $troop_type,
    $params->{alliance_boss_modifier});

  # Calculate monster stats with debuffs
  my $monster_attack  = $monster->attack * (1 - $monster_attack_debuff);
  my $monster_defense = $monster->defense * (1 - $monster_defense_debuff);
  my $monster_hp      = $monster->hp;

  # Calculate damage output
  # This is a simplified damage formula - the actual game formula may vary
  my $damage_per_troop = $player_attack * $troop_modifier;

  # Account for monster defense reducing damage
  my $effective_damage =
    $damage_per_troop * ($player_attack / ($player_attack + $monster_defense));

  # Calculate minimum troops to kill (simplified)
  my $min_troops_to_kill = 0;
  if ($effective_damage > 0 && $monster->troop_count) {
    $min_troops_to_kill =
      int(($monster_hp * $monster->troop_count) / $effective_damage) + 1;
  }

  # Calculate expected wounds (simplified)
  # Wounds depend on monster attack vs player defense/hp
  my $monster_damage_per_troop = $monster_attack / $troop_count
    if $troop_count > 0;
  my $expected_wounds = 0;
  if ($player_hp > 0 && $monster_attack > 0) {
    $expected_wounds =
      int(($monster_attack * $monster->troop_count) / $player_hp);
  }

  return {
    # Input summary
    monster     => $monster->to_hash,
    tier        => $tier,
    troop_type  => $troop_type,
    march_type  => $march_type,
    troop_count => $troop_count,

    # Player final stats (per troop)
    player_stats => {
      attack  => sprintf('%.2f', $player_attack),
      defense => sprintf('%.2f', $player_defense),
      hp      => sprintf('%.2f', $player_hp),
    },

    # Buff totals
    total_buffs => {
      attack_pct  => sprintf('%.2f%%', $total_attack_buff_pct * 100),
      defense_pct => sprintf('%.2f%%', $total_defense_buff_pct * 100),
      hp_pct      => sprintf('%.2f%%', $total_hp_buff_pct * 100),
    },

    # Monster final stats (with debuffs)
    monster_stats => {
      attack  => sprintf('%.0f', $monster_attack),
      defense => sprintf('%.0f', $monster_defense),
      hp      => sprintf('%.0f', $monster_hp),
    },

    # Calculation results
    troop_modifier     => sprintf('%.4f', $troop_modifier),
    damage_per_troop   => sprintf('%.2f', $damage_per_troop),
    effective_damage   => sprintf('%.2f', $effective_damage),
    min_troops_to_kill => $min_troops_to_kill,
    expected_wounds    => $expected_wounds,

    # Uncertainty tracking
    unknowns_count => $params->{unknowns_count} // 0,
  };
}

sub _calculate_troop_modifier ($self, $monster, $tier, $troop_type,
  $alliance_modifier = undef) {
  my $order = $monster->order;

  # Check for world boss (order 321-324)
  if ($self->is_world_boss($order)) {
    return $self->reference_data->get_world_boss_modifier_by_order($order,
      $troop_type);
  }

  # Check for alliance boss (order 693-695)
  if ($self->is_alliance_boss($order)) {
    my $base_mod =
      $self->reference_data->get_alliance_boss_modifier($tier, $troop_type);
    # Alliance boss can have additional modifier based on buff status
    if (defined $alliance_modifier) {
      if ($alliance_modifier eq '20%') {
        $base_mod *= 1.2;
      }
      elsif ($alliance_modifier eq '-20%') {
        $base_mod *= 0.8;
      }
    }
    return $base_mod;
  }

  # Check for common monsters (monster_type = 1 in spreadsheet means common)
  if ($monster->is_common) {
    # Common monsters use flat modifier of 1.0 for T1-T10
    my $tier_num = $self->tier_to_number($tier);
    if ($tier_num <= 10) {
      return 1.0;
    }
    else {
      # T11+ has different modifiers for common monsters
      if ($troop_type eq 'mounted') {
        return 1.1;
      }
      elsif ($troop_type eq 'siege') {
        return 0.5;
      }
      else {
        return 1.0;
      }
    }
  }

  # Boss monsters use tier modifiers
  if ($monster->is_boss) {
    return $self->reference_data->get_tier_modifier_vs_boss($tier, $troop_type);
  }

  # Default: use monster's own troop modifier if available
  return $monster->get_troop_modifier($troop_type);
}

# Helper to calculate "true buff" from combat results
# This is the reverse calculation to figure out actual buff from damage dealt
sub calculate_true_buff ($self, $params) {
  my $damage_dealt = $params->{damage_dealt};
  my $tier         = $params->{tier};
  my $troop_type   = $params->{troop_type};
  my $flat_buff    = $params->{flat_buff} // 0;
  my $troop_count  = $params->{troop_count};

  my $base_attack = $self->reference_data->get_base_attack($tier, $troop_type);

  # Reverse the formula: damage = troops * base * (1 + buff) + flat
  # buff = (damage/troops - flat) / base - 1
  if ($troop_count > 0 && $base_attack > 0) {
    my $damage_per_troop = $damage_dealt / $troop_count;
    my $true_buff        = ($damage_per_troop - $flat_buff) / $base_attack - 1;
    return $true_buff;
  }

  return 0;
}

1;
__END__

=head1 NAME

Game::EvonyTKR::Service::MonsterSimulator - Monster combat simulation calculator

=head1 SYNOPSIS

    my $simulator = Game::EvonyTKR::Service::MonsterSimulator->new(
        monsters_loader => $monsters_loader,
        reference_data  => $reference_data,
    );

    my $result = $simulator->simulate({
        monster_order => 291,  # Kraken Lv20
        tier          => 'T15',
        troop_type    => 'mounted',
        march_type    => 'solo',
        troop_count   => 500_000,
        buffs         => {
            attack => { basic => 0.5, march => 0.2, monster => 0.3 },
            # ...
        },
    });

=head1 DESCRIPTION

Implements the combat simulation formulas from the Monster Simulator spreadsheet.
Calculates player stats, monster stats with debuffs, damage output, minimum troops
to kill, and expected wounds.

=head1 CALCULATION NOTES

The main formulas are:

    final_stat = base_stat * (1 + buff_percent) + flat_buff

Troop modifiers vary by:
- Monster type (boss, common, world boss, alliance boss)
- Player tier (T1-T17)
- Troop type (ground, archer, mounted, siege)

=cut
