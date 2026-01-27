package Game::EvonyTKR::Loader::MonsterSimulatorData;
use v5.42.0;
use utf8::all;
use Moo;
use experimental qw(signatures);
use Path::Tiny;
use YAML::PP;

with 'Game::EvonyTKR::Role::Constants::MonsterConstants';

# Simple logging that works standalone or delegates to logger role if available
sub _log ($self, $level, $msg) {
  if ($self->can('logger') && $self->logger) {
    $self->logger->$level($msg);
  }
}

has data_file => (
  is       => 'ro',
  required => 1,
);

# Reference table data
has world_boss_modifiers => (
  is      => 'rw',
  default => sub { {} },
);

has tier_modifiers_vs_boss => (
  is      => 'rw',
  default => sub { {} },
);

has alliance_boss_modifiers => (
  is      => 'rw',
  default => sub { {} },
);

has troop_base_attack => (
  is      => 'rw',
  default => sub { {} },
);

has troop_base_defense => (
  is      => 'rw',
  default => sub { {} },
);

has troop_base_hp => (
  is      => 'rw',
  default => sub { {} },
);

sub load_all ($self) {
  my $file = path($self->data_file);
  unless ($file->exists) {
    warn "Reference tables file not found: $file";
    return 0;
  }

  my $data = YAML::PP->new(
    schema       => [qw/ + Perl /],
    yaml_version => [ '1.2', '1.1' ],
  )->load_string($file->slurp_utf8);

  $self->world_boss_modifiers($data->{world_boss_modifiers}     // {});
  $self->tier_modifiers_vs_boss($data->{tier_modifiers_vs_boss} // {});
  $self->alliance_boss_modifiers($data->{alliance_boss_modifiers} // {});
  $self->troop_base_attack($data->{troop_base_attack}           // {});
  $self->troop_base_defense($data->{troop_base_defense}         // {});
  $self->troop_base_hp($data->{troop_base_hp}                   // {});

  $self->_log('info',
    sprintf(
      'Loaded reference tables: %d tiers, %d world bosses',
      scalar(keys $self->troop_base_attack->%*),
      scalar(keys $self->world_boss_modifiers->%*)
    )
  );

  return 1;
}

# Get base attack for a tier and troop type
sub get_base_attack ($self, $tier, $troop_type) {
  return $self->troop_base_attack->{$tier}{lc $troop_type} // 0;
}

# Get base defense for a tier and troop type
sub get_base_defense ($self, $tier, $troop_type) {
  return $self->troop_base_defense->{$tier}{lc $troop_type} // 0;
}

# Get base HP for a tier and troop type
sub get_base_hp ($self, $tier, $troop_type) {
  return $self->troop_base_hp->{$tier}{lc $troop_type} // 0;
}

# Get tier modifier vs boss monsters
sub get_tier_modifier_vs_boss ($self, $tier, $troop_type) {
  return $self->tier_modifiers_vs_boss->{$tier}{lc $troop_type} // 1.0;
}

# Get alliance boss modifier
sub get_alliance_boss_modifier ($self, $tier, $troop_type) {
  return $self->alliance_boss_modifiers->{$tier}{lc $troop_type} // 1.0;
}

# Get world boss modifier by boss name
sub get_world_boss_modifier ($self, $boss_name, $troop_type) {
  return $self->world_boss_modifiers->{$boss_name}{lc $troop_type} // 1.0;
}

# Get world boss modifier by order number
sub get_world_boss_modifier_by_order ($self, $order, $troop_type) {
  my $boss_name = $self->WorldBossOrders->{$order};
  return 1.0 unless $boss_name;
  return $self->get_world_boss_modifier($boss_name, $troop_type);
}

# Get all base stats for a tier and troop type
sub get_base_stats ($self, $tier, $troop_type) {
  return {
    attack  => $self->get_base_attack($tier, $troop_type),
    defense => $self->get_base_defense($tier, $troop_type),
    hp      => $self->get_base_hp($tier, $troop_type),
  };
}

1;
__END__

=head1 NAME

Game::EvonyTKR::Loader::MonsterSimulatorData - Load reference tables for monster simulator

=head1 SYNOPSIS

    my $loader = Game::EvonyTKR::Loader::MonsterSimulatorData->new(
        data_file => 'share/collections/data/monster_simulator/reference_tables.yaml'
    );
    $loader->load_all();

    my $base_attack = $loader->get_base_attack('T15', 'mounted');
    my $modifier = $loader->get_tier_modifier_vs_boss('T15', 'mounted');

=head1 DESCRIPTION

Loads and provides access to troop base stats and various combat modifiers
used in monster damage calculations.

=cut
