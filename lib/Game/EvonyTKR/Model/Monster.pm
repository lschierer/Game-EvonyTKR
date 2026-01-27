package Game::EvonyTKR::Model::Monster;
use v5.42.0;
use utf8::all;

use Moo;
use experimental qw(signatures);
use Types::Standard qw(Str Int Num HashRef ArrayRef Maybe);

has order => (
  is       => 'ro',
  isa      => Int,
  required => 1,
);

has name => (
  is       => 'ro',
  isa      => Str,
  required => 1,
);

has alt_name => (
  is      => 'ro',
  isa     => Maybe [Str],
  default => sub { undef },
);

has monster_id => (
  is      => 'ro',
  isa     => Maybe [Int],
  default => sub { undef },
);

has level => (
  is       => 'ro',
  isa      => Int,
  required => 1,
);

has stamina => (
  is      => 'ro',
  isa     => Maybe [Int],
  default => sub { 6 },
);

has troop_count => (
  is      => 'ro',
  isa     => Maybe [Int],
  default => sub { undef },
);

has attack => (
  is      => 'ro',
  isa     => Maybe [Int],
  default => sub { 0 },
);

has defense => (
  is      => 'ro',
  isa     => Maybe [Int],
  default => sub { 0 },
);

has hp => (
  is      => 'ro',
  isa     => Maybe [Int],
  default => sub { 0 },
);

has spawn_rates => (
  is      => 'ro',
  isa     => ArrayRef [Int],
  default => sub { [ 0, 0, 0, 0, 0 ] },
);

has troop_modifiers => (
  is      => 'ro',
  isa     => HashRef [Num],
  default => sub {
    {
      ground  => 1.0,
      archer  => 1.0,
      mounted => 1.0,
      siege   => 1.0,
    };
  },
);

has monster_type => (
  is      => 'ro',
  isa     => Str,
  default => sub { 'common' },
);

has rewards => (
  is      => 'ro',
  isa     => HashRef,
  default => sub { {} },
);

sub is_boss ($self) {
  return $self->monster_type eq 'boss';
}

sub is_common ($self) {
  return $self->monster_type eq 'common';
}

sub get_troop_modifier ($self, $troop_type) {
  return $self->troop_modifiers->{lc $troop_type} // 1.0;
}

sub display_name ($self) {
  return sprintf('%s (Lv%d)', $self->name, $self->level);
}

sub to_hash ($self) {
  return {
    order          => $self->order,
    name           => $self->name,
    alt_name       => $self->alt_name,
    monster_id     => $self->monster_id,
    level          => $self->level,
    stamina        => $self->stamina,
    troop_count    => $self->troop_count,
    attack         => $self->attack,
    defense        => $self->defense,
    hp             => $self->hp,
    spawn_rates    => $self->spawn_rates,
    troop_modifiers => $self->troop_modifiers,
    monster_type   => $self->monster_type,
    rewards        => $self->rewards,
  };
}

sub from_hash ($class, $data) {
  return $class->new(
    order          => $data->{order},
    name           => $data->{name},
    alt_name       => $data->{alt_name},
    monster_id     => $data->{monster_id},
    level          => $data->{level},
    stamina        => $data->{stamina},
    troop_count    => $data->{troop_count},
    attack         => $data->{attack},
    defense        => $data->{defense},
    hp             => $data->{hp},
    spawn_rates    => $data->{spawn_rates}    // [ 0, 0, 0, 0, 0 ],
    troop_modifiers => $data->{troop_modifiers} // {},
    monster_type   => $data->{monster_type}   // 'common',
    rewards        => $data->{rewards}        // {},
  );
}

1;
__END__

=head1 NAME

Game::EvonyTKR::Model::Monster - Monster data model

=head1 SYNOPSIS

    my $monster = Game::EvonyTKR::Model::Monster->new(
        order  => 291,
        name   => 'Kraken',
        level  => 20,
        attack => 1234567,
        defense => 234567,
        hp     => 3456789,
        monster_type => 'boss',
    );

    say $monster->display_name;  # "Kraken (Lv20)"
    say $monster->get_troop_modifier('mounted');  # 1.0

=head1 DESCRIPTION

Represents a monster from the game with all its combat stats, spawn rates,
troop type modifiers, and rewards.

=cut
