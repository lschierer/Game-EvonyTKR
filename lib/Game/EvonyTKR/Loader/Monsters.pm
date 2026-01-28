package Game::EvonyTKR::Loader::Monsters;
use v5.42.0;
use utf8::all;
use Moo;
use experimental qw(signatures);
use Path::Tiny;
use YAML::PP;

with 'Game::EvonyTKR::Role::Common';
with 'Game::EvonyTKR::Role::Constants::MonsterConstants';
with 'WebFramework::Role::Logger';

require Game::EvonyTKR::Model::Monster;

has data_file => (
  is       => 'ro',
  required => 1,
);

has monsters => (
  is      => 'rw',
  default => sub { {} },
);

has monsters_by_name => (
  is      => 'rw',
  default => sub { {} },
);

has monsters_by_level => (
  is      => 'rw',
  default => sub { {} },
);

sub load_all ($self) {
  my $file = path($self->data_file);
  unless ($file->exists) {
    warn "Monsters data file not found: $file";
    return 0;
  }

  my $data = YAML::PP->new(
    schema       => [qw/ + Perl /],
    yaml_version => ['1.2', '1.1'],
  )->load_string($file->slurp_utf8);

  my $monsters_data = $data->{monsters} // [];
  my $loaded        = 0;

  for my $monster_data ($monsters_data->@*) {
    eval {
      my $monster = Game::EvonyTKR::Model::Monster->from_hash($monster_data);
      if ($monster) {
        # Index by order number (primary key)
        $self->monsters->{ $monster->order } = $monster;

        # Index by normalized name + level for searching
        my $name_key = $self->normalize($monster->name) . '_' . $monster->level;
        $self->monsters_by_name->{$name_key} = $monster;

        # Index by level for filtering
        push $self->monsters_by_level->{ $monster->level }->@*, $monster;

        $loaded++;
      }
    };
    if ($@) {
      warn "Failed to load monster: $@";
    }
  }

  $self->logger->info("Loaded $loaded monsters");
  return $loaded;
}

sub get_by_order ($self, $order) {
  return $self->monsters->{$order};
}

sub get_by_name_and_level ($self, $name, $level) {
  my $key = $self->normalize($name) . '_' . $level;
  return $self->monsters_by_name->{$key};
}

sub get_by_level ($self, $level) {
  return $self->monsters_by_level->{$level} // [];
}

sub search ($self, $query) {
  my $normalized = $self->normalize($query);
  my @results;

  for my $monster (values $self->monsters->%*) {
    my $name_norm = $self->normalize($monster->name);
    if ($name_norm =~ /\Q$normalized\E/i) {
      push @results, $monster;
    }
  }

  # Sort by level, then name
  @results = sort { $a->name cmp $b->name || $a->level <=> $b->level } @results;

  return \@results;
}

sub list_all ($self) {
  return [sort { $a->order <=> $b->order } values $self->monsters->%*];
}

sub list_boss_monsters ($self) {
  return [
    sort { $a->order <=> $b->order }
    grep { $_->is_boss } values $self->monsters->%*
  ];
}

sub list_common_monsters ($self) {
  return [
    sort { $a->order <=> $b->order }
    grep { $_->is_common } values $self->monsters->%*
  ];
}

sub list_unique_names ($self) {
  my %seen;
  my @names;
  for my $monster (sort { $a->order <=> $b->order } values $self->monsters->%*)
  {
    unless ($seen{ $monster->name }++) {
      push @names, $monster->name;
    }
  }
  return \@names;
}

sub get_levels_for_name ($self, $name) {
  my $normalized = $self->normalize($name);
  my @levels;

  for my $monster (values $self->monsters->%*) {
    if ($self->normalize($monster->name) eq $normalized) {
      push @levels, $monster->level;
    }
  }

  return [sort { $a <=> $b } @levels];
}

1;
__END__

=head1 NAME

Game::EvonyTKR::Loader::Monsters - Load monster data from YAML

=head1 SYNOPSIS

    my $loader = Game::EvonyTKR::Loader::Monsters->new(
        data_file => 'share/collections/data/monsters/monsters.yaml'
    );
    $loader->load_all();

    my $kraken = $loader->get_by_order(291);
    my $results = $loader->search('Dragon');
    my $bosses = $loader->list_boss_monsters();

=head1 DESCRIPTION

Loads monster data from YAML file and provides lookup methods by order number,
name, level, and type.

=cut
