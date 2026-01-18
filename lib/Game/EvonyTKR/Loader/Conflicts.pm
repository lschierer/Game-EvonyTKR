package Game::EvonyTKR::Loader::Conflicts;
use v5.42.0;
use utf8::all;
use Mooish::Base -standard;
with 'WebFramework::Role::Logger';
with 'Game::EvonyTKR::Role::Common';
use experimental qw(signatures);
use Path::Tiny;
use JSON::PP;

=head1 NAME

Game::EvonyTKR::Loader::Conflicts - Load conflict predictions from JSON

=head1 SYNOPSIS

    my $loader = Game::EvonyTKR::Loader::Conflicts->new(
        data_file => 'conflicts.json'
    );
    $loader->load();

    # Check if two generals conflict
    my $conflicts = $loader->do_generals_conflict('Aethelflaed', 'Caesar');

    # Get all conflicts for a general
    my $conflicts_for = $loader->get_conflicts_for('Aethelflaed');

=head1 DESCRIPTION

Loads ML-predicted conflict data from conflicts.json. This file is generated
by the ML pipeline (train_conflict_model.py + predict_conflicts.py).

The conflict data structure in JSON is:
  {
    "general1_name": {
      "general2_name": { "conflict": true/false, "confidence": 0.95 },
      ...
    },
    ...
  }

=cut

has data_file => (
  is      => 'ro',
  default => sub {'conflicts.json'},
);

has generals_loader => (
  is        => 'ro',
  predicate => 'has_generals_loader',
);

# Raw conflict data from JSON
has raw_conflicts => (
  is      => 'rw',
  default => sub { {} },
);

# Processed conflict lookup: { "norm_name1|norm_name2" => 1/0 }
has conflict_lookup => (
  is      => 'rw',
  default => sub { {} },
);

# By-general index: { "norm_name" => { "other_norm_name" => 1/0, ... } }
has by_general => (
  is      => 'rw',
  default => sub { {} },
);

# Statistics
has stats => (
  is      => 'rw',
  default => sub {
    {
      total_pairs => 0,
      conflicts   => 0,
      compatible  => 0,
      filtered    => 0,
    };
  },
);

sub load ($self) {
  my $file = path($self->data_file);

  unless ($file->exists) {
    $self->logger->warn("Conflicts file not found: $file");
    $self->logger->warn(
"Run the ML pipeline to generate it, or conflicts will use heuristic detection"
    );
    return 0;
  }

  $self->logger->info("Loading conflicts from $file");

  my $json_text = $file->slurp_utf8;
  my $raw_data  = JSON::PP->new->utf8->decode($json_text);

  $self->raw_conflicts($raw_data);

  # Process and index the conflicts
  my $processed = $self->_process_conflicts($raw_data);

  $self->logger->info(sprintf(
"Loaded conflicts: %d total pairs, %d conflicts, %d compatible, %d filtered",
    $self->stats->{total_pairs}, $self->stats->{conflicts},
    $self->stats->{compatible},  $self->stats->{filtered}
  ));

  return $processed;
}

sub _process_conflicts ($self, $raw_data) {
  my %lookup;
  my %by_general;
  my $total      = 0;
  my $conflicts  = 0;
  my $compatible = 0;
  my $filtered   = 0;

  for my $g1_name (keys %$raw_data) {
    my $g1_norm = lc($self->normalize($g1_name));

    # Validate general exists if we have a generals loader
    if ($self->has_generals_loader) {
      unless ($self->generals_loader->get_general($g1_norm)) {
        $filtered++;
        next;
      }
    }

    for my $g2_name (keys %{ $raw_data->{$g1_name} }) {
      my $g2_norm = lc($self->normalize($g2_name));

      # Validate general exists if we have a generals loader
      if ($self->has_generals_loader) {
        unless ($self->generals_loader->get_general($g2_norm)) {
          $filtered++;
          next;
        }
      }

      my $conflict_data = $raw_data->{$g1_name}{$g2_name};
      my $is_conflict   = $conflict_data->{conflict} ? 1 : 0;

      $total++;

      if ($is_conflict) {
        $conflicts++;
      }
      else {
        $compatible++;
      }

      # Create canonical key (sorted names for consistent lookup)
      my ($first, $second) = sort ($g1_norm, $g2_norm);
      my $key = "$first|$second";

      $lookup{$key} = $is_conflict;

      # Index by general for quick "all conflicts for X" lookups
      $by_general{$g1_norm}{$g2_norm} = $is_conflict;
      $by_general{$g2_norm}{$g1_norm} = $is_conflict;
    }
  }

  $self->conflict_lookup(\%lookup);
  $self->by_general(\%by_general);
  $self->stats({
    total_pairs => $total,
    conflicts   => $conflicts,
    compatible  => $compatible,
    filtered    => $filtered,
  });

  return $total;
}

=head2 do_generals_conflict

Check if two generals conflict.

  my $conflicts = $loader->do_generals_conflict('Aethelflaed', 'Caesar');
  # Returns: 1 (conflict), 0 (compatible), undef (unknown)

=cut

sub do_generals_conflict ($self, $name1, $name2) {
  my $norm1 = lc($self->normalize($name1));
  my $norm2 = lc($self->normalize($name2));

  # Create canonical key
  my ($first, $second) = sort ($norm1, $norm2);
  my $key = "$first|$second";

  return $self->conflict_lookup->{$key};
}

=head2 are_generals_compatible

Check if two generals are compatible (inverse of do_generals_conflict).

  my $compatible = $loader->are_generals_compatible('Aethelflaed', 'Caesar');
  # Returns: 1 (compatible), 0 (conflict), undef (unknown)

=cut

sub are_generals_compatible ($self, $name1, $name2) {
  my $result = $self->do_generals_conflict($name1, $name2);
  return undef unless defined $result;
  return $result ? 0 : 1;
}

=head2 get_conflicts_for

Get all conflict information for a specific general.

  my $conflicts = $loader->get_conflicts_for('Aethelflaed');
  # Returns: { 'caesar' => 1, 'trajan' => 0, ... }

=cut

sub get_conflicts_for ($self, $name) {
  my $norm = lc($self->normalize($name));
  return $self->by_general->{$norm} // {};
}

=head2 get_conflicting_generals

Get list of generals that conflict with the given general.

  my @conflicting = $loader->get_conflicting_generals('Aethelflaed');

=cut

sub get_conflicting_generals ($self, $name) {
  my $conflicts = $self->get_conflicts_for($name);
  return grep { $conflicts->{$_} } keys %$conflicts;
}

=head2 get_compatible_generals

Get list of generals that are compatible with the given general.

  my @compatible = $loader->get_compatible_generals('Aethelflaed');

=cut

sub get_compatible_generals ($self, $name) {
  my $conflicts = $self->get_conflicts_for($name);
  return grep { !$conflicts->{$_} } keys %$conflicts;
}

=head2 has_conflict_data

Check if conflict data has been loaded.

=cut

sub has_conflict_data ($self) {
  return scalar(keys %{ $self->conflict_lookup }) > 0;
}

=head2 conflict_count

Get the number of conflict pairs loaded.

=cut

sub conflict_count ($self) {
  return $self->stats->{conflicts};
}

=head2 total_pairs

Get the total number of pairs loaded.

=cut

sub total_pairs ($self) {
  return $self->stats->{total_pairs};
}

1;

__END__

=head1 AUTHOR

Game::EvonyTKR Development Team

=head1 SEE ALSO

L<Game::EvonyTKR::Loader::Generals>, L<bin/extract_conflict_features.pl>

=cut
