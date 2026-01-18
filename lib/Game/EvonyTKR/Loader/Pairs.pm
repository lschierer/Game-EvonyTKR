package Game::EvonyTKR::Loader::Pairs;
use v5.42.0;
use utf8::all;
use Mooish::Base -standard;
with 'WebFramework::Role::Logger';
with 'Game::EvonyTKR::Role::Common';
use experimental qw(signatures);

use Game::EvonyTKR::Role::Constants::GeneralConstants;

=head1 NAME

Game::EvonyTKR::Loader::Pairs - Generate valid general pairs at startup

=head1 SYNOPSIS

    my $loader = Game::EvonyTKR::Loader::Pairs->new(
        generals_loader  => $generals_loader,
        conflicts_loader => $conflicts_loader,
    );
    my $count = $loader->load_all();

    # Get all pairs for a general type
    my $pairs = $loader->get_pairs_for_type('ground_specialist');

=head1 DESCRIPTION

Generates all valid general pairs at application startup by:
1. Getting all generals for each type (matched by specialty)
2. Creating all possible pairs
3. Filtering out pairs where generals conflict (using ML predictions)

=cut

has generals_loader => (
  is       => 'ro',
  required => 1,
);

has conflicts_loader => (
  is       => 'ro',
  required => 1,
);

# Pairs indexed by type: { 'ground_specialist' => [ pair_hash, ... ] }
has pairs_by_type => (
  is      => 'rw',
  default => sub { {} },
);

# All pairs indexed by key: { 'type/primary/secondary' => pair_hash }
has pairs_by_key => (
  is      => 'rw',
  default => sub { {} },
);

# Statistics
has stats => (
  is      => 'rw',
  default => sub {
    {
      total_pairs     => 0,
      pairs_by_type   => {},
      conflicts_found => 0,
    };
  },
);

# Type mappings from GeneralConstants
my %type_to_troop = (
  ground_specialist  => 'Ground Troops',
  mounted_specialist => 'Mounted Troops',
  ranged_specialist  => 'Ranged Troops',
  siege_specialist   => 'Siege Machines',
  mayor              => 'ALL',
  officer            => 'ALL',
  wall               => 'ALL',
);

my @general_types = keys %type_to_troop;

sub load_all ($self) {
  $self->logger->info("Generating pairs from generals and conflict data...");

  my %pairs_by_type;
  my %pairs_by_key;
  my $total_pairs     = 0;
  my $conflicts_found = 0;
  my %type_counts;

  # Get all generals
  my @all_generals;
  foreach my $key ($self->generals_loader->list_generals->@*) {
    my $general = $self->generals_loader->get_general($key);
    next unless $general;
    push @all_generals, $general;
  }

  $self->logger->info(
    sprintf("Found %d generals to pair", scalar(@all_generals)));

  # Process each general type
  foreach my $type (@general_types) {
    my $troop_type = $type_to_troop{$type};

   # Get generals matching this type
   # Pass both the type key (e.g., 'mayor') and troop_type mapping (e.g., 'ALL')
    my @matching_generals =
      $self->_get_generals_for_type(\@all_generals, $type, $troop_type);

    $self->logger->debug(sprintf(
      "Found %d generals for type %s",
      scalar(@matching_generals), $type
    ));

    my @type_pairs;
    my $type_conflicts = 0;

    # Generate all pairs (avoiding duplicates)
    for my $i (0 .. $#matching_generals) {
      my $primary = $matching_generals[$i];

      for my $j ($i + 1 .. $#matching_generals) {
        my $secondary = $matching_generals[$j];

        # Check for conflicts using ML predictions
        my $conflicts =
          $self->conflicts_loader->do_generals_conflict($primary->name,
          $secondary->name);

        if ($conflicts) {
          $type_conflicts++;
          $self->logger->debug(sprintf(
            "Skipping conflicting pair: %s <-> %s",
            $primary->name, $secondary->name
          ));
          next;
        }

        # Create bidirectional pairs (A->B and B->A)
        my $pair_ab = $self->_create_pair_hash($primary,   $secondary, $type);
        my $pair_ba = $self->_create_pair_hash($secondary, $primary,   $type);

        push @type_pairs, $pair_ab, $pair_ba;

        # Index by key
        my $key_ab = $self->_pair_key($type, $primary->name, $secondary->name);
        my $key_ba = $self->_pair_key($type, $secondary->name, $primary->name);
        $pairs_by_key{$key_ab} = $pair_ab;
        $pairs_by_key{$key_ba} = $pair_ba;
      }
    }

    $pairs_by_type{$type} = \@type_pairs;
    $type_counts{$type}   = scalar(@type_pairs);
    $total_pairs     += scalar(@type_pairs);
    $conflicts_found += $type_conflicts;

    $self->logger->info(sprintf(
      "Type %s: %d pairs, %d conflicts filtered",
      $type, scalar(@type_pairs), $type_conflicts
    ));
  }

  $self->pairs_by_type(\%pairs_by_type);
  $self->pairs_by_key(\%pairs_by_key);
  $self->stats({
    total_pairs     => $total_pairs,
    pairs_by_type   => \%type_counts,
    conflicts_found => $conflicts_found,
  });

  $self->logger->info(sprintf(
    "Loaded %d pairs total (%d conflicts filtered)",
    $total_pairs, $conflicts_found
  ));

  return $total_pairs;
}

# Pass both the loader type key and the troop_type mapping
sub _get_generals_for_type ($self, $all_generals, $type_key, $troop_type) {
  my @matching;

  foreach my $general (@$all_generals) {
    # For 'ALL' types (mayor, officer, wall), check the general's type field
    if ($troop_type eq 'ALL') {
      my $general_types = $general->type // [];
      $general_types = [$general_types] unless ref($general_types) eq 'ARRAY';

  # Check if the general's type array contains this type (e.g., 'mayor', 'wall')
      if (grep { lc($_) eq lc($type_key) } @$general_types) {
        push @matching, $general;
      }
      next;
    }

    # Filter by matching specialty for troop types
    my $specialty_names = $general->specialtyNames // [];
    my $search_term     = $troop_type;
    $search_term =~ s/s$//;    # "Mounted Troops" -> "Mounted Troop"

    for my $specialty (@$specialty_names) {
      if ($specialty =~ /$search_term/i) {
        push @matching, $general;
        last;
      }
    }
  }

  return @matching;
}

sub _create_pair_hash ($self, $primary, $secondary, $type) {
  return {
    type      => $type,
    primary   => { name => $primary->name },
    secondary => { name => $secondary->name },
  };
}

sub _pair_key ($self, $type, $primary_name, $secondary_name) {
  return sprintf('%s/%s/%s',
    $type,
    lc($self->normalize($primary_name)),
    lc($self->normalize($secondary_name)),
  );
}

=head2 get_pairs_for_type

Get all pairs for a specific general type.

  my $pairs = $loader->get_pairs_for_type('ground_specialist');

Returns arrayref of pair hashes.

=cut

sub get_pairs_for_type ($self, $type) {
  return $self->pairs_by_type->{$type} // [];
}

=head2 get_pair

Get a specific pair by key.

  my $pair = $loader->get_pair('ground_specialist/caesar/aethelflaed');

=cut

sub get_pair ($self, $key) {
  return $self->pairs_by_key->{$key};
}

=head2 list_types

Get list of available pair types.

=cut

sub list_types ($self) {
  return [sort keys %{ $self->pairs_by_type }];
}

=head2 pair_count

Get total number of pairs loaded.

=cut

sub pair_count ($self) {
  return $self->stats->{total_pairs};
}

=head2 pair_count_for_type

Get number of pairs for a specific type.

=cut

sub pair_count_for_type ($self, $type) {
  return $self->stats->{pairs_by_type}{$type} // 0;
}

1;

__END__

=head1 AUTHOR

Game::EvonyTKR Development Team

=head1 SEE ALSO

L<Game::EvonyTKR::Loader::Generals>, L<Game::EvonyTKR::Loader::Conflicts>

=cut
