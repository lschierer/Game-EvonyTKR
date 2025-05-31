use v5.40.0;
use experimental qw(class);
use utf8::all;
require Data::Printer;

class Game::EvonyTKR::Model::General::ConflictGroup :
  isa(Game::EvonyTKR::Model::Data) {
  field $id : param : reader;
  field $name : param : reader;
  field $primary_generals : param = [];
  field $linked_groups : param    = [];
  field $expanded_conflicts       = {};
  field $other_conflicts          = {};

  method primary_generals() {
    return [@$primary_generals];    # Return a copy to prevent modification
  }

  method linked_groups() {
    return [@$linked_groups];       # Return a copy to prevent modification
  }

  method get_other_conflicts() {
    my %copy = %{$other_conflicts};
    Hash::Util::lock_hash(%copy);    # prevents adding/deleting top-level keys
    $self->logger->debug(
      "get_other_conflicts will return " . Data::Printer::np(%copy));
    return \%copy;
  }

  method normalize_name ($raw_name, $original = '') {

    # Remove square bracket metadata completely
    $raw_name =~ s/\s*\[[^\]]+\]\s*//g;

    # Replace (content) with just the content — keep the word "Prime" etc.
    $raw_name =~ s/\(([^)]+)\)/ $1/g;

    # Normalize whitespace: collapse multiple spaces and trim
    $raw_name =~ s/^\s+|\s+$//g;
    $raw_name =~ s/\s{2,}/ /g;

    my $name = $raw_name;
    if (length $original) {

      $self->logger->debug("Normalized '$original' → '$name'")
        if $name ne $original;
    }
    return $name;
  }

  method build_expanded_conflicts($all_groups) {

    # Index your own primary generals
    my %is_primary = map { $self->normalize_name($_) => 1 } @$primary_generals;
    $self->logger->debug("build_expanded_conflicts for $name detects primarys "
        . Data::Printer::np(%is_primary));

    # Add all of your own to expanded
    $expanded_conflicts->{ $self->normalize_name($_) } = 1 for keys %is_primary;

    $self->logger->debug(
      "linked groups for $name are " . Data::Printer::np($linked_groups));
    # Traverse linked groups
    for my $linked_id (@$linked_groups) {
      my $linked = $all_groups->{"$linked_id"};
      $self->logger->debug(
        sprintf('traversing %s / %s', $linked->name, $linked->id));
      next unless $linked;

      for my $general (@{ $linked->primary_generals }) {
        $self->logger->debug(
          sprintf(
            'linked group %s adding %s to %s',
            $linked->name, $general, $self->name
          )
        );
        my $norm = $self->normalize_name($general);
        $expanded_conflicts->{$norm} = 1;
        $other_conflicts->{$norm}    = 1 unless $is_primary{$norm};
      }
    }
    $self->logger->debug(
      "other_conflicts is " . Data::Printer::np($other_conflicts));
    return $self;
  }

  method is_compatible($general1, $general2) {
    # Check if either general conflicts with the other
    return 0
      if exists $expanded_conflicts->{$general1}
      && exists $expanded_conflicts->{$general2};

    return 1;
  }

  method contains_general($general) {
    return exists $expanded_conflicts->{$general};
  }
}
1;
