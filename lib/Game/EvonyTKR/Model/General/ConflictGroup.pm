use v5.40.0;
use experimental qw(class);

class Game::EvonyTKR::Model::General::ConflictGroup {
  field $id : param : reader;
  field $name : param : reader;
  field $primary_generals : param     = [];
  field $linked_groups : param        = [];
  field $expanded_conflicts : private = {};

  method primary_generals() {
    return [@$primary_generals];    # Return a copy to prevent modification
  }

  method linked_groups() {
    return [@$linked_groups];       # Return a copy to prevent modification
  }

  method build_expanded_conflicts($all_groups) {
    # Clear existing cache
    $expanded_conflicts = {};

    # Add primary generals to the conflict list
    foreach my $general (@$primary_generals) {
      $expanded_conflicts->{$general} = 1;
    }

    # Process linked groups
    foreach my $linked_id (@$linked_groups) {
      my $linked_group = $all_groups->{$linked_id};
      next unless $linked_group;

      # Add all primary generals from linked group
      foreach my $general (@{ $linked_group->primary_generals }) {
        $expanded_conflicts->{$general} = 1;
      }
    }

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
