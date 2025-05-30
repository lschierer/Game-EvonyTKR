use v5.40.0;
use experimental qw(class);

class Game::EvonyTKR::Model::General::ConflictGroup::Manager {
  field $conflict_groups : private         = {};
  field $general_to_groups_index : private = {};

  method load_conflict_groups($collection_path) {
    # Load and index conflict groups
    # Build general_to_groups_index
  }

  method get_conflict_groups_for_general($general) {
    return $general_to_groups_index->{$general} // [];
  }

  method are_generals_compatible($general1, $general2) {
    # Efficient compatibility check using indexes
  }
}
