use v5.42.0;
use utf8::all;
use File::FindLib 'lib';

# lib/Game/EvonyTKR/Model/Factory.pm
package Game::EvonyTKR::Model::Factory {
  use Module::Runtime 'use_module';

# $type like 'Book', 'General', 'Pair', etc.
# $opts - optional hash ref of options to pass to from_wire_hash (e.g., { populateGenericBooks => 0 })
  sub build_from_wire ($class, $type, $wire_hash, $opts = {}) {
    my $pkg = "Game::EvonyTKR::Model::$type";
    use_module($pkg);    # runtime load

    # Only pass $opts if it's not empty (some models don't accept extra params)
    if (%$opts) {
      return $pkg->from_wire_hash($wire_hash, $opts);
    }
    else {
      return $pkg->from_wire_hash($wire_hash);
    }
  }
}
1;
__END__
