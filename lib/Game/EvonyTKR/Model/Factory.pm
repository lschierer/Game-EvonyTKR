use v5.42.0;
use utf8::all;
use File::FindLib 'lib';

# lib/Game/EvonyTKR/Model/Factory.pm
package Game::EvonyTKR::Model::Factory {
  use Module::Runtime 'use_module';

  # $type like 'Book', 'General', 'Pair', etc.
  sub build_from_wire ($class, $type, $wire_hash) {
    my $pkg = "Game::EvonyTKR::Model::$type";
    use_module($pkg);    # runtime load
    return $pkg->from_wire_hash($wire_hash);
  }
}
1;
__END__
