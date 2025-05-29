use v5.40.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
use namespace::clean;

package Game::EvonyTKR::Plugins::GeneralConflictGroups {
  use Mojo::Base 'Game::EvonyTKR::Plugins::CollectionBase';

  # Specify which collection this controller handles
  sub collection_name {'generalConflictGroups'}

  # Override loadItem to add any generals-specific processing

}

1;
