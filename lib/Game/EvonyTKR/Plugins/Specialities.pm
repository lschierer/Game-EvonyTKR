use v5.40.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
use namespace::clean;

package Game::EvonyTKR::Plugins::Specialities {
  use Mojo::Base 'Game::EvonyTKR::Plugins::CollectionBase';

  # Specify which collection this controller handles
  sub collection_name {'specialities'}

  # Override loadItem to add any Speciality-specific processing

  sub sort_levels($self, $levels) {
    # Define the order of levels (if they don't sort alphabetically)
    my %level_order = (
      'Green'  => 1,
      'Blue'   => 2,
      'Purple' => 3,
      'Orange' => 4,
      'Gold'   => 5,
    );

    # Return sorted array
    return [
      sort {
  # Use the defined order if available, otherwise fall back to string comparison
        ($level_order{ $a->{level} } // 999)
          <=> ($level_order{ $b->{level} } // 999)
          || $a->{level} cmp $b->{level}
      } @$levels
    ];
  }

}

1;
