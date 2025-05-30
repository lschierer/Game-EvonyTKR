use v5.40.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Game::EvonyTKR::Model::Speciality::Manager;
use namespace::clean;

package Game::EvonyTKR::Plugins::Specialities {
  use Mojo::Base 'Game::EvonyTKR::Plugins::CollectionBase';

  # Specify which collection this controller handles
  sub collection_name {'specialities'}

  my $manager;

  # Override loadItem to add any Speciality-specific processing

  sub register($self, $app, $config = {}) {
    my $logger = Log::Log4perl->get_logger(__PACKAGE__);
    $logger->info("Registering routes for " . ref($self));
    $self->SUPER::register($app, $config);

    eval {
      $manager = Game::EvonyTKR::Model::Speciality::Manager->new();

      my $distDir    = Mojo::File::Share::dist_dir('Game::EvonyTKR');
      my $collection = $self->collection_name;
      my $SourceDir  = $distDir->child("collections/$collection");
      $manager->importAll($SourceDir);

      $logger->info(
        "Successfully loaded Speciality manager with collection from $SourceDir");
    };
    if ($@) {
      $logger->error("Failed to initialize Speciality Manager: $@");
      $manager = undef;
    }

    $app->helper(
      get_builtinbook_manager => sub {
        return $manager;
      }
    );
  }

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
