use v5.40.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Game::EvonyTKR::Model::AscendingAttributes::Manager;
use namespace::clean;

package Game::EvonyTKR::Plugins::AscendingAttributes {
  use Mojo::Base 'Game::EvonyTKR::Plugins::CollectionBase';

  my $manager;

  # Specify which collection this controller handles
  sub collection_name {'ascending attributes'}

  sub controller_name($self) {
    return 'AscendingAttributes';    # Explicitly return the controller name
  }

  # Override loadItem to add any Ascending Attributes-specific processing

  sub register($self, $app, $config = {}) {
    my $logger = Log::Log4perl->get_logger(__PACKAGE__);
    $logger->info("Registering routes for " . ref($self));
    $self->SUPER::register($app, $config);

    eval {
      $manager = Game::EvonyTKR::Model::AscendingAttributes::Manager->new();

      my $distDir    = Mojo::File::Share::dist_dir('Game::EvonyTKR');
      my $collection = $self->collection_name;
      my $SourceDir  = $distDir->child("collections/$collection");
      $manager->importAll($SourceDir);

      $logger->info(
"Successfully loaded Ascending Attributes manager with collection from $SourceDir"
      );
    };
    if ($@) {
      $logger->error("Failed to initialize Ascending Attributes Manager: $@");
      $manager = undef;
    }

    $app->helper(
      get_builtinbook_manager => sub {
        return $manager;
      }
    );

    $app->helper(
      get_column_info => sub($c, $item) {
        return $self->get_column_info($item);
      }
    );

    $app->helper(
      sort_ascending_levels => sub($c, $item) {
        return $self->sort_ascending_levels($item);
      }
    );

    $app->helper(
      ascending_level_names => sub($c, $level) {
        return $self->level_names($level);
      }
    );

    $app->helper(
      get_ascending_section => sub ($c, $name = '') {
        if (length($name)) {
          my $item = $self->fetch_for_helper($self, $name);
          if (ref($item) eq 'HASH') {
            return $c->render_to_string(
              item     => $item,
              template => $self->details_template,
              layout   => undef
            );
          }
        }
        return "";
      }
    );
  }

  sub show ($self) {
    return $self->SUPER::show();
  }

  sub fetch_for_helper ($self, $c, $name) {
    my $logger = Log::Log4perl->get_logger(ref($self));
    $logger->debug("start of show_as_string method for $name");

    unless ($self->itemExists($name)) {
      $logger->error("Item not found: $name");
      return '';
    }

    my $item = $self->loadItem($name);
    $c->stash(item => $item);
    return $item;

  }

  sub get_column_info($self, $item) {
    # Define the two possible sets of level names
    my %level_sets = (
      # First set (e.g., for regular generals)
      purple => {
        levels      => ['purple1', 'purple2', 'purple3', 'purple4', 'purple5'],
        title       => 'Purple Ascending Attributes',
        description => 'Attributes for purple generals'
      },
      # Second set (e.g., for special generals)
      red => {
        levels      => ['red1', 'red2', 'red3', 'red4', 'red5'],
        title       => 'Red Ascending Attributes',
        description => 'Attributes for red generals'
      }
    );

    # Default to regular if we can't determine
    my $set_type = 'purple';

    # Check if item has ascending levels
    if ( $item
      && $item->{ascending}
      && ref $item->{ascending} eq 'ARRAY'
      && @{ $item->{ascending} }) {
      # Get the first level name
      my $first_level = $item->{ascending}[0]{level};

      # Determine which set it belongs to
      if (grep { $_ eq $first_level } @{ $level_sets{special}{levels} }) {
        $set_type = 'red';
      }
    }

    return $level_sets{$set_type};
  }

  sub level_names ($self, $level) {
    my $level_names = {
      "purple1" => "1 Purple Star",
      "purple2" => "2 Purple Stars",
      "purple3" => "3 Purple Stars",
      "purple4" => "4 Purple Stars",
      "purple5" => "5 Purple Stars",
      "red1"    => "1 Red Star",
      "red2"    => "2 Red Stars",
      "red3"    => "3 Red Stars",
      "red4"    => "4 Red Stars",
      "red5"    => "5 Red Stars",
    };
    if (exists $level_names->{$level}) {
      return $level_names->{$level};
    }
    return 'None';
  }

  sub sort_ascending_levels($self, $item) {
    # Get column info to determine the level order
    my $column_info = $self->get_column_info($item);
    my %level_order;

    # Create a mapping of level names to their order
    my $i = 0;
    foreach my $level (@{ $column_info->{levels} }) {
      $level_order{$level} = $i++;
    }

    # Return sorted array if ascending exists
    if ($item && $item->{ascending} && ref $item->{ascending} eq 'ARRAY') {
      return [
        sort {
          ($level_order{ $a->{level} } // 999)
            <=> ($level_order{ $b->{level} } // 999)
        } @{ $item->{ascending} }
      ];
    }

    # Return empty array if no ascending data
    return [];
  }

}

1;
