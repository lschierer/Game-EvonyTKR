use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Game::EvonyTKR::Model::AscendingAttributes::Manager;
require Data::Printer;
use namespace::clean;

package Game::EvonyTKR::Controller::AscendingAttributes {
  use Mojo::Base 'Game::EvonyTKR::Controller::ControllerBase', -strict,
    -signatures;
  use List::AllUtils qw(uniq);
  use Carp;

  # Specify which collection this controller handles
  sub collection_name {'ascending attributes'}

  my $base = '/Ascending Attributes';

  sub getBase($self) {
    return $base;
  }

  sub controller_name($self) {
    return 'AscendingAttributes';    # Explicitly return the controller name
  }

  sub get_manager ($self) {
    return $self->app->get_root_manager->ascendingAttributesManager;
  }

  # Override loadItem to add any Ascending Attributes-specific processing

  sub register($self, $app, $config = {}) {
    my $logger = Log::Log4perl->get_logger(__PACKAGE__);
    $logger->info("Registering routes for " . ref($self));
    $self->SUPER::register($app, $config);

    my $distDir    = Mojo::File::Share::dist_dir('Game::EvonyTKR');
    my $collection = $self->collection_name;
    my $SourceDir  = $distDir->child("collections/$collection");

    $logger->info(
"Successfully loaded Ascending Attributes manager with collection from $SourceDir"
    );

    $app->helper(
      get_ascendingattributes_manager => sub ($c) {

        return $c->app->get_root_manager->ascendingAttributesManager;
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
      ascending_level_names => sub($c, $level = '', $printable = 0) {
        my $logger = Log::Log4perl->get_logger(__PACKAGE__);
        if (length($level) == 0) {

          my $purpleNames =
            $c->app->get_root_manager->AscendingLevelNames(0, $printable);
          my $redNames =
            $c->app->get_root_manager->AscendingLevelNames(1, $printable);

          # Combine and get unique values
          my @combined = (@$purpleNames, @$redNames);
          my @unique   = uniq(@combined);

          $logger->debug("ascending_level_names returning unique list: "
              . Data::Printer::np(@unique));
          return \@unique;
        }
        if ($level eq 'None') {
          return 'None';
        }
        return $c->app->get_root_manager->AscendingLevelLabel($level);
      }
    );

    $app->helper(
      get_ascending_section => sub ($c, $name = '') {
        if (length($name)) {
          my $item = $app->get_root_manager->ascendingAttributesManager
            ->getAscendingAttributes($name);
          if ( Scalar::Util::reftype($item) eq 'OBJECT'
            && blessed($item) eq 'Game::EvonyTKR::Model::AscendingAttributes') {
            $logger->debug("rendering get_ascending_section for $name");
            return $c->render_to_string(
              item     => $item,
              template => '/ascending attributes/details',
              layout   => undef
            );
          }
          else {
            $logger->warn(
              "get_ascending_section cannot find Ascending Attributes for $name"
            );
            $logger->debug(sprintf(
              "searching for $name, instead got %s %s",
              Scalar::Util::reftype($item),
              blessed($item)
            ));
          }
        }
        else {
          $logger->warn("cannot get_ascending_section without a name");
        }
        return "";
      }
    );
  }

  sub show ($self) {
    return $self->SUPER::show();
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
      && $item->ascending
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
