use v5.42.0;
use utf8::all;
use File::FindLib 'lib';
require Game::EvonyTKR::Model::AscendingAttributes;
use namespace::autoclean;

package Game::EvonyTKR::Controller::AscendingAttributes {
  use Mooish::Base -standard;
  extends 'Game::EvonyTKR::Controller::ControllerBase';

  use List::AllUtils qw(all any none first uniq);
  use Carp;
  use Scalar::Util qw(blessed reftype);
  use Data::Printer;

  # Specify which collection this controller handles
  sub collection_name {'Ascending Attributes'}

  sub controller_name ($self) {
    return "AscendingAttributes";
  }

  # Build method - this controller doesn't register routes, only helpers
  sub build ($self) {
    $self->logger->info(
      "Building AscendingAttributes controller (helpers only)");

    # Call parent to register common routes
    $self->SUPER::build();

    # This controller provides helpers for other controllers to use
    # No routes are registered here - ascending attributes are displayed
    # as part of General detail pages

   # Note: Helper methods are called directly by other controllers
   # (like Generals controller) via $self->get_ascendingattributes_for_general()
  }

  # Get ascending attributes for a specific general
  sub get_ascendingattributes_for_general ($self, $g) {
    my $nn;
    my $gn;

    if (blessed($g) && $g->isa('Game::EvonyTKR::Model::General')) {
      $gn = $g->name;
      $nn = lc($self->normalize($g->name));
    }
    else {
      $gn = "$g";
      $nn = lc($self->normalize($gn));
    }

    $self->logger->debug("Looking for ascending attributes for $nn");

    my $loader = $self->ascending_attributes_loader();
    unless ($loader) {
      $self->logger->error("Ascending attributes loader not available");
      return undef;
    }

    my $aa = $loader->get_for_general($nn);
    unless (defined $aa) {
      $self->logger->error(sprintf(
'No ascending attributes found for general named "%s" normalized to "%s"',
        $gn, $nn
      ));
      return undef;
    }

    $self->logger->debug(
      sprintf('Found %s for requested key %s', np($aa), $nn));
    return $aa;
  }

  # Get column info for rendering ascending attributes table
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

    # Default to purple if we can't determine
    my $set_type = 'purple';

    # Check if item has ascending levels
    if ( $item
      && $item->ascending
      && ref $item->{ascending} eq 'ARRAY'
      && @{ $item->{ascending} }) {
      # Get the first level name
      my $first_level = $item->{ascending}[0]{level};

      # Determine which set it belongs to
      if (grep { $_ eq $first_level } @{ $level_sets{red}{levels} }) {
        $set_type = 'red';
      }
    }

    return $level_sets{$set_type};
  }

  # Sort ascending levels according to their progression
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

# Get ascending section HTML for a general (to be embedded in general detail page)
  sub get_ascending_section ($self, $name = '') {
    unless (length($name)) {
      $self->logger->warn("Cannot get_ascending_section without a name");
      return "";
    }

    my $item = $self->get_ascendingattributes_for_general($name);

    unless ($item
      && reftype($item) eq 'HASH'
      && blessed($item) eq 'Game::EvonyTKR::Model::AscendingAttributes') {
      $self->logger->warn(
        "get_ascending_section cannot find Ascending Attributes for $name");
      $self->logger->debug(sprintf(
        "searching for $name, instead got %s %s",
        reftype($item) // '',
        blessed($item) // ''
      ));
      return "";
    }

    $self->logger->debug("Rendering get_ascending_section for $name");

# This would render the partial template
# For now, return a placeholder - will be properly implemented when Generals controller is created
    my $vars = { item => $item, };

    return $self->render_to_string('ascending attributes/details.tt', $vars);
  }
}

1;

__END__

=head1 NAME

Game::EvonyTKR::Controller::AscendingAttributes - Thunderhorse controller for Ascending Attributes

=head1 DESCRIPTION

This controller provides helper methods for working with Ascending Attributes.
It does NOT register any routes - ascending attributes are displayed as part of
General detail pages rather than as standalone pages.

Helper methods:
- get_ascendingattributes_for_general($general) - Get AA for a general
- get_column_info($item) - Get column info for AA table
- sort_ascending_levels($item) - Sort AA levels in proper order
- get_ascending_section($name) - Render AA section HTML

=cut
