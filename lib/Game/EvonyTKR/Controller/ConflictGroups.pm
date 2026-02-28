use v5.42.0;
# cspell: disable
use utf8::all;
use File::FindLib 'lib';
use namespace::autoclean;

package Game::EvonyTKR::Controller::ConflictGroups {
  use Mooish::Base -standard;
  extends 'Game::EvonyTKR::Controller::ControllerBase';

  use List::AllUtils qw(all any none);
  use Carp;

  # Specify which collection this controller handles
  sub collection_name {'Conflict Groups'}

  my $base = '/Reference/Conflict Groups';

  sub getBase ($self) {
    return $base;
  }

  sub controller_name ($self) {
    return "ConflictGroups";
  }

  # Build method - replaces register() from Mojolicious
  sub build ($self) {
    $self->logger->info("Building ConflictGroups controller");

    # Call parent to register common routes
    $self->SUPER::build();

    # Add navigation for conflict groups page
    $self->add_navigation_route(
      $base,
      'General Conflict Groups',
      { order => 60, parent => '/Reference' }
    );

    # Register routes
    # Main conflict groups landing page
    $self->router->add(
      $base,
      {
        to => sub ($self, $ctx) {
          return $self->index($ctx);
        },
        action => 'http.*',
      }
    );
  }

  # Main conflict groups page
  sub index ($self, $ctx) {
    $self->logger->debug("Rendering conflict groups index");

    my $conflicts_loader = $self->conflicts_loader();

    unless ($conflicts_loader) {
      $self->logger->warn("Conflicts loader not available");
      # Render with empty data - template handles this gracefully
      return $self->_render_index($ctx, {}, {});
    }

    # Get conflict pairs indexed by general
    my $pairs = $conflicts_loader->by_general;
    $self->logger->debug(sprintf(
      'Found %d generals in conflict index', scalar keys %$pairs));

    # groups_by_conflict_type is not available from ML predictions
    # (ML doesn't categorize conflicts by type)
    my $groups = {};

    return $self->_render_index($ctx, $groups, $pairs);
  }

  sub _render_index ($self, $ctx, $groups, $pairs) {
    my $vars = {
      groups       => $groups,
      pairs        => $pairs,
      linkBase     => $base,
      title        => 'General Conflict Groups',
      current_year => (localtime)[5] + 1900,
      css_files    => ['/css/collectionIndex.css'],
      sidebar      => 1,
      navigation   => $self->render_navigation($ctx->req->path),
      site_logo    => $self->site_logo(),
    };

    return $self->template('conflict_groups/index.tt', $vars);
  }
}

1;

__END__

=head1 NAME

Game::EvonyTKR::Controller::ConflictGroups - Thunderhorse controller for General Conflict Groups

=head1 DESCRIPTION

Displays general conflicts loaded from ML predictions (conflicts.json).

Routes:
- GET /Reference/Conflict Groups - Index showing conflict pairs

The controller uses Game::EvonyTKR::Loader::Conflicts which loads ML-predicted
conflict data at application startup.

=cut
