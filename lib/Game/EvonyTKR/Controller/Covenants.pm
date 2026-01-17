use v5.42.0;
use utf8::all;
use File::FindLib 'lib';
use namespace::autoclean;

package Game::EvonyTKR::Controller::Covenants {
  use Mooish::Base -standard;
  extends 'Game::EvonyTKR::Controller::ControllerBase';

  use List::AllUtils qw(all any none first);
  use Carp;
  use Path::Tiny   qw(path);
  use URI::Escape  qw(uri_unescape);
  use Encode       qw(decode is_utf8);
  use Scalar::Util qw(blessed);

  # Specify which collection this controller handles
  sub collection_name {'Covenants'}

  my $base = '/Reference/Covenants';

  sub getBase($self) {
    return $base;
  }

  sub controller_name ($self) {
    return "Covenants";
  }

  # Build method - replaces register() from Mojolicious
  sub build ($self) {
    $self->logger->info("Building Covenants controller");

    # Call parent to register common routes
    $self->SUPER::build();

    # Add navigation for main covenants page
    $self->add_navigation_route($base, 'Covenants',
      { order => 30, parent => '/Reference' });

    # Register routes
    # Main covenants landing page
    $self->router->add(
      $base,
      {
        to => sub ($self, $ctx) {
          return $self->index($ctx);
        },
        action => 'http.*',
      }
    );

    # Single covenant detail page
    $self->router->add(
      "$base/:name",
      {
        to => sub ($self, $ctx, @args) {
          my $name = uri_unescape($args[0]);
          # Ensure UTF-8 decoding
          $name = decode('UTF-8', $name) unless is_utf8($name);
          return $self->show($ctx, $name);
        },
        action => 'http.*',
      }
    );

    # Build navigation items for individual covenants
    $self->build_nav_items();
  }

  sub build_nav_items ($self) {
    # Get covenants loader from app (registered by DataLoaders module)
    my $covenants_loader = $self->covenants_loader();

    unless ($covenants_loader) {
      $self->logger->error("Covenants loader not available");
      return;
    }

    # Note: list_covenants() returns normalized keys, not display names
    # We need to load each covenant to get the proper display name
    my $covenant_keys = $covenants_loader->list_covenants;
    $self->logger->debug(
      sprintf("Building nav for %d covenants", scalar(@$covenant_keys)));

    foreach my $normalized_key (@$covenant_keys) {
      $self->logger->debug("Processing covenant key: $normalized_key");

      my $covenant = eval { $covenants_loader->get_covenant($normalized_key) };

      unless ($covenant) {
        $self->logger->warn(sprintf(
          'Failed to load covenant with key "%s"', $normalized_key));
        next;
      }

      $self->logger->debug(
        sprintf("Got covenant object: %s", ref($covenant) || 'not a ref'));

      # Debug: Check what we got
      $self->logger->debug(sprintf(
"Covenant object for key '%s': has primary method? %s, primary defined? %s",
        $normalized_key,
        $covenant->can('primary')                         ? 'yes' : 'no',
        ($covenant->can('primary') && $covenant->primary) ? 'yes' : 'no'
      ));

      # Get display name from the primary general
      my $display_name;
      if ($covenant->can('primary') && $covenant->primary) {
        my $primary = $covenant->primary;
        $self->logger->debug(sprintf(
          "Primary object: %s, has name method? %s",
          ref($primary) || 'not a ref',
          (blessed($primary) && $primary->can('name')) ? 'yes' : 'no'
        ));
        $display_name = eval { $covenant->primary->name };
        if ($@) {
          $self->logger->error("Error getting primary name: $@");
        }
      }

      unless (defined($display_name) && length($display_name)) {
        $self->logger->warn(sprintf(
          'Covenant with key "%s" has no valid primary name, skipping',
          $normalized_key));
        next;
      }

      # Add to navigation using the proper display name
      eval {
        $self->add_navigation_route("$base/$display_name", $display_name,
          { order => 30, parent => $base });
      };
      if ($@) {
        $self->logger->error(sprintf(
          'Failed to add nav item for covenant "%s" (key: %s): %s',
          $display_name, $normalized_key, $@
        ));
      }
      else {
        $self->logger->debug(sprintf(
          'Added nav item for covenant "%s" with path "%s/%s" (key: %s)',
          $display_name, $base, $display_name, $normalized_key
        ));
      }
    }
  }

  # Main covenants landing page
  sub index($self, $ctx) {
    $self->logger->debug("Rendering covenants landing page");

    my $covenants_loader = $self->covenants_loader();

    unless ($covenants_loader) {
      return $self->render_error($ctx, 500, "Covenants data not loaded");
    }

    # Gather all covenants
    my $items = [];
    foreach my $normalized_key ($covenants_loader->list_covenants->@*) {
      my $covenant = $covenants_loader->get_covenant($normalized_key);
      unless ($covenant) {
        $self->logger->error(
          sprintf('Failed to get listed covenant "%s"', $normalized_key));
        next;
      }
      push @{$items}, $covenant;
    }

    $self->logger->debug(
      sprintf('Covenants: %s with %s items', ref($items), scalar(@$items)));

    my $vars = {
      items        => $items,
      title        => 'Covenants',
      current_year => (localtime)[5] + 1900,
      css_files    => ['/css/collectionIndex.css'],
      sidebar      => 1,
      navigation   => $self->render_navigation($ctx->req->path),
      site_logo    => $self->site_logo(),
    };

    return $self->render('covenants/index.tt', $vars);
  }

  # Show covenant details
  sub show ($self, $ctx, $name) {
    $self->logger->debug("Show details for covenant: $name");

    my $covenants_loader = $self->covenants_loader();

    unless ($covenants_loader) {
      return $self->render_error($ctx, 500, "Covenants data not loaded");
    }

    # Normalize the name for lookup
    my $normalized_name = $self->normalize($name);

    my $covenant = $covenants_loader->get_covenant($normalized_name);

    unless ($covenant) {
      $self->logger->debug(
        "Covenant '$name' (normalized: '$normalized_name') not found");
      return $self->render_error($ctx, 404, "Covenant not found");
    }

    $self->logger->debug("Retrieved covenant object");

    my $cov_name = eval { $covenant->primary->name } // $normalized_name;

    my $vars = {
      item         => $covenant,
      title        => "Covenant: $cov_name",
      current_year => (localtime)[5] + 1900,
      css_files    => ['/css/collectionDetails.css'],
      sidebar      => 1,
      navigation   => $self->render_navigation($ctx->req->path),
      site_logo    => $self->site_logo(),
    };

    $self->logger->debug("About to render covenants/details.tt");
    my $result = $self->render('covenants/details.tt', $vars);
    $self->logger->debug("Render returned: " . ref($result));
    return $result;
  }
}

1;

__END__

=head1 NAME

Game::EvonyTKR::Controller::Covenants - Thunderhorse controller for Covenants

=head1 DESCRIPTION

Manages routes and views for Covenants in EvonyTKR.

Routes:
- GET /Reference/Covenants - Index of all covenants
- GET /Reference/Covenants/:name - Details for specific covenant

=cut
