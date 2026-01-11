use v5.42.0;
use utf8::all;
use File::FindLib 'lib';
require Game::EvonyTKR::Model::General;
use namespace::autoclean;

package Game::EvonyTKR::Controller::Generals {
  use Mooish::Base -standard;
  extends 'Game::EvonyTKR::Controller::ControllerBase';

  use List::AllUtils qw(all any none first);
  use Carp;
  use Path::Tiny qw(path);
  use URI::Escape qw(uri_unescape);
  use Encode qw(decode is_utf8);
  use Scalar::Util qw(blessed);
  use Game::EvonyTKR::Service::PDL::Runtime;

  # PDL Runtime service for fast buff computation
  has 'pdl_runtime' => (
    is => 'ro',
    lazy => 1,
    default => sub ($self) {
      return Game::EvonyTKR::Service::PDL::Runtime->new(
        data_dir => 'share/collections/data'
      );
    },
  );

  # Specify which collection this controller handles
  sub collection_name { 'Generals' }

  my $base = '/Reference/Generals';

  sub getBase($self) {
    return $base;
  }

  sub controller_name ($self) {
    return "Generals";
  }

  # Build method - replaces register() from Mojolicious
  sub build ($self) {
    $self->logger->info("Building Generals controller");

    # Call parent to register common routes
    $self->SUPER::build();

    # Add navigation for main generals page
    $self->add_navigation_route(
      $base,
      'Generals',
      { order => 20, parent => '/Reference' }
    );

    # Register routes
    # Main generals landing page
    $self->router->add($base, {
      to => sub ($self, $ctx) {
        return $self->index($ctx);
      },
      action => 'http.get',
    });

    # Single general detail page
    $self->router->add("$base/:name", {
      to => sub ($self, $ctx, @args) {
        my $name = uri_unescape($args[0]);
        # Ensure UTF-8 decoding
        $name = decode('UTF-8', $name) unless is_utf8($name);
        return $self->show($ctx, $name);
      },
      action => 'http.get',
    });

    # Build navigation items for individual generals
    $self->build_nav_items();
  }

  sub build_nav_items ($self) {
    # Get generals loader from app (registered by DataLoaders module)
    my $generals_loader = $self->generals_loader();

    unless ($generals_loader) {
      $self->logger->error("Generals loader not available");
      return;
    }

    # Note: list_generals() returns normalized keys, not display names
    # We need to load each general to get the proper display name
    foreach my $normalized_key ($generals_loader->list_generals->@*) {
      my $general = eval { $generals_loader->get_general($normalized_key) };

      unless ($general) {
        $self->logger->warn(sprintf(
          'Failed to load general with key "%s"',
          $normalized_key
        ));
        next;
      }

      # Get display name from the general object
      my $display_name = eval { $general->name };
      unless (defined($display_name) && length($display_name)) {
        $self->logger->warn(sprintf(
          'General with key "%s" has no valid name, skipping',
          $normalized_key
        ));
        next;
      }

      # Add to navigation using the proper display name
      eval {
        $self->add_navigation_route(
          "$base/$display_name",
          $display_name,
          { order => 20, parent => $base }
        );
      };
      if ($@) {
        $self->logger->error(sprintf(
          'Failed to add nav item for general "%s" (key: %s): %s',
          $display_name, $normalized_key, $@
        ));
      }
      else {
        $self->logger->debug(sprintf(
          'Added nav item for general "%s" with path "%s/%s" (key: %s)',
          $display_name, $base, $display_name, $normalized_key
        ));
      }
    }
  }

  # Main generals landing page
  sub index($self, $ctx) {
    $self->logger->debug("Rendering generals landing page");

    my $generals_loader = $self->generals_loader();

    unless ($generals_loader) {
      return $self->render_error($ctx, 500, "Generals data not loaded");
    }

    # Gather all generals
    my $items = [];
    foreach my $general_name ($generals_loader->list_generals->@*) {
      my $general = $generals_loader->get_general($general_name);
      unless ($general) {
        $self->logger->error(sprintf('Failed to get listed general "%s"', $general_name));
        next;
      }
      push @{$items}, $general;
    }

    $self->logger->debug(
      sprintf('Generals: %s with %s items', ref($items), scalar(@$items))
    );

    my $vars = {
      items        => $items,
      title        => 'Generals',
      current_year => (localtime)[5] + 1900,
      css_files    => ['/css/collectionIndex.css'],
      sidebar      => 1,
      navigation   => $self->render_navigation($ctx->req->path),
      site_logo    => $self->site_logo(),
    };

    return $self->render('generals/index.tt', $vars);
  }

  # Show general details
  sub show ($self, $ctx, $name) {
    $self->logger->debug("Show details for general: $name");

    my $generals_loader = $self->generals_loader();

    unless ($generals_loader) {
      return $self->render_error($ctx, 500, "Generals data not loaded");
    }

    # Normalize the name for lookup
    my $normalized_name = $self->normalize($name);

    my $general = $generals_loader->get_general($normalized_name);

    unless ($general) {
      $self->logger->debug(
        "General '$name' (normalized: '$normalized_name') not found"
      );
      return $self->render_error($ctx, 404, "General not found");
    }

    $self->logger->debug("Retrieved general object");

    # Debug: Check what we got
    $self->logger->debug(sprintf(
      "General data - name: %s, type: %s, ascending: %s, book: %s",
      $general->can('name') ? ($general->name // 'undef') : 'no name method',
      $general->can('type') ? (ref($general->type) || $general->type // 'undef') : 'no type method',
      $general->can('ascending') ? ($general->ascending // 'undef') : 'no ascending method',
      $general->can('builtInBookName') ? ($general->builtInBookName // 'undef') : 'no book method'
    ));

    # Get ascending attributes for this general if applicable
    my $ascending_attrs;
    eval {
      if ($general->can('ascending') && $general->ascending) {
        $self->logger->debug("General is ascending, looking up attributes");
        my $aa_loader = $self->ascending_attributes_loader();
        if ($aa_loader) {
          my $gen_name = $general->can('name') ? $general->name : $normalized_name;
          my $normalized_general_name = $self->normalize($gen_name);
          $self->logger->debug(sprintf(
            "Looking up ascending attrs for '%s' (normalized: '%s')",
            $gen_name, $normalized_general_name
          ));
          $ascending_attrs = $aa_loader->get_for_general($normalized_general_name);
          $self->logger->debug(sprintf(
            "Ascending attrs lookup result: %s",
            $ascending_attrs ? 'found' : 'not found'
          ));
        } else {
          $self->logger->warn("No ascending attributes loader available");
        }
      } else {
        $self->logger->debug("General is not ascending or ascending field not set");
      }
    };
    if ($@) {
      $self->logger->error("Error getting ascending attributes: $@");
    }

    my $gen_name = eval { $general->name } // $normalized_name;

    # Get built-in book if available
    my $built_in_book;
    eval {
      if ($general->can('builtInBookName') && $general->builtInBookName) {
        my $books_loader = $self->books_loader();
        if ($books_loader) {
          my $book_name = $general->builtInBookName;
          my $normalized_book_name = $self->normalize($book_name);
          $built_in_book = $books_loader->get_book($normalized_book_name);
          $self->logger->debug(sprintf(
            "Looked up book '%s' (normalized: '%s'), found: %s",
            $book_name, $normalized_book_name, $built_in_book ? 'yes' : 'no'
          ));
        }
      }
    };
    if ($@) {
      $self->logger->error("Error getting built-in book: $@");
    }

    # Check if buff calculation is requested via query parameter
    my $buff_summaries;
    my $calculate_buffs = $ctx->req->query('calculate_buffs');

    if ($calculate_buffs) {
      $self->logger->debug("Buff calculation requested for $gen_name");

      # Extract query parameters with defaults
      my $ascending_level = $ctx->req->query('ascendingLevel') // 'red5';
      my $covenant_level  = $ctx->req->query('covenantLevel') // 'civilization';
      my $specialty1      = $ctx->req->query('specialty1') // 'gold';
      my $specialty2      = $ctx->req->query('specialty2') // 'gold';
      my $specialty3      = $ctx->req->query('specialty3') // 'gold';
      my $specialty4      = $ctx->req->query('specialty4') // 'gold';
      my $activation      = $ctx->req->query('activation') // 'Attacking';

      $self->logger->debug(sprintf(
        "Buff params: activation=%s, ascending=%s, covenant=%s, specialties=%s/%s/%s/%s",
        $activation, $ascending_level, $covenant_level,
        $specialty1, $specialty2, $specialty3, $specialty4
      ));

      # Compute buffs using PDL Runtime
      eval {
        $buff_summaries = $self->pdl_runtime->get_buff_summary(
          general => $gen_name,
          activation => $activation,
          filters => {
            ascendingLevel => $ascending_level,
            covenantLevel  => $covenant_level,
            specialty1     => $specialty1,
            specialty2     => $specialty2,
            specialty3     => $specialty3,
            specialty4     => $specialty4,
            generic1       => 'level4',  # Default to level 4 generic books
          }
        );

        $self->logger->debug("Successfully computed buff summaries");
      };
      if ($@) {
        $self->logger->error("Error computing buffs: $@");
      }
    }

    my $vars = {
      item          => $general,
      ascending     => $ascending_attrs,
      builtInBook   => $built_in_book,
      buff_summaries => $buff_summaries,  # Add computed buffs if available
      title         => "Details for $gen_name",
      current_year  => (localtime)[5] + 1900,
      css_files     => ['/css/collectionDetails.css'],
      sidebar       => 1,
      navigation    => $self->render_navigation($ctx->req->path),
      site_logo     => $self->site_logo(),
    };

    $self->logger->debug("About to render generals/details.tt");
    my $result = $self->render('generals/details.tt', $vars);
    $self->logger->debug("Render returned: " . ref($result));
    return $result;
  }
}

1;

__END__

=head1 NAME

Game::EvonyTKR::Controller::Generals - Thunderhorse controller for Generals

=head1 DESCRIPTION

Manages routes and views for Generals in EvonyTKR.

This is Part 1 - Single General Display Only.
Table routes and complex filtering will be added in Part 2.

Routes:
- GET /Reference/Generals - Index of all generals
- GET /Reference/Generals/:name - Details for specific general

=cut
