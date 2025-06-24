use v5.40.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
use namespace::clean;

# lib/Game/EvonyTKR/Plugins/Navigation.pm
package Game::EvonyTKR::Plugins::Navigation {
  use Mojo::Base 'Mojolicious::Plugin';
  use Carp;
  require Log::Log4perl;

  # Store navigation items by path
  my %nav_items_by_path = ();

  sub register {
    my ($self, $app, $config) = @_;
    my $logger = Log::Log4perl->get_logger(__PACKAGE__);
    $logger->info("Registering navigation plugin");

    # Helper to add navigation items
    $app->helper(add_navigation_item => sub {
      my ($c, $item) = @_;

      my $logger = Log::Log4perl->get_logger(__PACKAGE__);
      $logger->debug(
        "add_navigation_item called for " . Data::Printer::np($item));

      # Validate item structure
      unless (ref $item eq 'HASH'
        && exists $item->{title}
        && exists $item->{path}) {
        $logger->error("Invalid navigation item: " . Data::Printer::np($item));
        return;
      }

      my $path = $item->{path};

      # Handle conflicts
      if (exists $nav_items_by_path{$path}) {
        my $existing = $nav_items_by_path{$path};

        # If both have order, use the one with lower value
        if (exists $item->{order} && exists $existing->{order}) {
          if ($item->{order} < $existing->{order}) {
            $nav_items_by_path{$path} = $item;
            $logger->debug(
              "Replaced navigation item for path '$path' with lower order item");
          }
          else {
            $logger->debug(
              "Kept existing navigation item for path '$path' with lower order");
          }
        }
        # If only new item has order, use it
        elsif (exists $item->{order}) {
          $nav_items_by_path{$path} = $item;
          $logger->debug(
            "Replaced navigation item for path '$path' with ordered item");
        }
        # If only existing item has order, keep it
        elsif (exists $existing->{order}) {
          $logger->debug(
            "Kept existing navigation item for path '$path' with order");
        }
        # If neither has order, log error and keep first
        else {
          $logger->error(
            "Multiple navigation items for path '$path' without order attribute");
        }
      }
      else {
        # No conflict, just add the item
        $nav_items_by_path{$path} = $item;
        $logger->debug(
          "Added navigation item: " . $item->{title} . " => " . $path);
      }

      return 1;
    });

    # Helper to generate organized navigation
    $app->helper(
      generate_navigation => sub {
        my $c = shift;

        # Build navigation structure
        my $nav_structure = {};

        # Process all navigation items
        foreach my $path (keys %nav_items_by_path) {
          my $item = $nav_items_by_path{$path};
          $self->add_item_to_structure($item, $nav_structure);
        }

        # Organize and return
        return $self->organize_navigation($nav_structure);
      }
    );

    # Add the navigation to every request
    $app->hook(
      before_render => sub {
        my ($c, $args) = @_;

        # Skip for API routes and non-HTML responses
        return
          if $args->{json} || $args->{text} || $c->req->url->path =~ /\.json$/;

        # Generate navigation and add to stash
        $c->stash(navigation => $c->generate_navigation);
      }
    );
  }


  # Add an item to the navigation structure
  sub add_item_to_structure {
    my ($self, $item, $nav_structure) = @_;

    my $path     = $item->{path};
    my @segments = split('/', $path);
    shift @segments if $segments[0] eq '';    # Remove empty first segment

    # Skip if no segments
    return unless @segments;

    # Handle parent-child relationships
    if ($item->{parent}) {
      # This is a child item with a specified parent
      my $parent = $item->{parent};

      # Ensure parent exists
      $nav_structure->{$parent} ||= {
        title    => $self->format_title($parent),
        path     => "/$parent",
        children => {},
        order    => $item->{parent_order} || 0,
      };

      # Add as child
      my $last_segment = $segments[-1];
      $nav_structure->{$parent}->{children}->{$last_segment} = {
        title    => $item->{title},
        path     => $path,
        children => $item->{children} || {},
        order    => $item->{order}    || 0,
      };
    }
    else {
      # This is a top-level item or a path with multiple segments
      my $current   = $nav_structure;
      my $full_path = '';

      for my $i (0 .. $#segments) {
        my $segment = $segments[$i];
        $full_path .= "/$segment";

        if ($i == $#segments) {
          # This is the last segment - use the item's title
          $current->{$segment} = {
            title    => $item->{title},
            path     => $full_path,
            children => $item->{children} || {},
            order    => $item->{order}    || 0,
          };
        }
        else {
          # Create intermediate segments if they don't exist
          $current->{$segment} ||= {
            title    => $self->format_title($segment),
            path     => $full_path,
            children => {},
            order    => 0,
          };

          # Move to next level
          $current = $current->{$segment}->{children};
        }
      }
    }
  }

  # Format a path segment into a title
  sub format_title {
    my ($self, $segment) = @_;

    # Replace hyphens and underscores with spaces
    $segment =~ s/[-_]/ /g;

    # Capitalize words
    $segment =~ s/(\w+)/\u\L$1/g;

    return $segment;
  }

  # Organize navigation into a hierarchical structure
  sub organize_navigation {
    my ($self, $nav_structure) = @_;

    my @nav = ();

    # Process top-level items
    foreach my $key (sort keys %$nav_structure) {
      my $item = $nav_structure->{$key};

      # Process children recursively
      my @children = ();
      foreach my $child_key (sort keys %{ $item->{children} }) {
        push @children,
          $self->organize_navigation_item($child_key,
          $item->{children}->{$child_key});
      }

      # Add item with its children
      push @nav,
        {
        title    => $item->{title},
        path     => $item->{path},
        children => \@children,
        order    => $item->{order},
        };
    }

    # Sort by order
    @nav = sort { $a->{order} <=> $b->{order} } @nav;

    return \@nav;
  }

  # Recursively organize a navigation item
  sub organize_navigation_item {
    my ($self, $key, $item) = @_;

    my @children = ();
    foreach my $child_key (sort keys %{ $item->{children} }) {
      push @children,
        $self->organize_navigation_item($child_key,
        $item->{children}->{$child_key});
    }

    return {
      title    => $item->{title},
      path     => $item->{path},
      children => \@children,
      order    => $item->{order},
    };
  }
}

1;
