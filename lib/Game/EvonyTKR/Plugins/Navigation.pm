use v5.40.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';

package Game::EvonyTKR::Plugins::Navigation {
  use Mojo::Base 'Mojolicious::Plugin';
  use Carp;
  require Log::Log4perl;
  require Data::Printer;

  sub register {
    my ($self, $app, $config) = @_;
    my $logger = Log::Log4perl->get_logger(__PACKAGE__);
    $logger->info("Registering navigation plugin");

    # Add helper to generate navigation
    $app->helper(generate_navigation => sub {
      my $c = shift;

      # Get all routes
      my $routes = $c->app->routes->children;
      my %nav_structure;

      # Process routes to build navigation structure
      foreach my $route (@$routes) {
        $self->process_route($route, \%nav_structure);
      }

      # Sort and organize navigation
      return $self->organize_navigation(\%nav_structure);
    });

    # Add the navigation to every request
    $app->hook(before_render => sub {
      my ($c, $args) = @_;
      my $logger = Log::Log4perl->get_logger(__PACKAGE__);
      # Skip for API routes and non-HTML responses
      return if $args->{json} || $args->{text} || $c->req->url->path =~ /\.json$/;
      $logger->debug("generating navigation for request " . $c->req->url->path);
      # Generate navigation and add to stash
      my $nav = $c->generate_navigation;
      $logger->debug("generated navigation". Data::Printer::np($nav));
      $c->stash(navigation => $nav);
    });
  }

  # Process a route to extract navigation information
  sub process_route {
    my ($self, $route, $nav_structure) = @_;

    # Skip routes without paths or names
    return unless $route->pattern->unparsed && $route->name;

    # Skip API routes and static assets
    return if $route->pattern->unparsed =~ /\.(json|css|js|png|jpg|gif)$/;

    # Extract path segments
    my $path = $route->pattern->unparsed;
    my @segments = split('/', $path);
    shift @segments if $segments[0] eq '';  # Remove empty first segment

    # Skip if no segments
    return unless @segments;

    # Add to navigation structure
    my $current = $nav_structure;
    my $full_path = '';

    for my $i (0..$#segments) {
      my $segment = $segments[$i];
      $full_path .= "/$segment";

      # Skip placeholder segments
      next if $segment =~ /^:/;

      # Create segment if it doesn't exist
      $current->{$segment} ||= {
        path => $full_path,
        title => $self->format_title($segment),
        children => {},
        order => $i,
      };

      # Move to next level
      $current = $current->{$segment}->{children};
    }

    # Process child routes
    foreach my $child ($route->children->@*) {
      $self->process_route($child, $nav_structure);
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
      foreach my $child_key (sort keys %{$item->{children}}) {
        push @children, $self->organize_navigation_item(
          $child_key,
          $item->{children}->{$child_key}
        );
      }

      # Add item with its children
      push @nav, {
        title => $item->{title},
        path => $item->{path},
        children => \@children,
        order => $item->{order},
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
    foreach my $child_key (sort keys %{$item->{children}}) {
      push @children, $self->organize_navigation_item(
        $child_key,
        $item->{children}->{$child_key}
      );
    }

    return {
      title => $item->{title},
      path => $item->{path},
      children => \@children,
      order => $item->{order},
    };
  }
};

1;
