use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
use Mojo::File;
use Path::Iterator::Rule;
require YAML::PP;

package Game::EvonyTKR::Plugins::StaticPages {
  use Mojo::Base 'Mojolicious::Plugin';
  use Carp;

  my %static_routes;

  sub register {
    my ($self, $app, $config) = @_;
    my $logger = Log::Log4perl->get_logger(__PACKAGE__);
    $logger->info("Registering static page routes");

    # Add helper to check if a static route exists
    $app->helper(
      static_route_name_for => sub ($c, $path) {
        return $static_routes{$path};    # undef if not present
      }
    );

    # Get the pages directory
    my $pages_dir =
      Mojo::File::Share::dist_dir('Game::EvonyTKR')->child('pages');

    # Find all markdown files
    my $rule = Path::Iterator::Rule->new;
    $rule->file->name('*.md');
    my $iter = $rule->iter($pages_dir);

    # Register routes for each markdown file
    while (my $file = $iter->()) {
      my $file_path     = Mojo::File->new($file);
      my $relative_path = $file_path->to_rel($pages_dir);

      # Convert path to route
      my $route_path = $self->file_path_to_route($relative_path);
      $logger->debug(
        "Registering static route: $route_path for file: $relative_path");

      # Register the route with low priority
      $app->routes->get($route_path)->to(
        cb => sub {
          my $c = shift;
          my $rp = $c->req->url->path->to_string;
          # Remove trailing slash from pages
          if ($rp =~ qr{/$}) {
            my $canonical = $rp;
            $canonical =~ s{/$}{};
            return $c->redirect_to($canonical, 301);
          }
          return $c->render_markdown_file($file_path);
        }
      )->name("static_$route_path");
      my $requested_path = $route_path =~ s{^/}{}r;
      $logger->debug(
        "storing $requested_path in static_routes hash for $route_path");
      $static_routes{$requested_path} = "static_$route_path";

      my $parsedFile = $app->parse_markdown_frontmatter($file_path);
      if ($parsedFile) {
        # Check for case-insensitive navigation conflicts before adding
        my $normalized_route = lc($route_path);
        my $has_conflict     = 0;

        # Check if any existing navigation item conflicts (case-insensitive)
        my $existing_nav = $app->get_existing_navigation_items() || {};
        $logger->debug(sprintf('comparing against %s existing nav entries.',
          scalar keys %$existing_nav));
        foreach my $existing_path (keys %$existing_nav) {
          if (lc($existing_path) eq $normalized_route) {
            $has_conflict = 1;
            $logger->debug(
"Skipping static page navigation for '$route_path' - conflicts with existing '$existing_path'"
            );
            last;
          }
        }

        # Only add navigation item if no case-insensitive conflict exists
        unless ($has_conflict) {
          $logger->debug("$route_path has no conflicts, adding nav entry.");
          $app->add_navigation_item({
            title => $parsedFile->{title},
            path  => $route_path,
            order => $parsedFile->{order}
            ,    # Static pages come after dynamic content
          });
        }
      }
    }
  }

  # Convert file path to route path
  sub file_path_to_route {
    my ($self, $path) = @_;
    my $logger = Log::Log4perl->get_logger(__PACKAGE__);

    # Remove file extension
    $path =~ s/\.md$//;

    # Special case for root index.md
    if ($path eq 'index') {
      return '/';
    }

    # Handle other index files in subdirectories
    $path =~ s/\/index$//;

    # Ensure path starts with /
    $path = "/$path" unless $path =~ /^\//;

    return $path;
  }

};

1;
