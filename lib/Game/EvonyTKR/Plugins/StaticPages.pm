# lib/Game/EvonyTKR/Plugins/StaticPages.pm
use v5.40.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
use Mojo::File;
use Path::Iterator::Rule;

package Game::EvonyTKR::Plugins::StaticPages {
  use Mojo::Base 'Mojolicious::Plugin';
  use Carp;

  sub register {
    my ($self, $app, $config) = @_;
    my $logger = Log::Log4perl->get_logger(__PACKAGE__);
    $logger->info("Registering static page routes");

    # Get the pages directory
    my $pages_dir = Mojo::File::Share::dist_dir('Game::EvonyTKR')->child('pages');

    # Find all markdown files
    my $rule = Path::Iterator::Rule->new;
    $rule->file->name('*.md');
    my $iter = $rule->iter($pages_dir);

    # Register routes for each markdown file
    while (my $file = $iter->()) {
      my $file_path = Mojo::File->new($file);
      my $relative_path = $file_path->to_rel($pages_dir);

      # Convert path to route
      my $route_path = $self->file_path_to_route($relative_path);
      $logger->debug("Registering static route: $route_path for file: $relative_path");

      # Register the route with low priority
      $app->routes->get($route_path)->to(
        cb => sub {
          my $c = shift;
          return $c->render_markdown_file($file_path);
        }
      )->name("static_$route_path");
    }
  }

  # Convert file path to route path
  sub file_path_to_route {
    my ($self, $path) = @_;

    # Remove file extension
    $path =~ s/\.md$//;

    # Handle index files
    $path =~ s/\/index$//;

    # Ensure path starts with /
    $path = "/$path" unless $path =~ /^\//;

    return $path;
  }
};

1;
