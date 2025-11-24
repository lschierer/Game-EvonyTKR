use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
use Mojo::File;
use Path::Iterator::Rule;
require YAML::PP;
require Game::EvonyTKR::Role::MarkdownRenderer;

package Game::EvonyTKR::Role::StaticPages {
  use Mojo::Base -role, -signatures;

  use Carp;

  my %static_routes;

  # Provide logger() method for MarkdownRenderer role interdependency

  sub static_route_name_for ($self, $path) {
    return $static_routes{$path};
  }

  sub static_pages ($self, $app, $path) {
    $self->logger->info(sprintf(
      'static_pages function for %s with path "%s"',
      ref($self) ? ref($self) : $self, $path
    ));

    unless ($app) {
      $self->logger->error(sprintf(
        'app undefined for static_pages called by %s',
        ref($self) ? ref($self) : $self));
      return;
    }

    my @parts           = split '::', ref($self) ? ref($self) : $self;
    my $controller_name = $parts[$#parts];
    foreach my $static_entry ($self->build_routes($app, $path)) {
      $self->logger->info(sprintf(
        'Adding route "%s" for file "%s"',
        $static_entry->{route},
        $static_entry->{path}
      ));
      $app->routes->get($static_entry->{route})->to(
        controller => $controller_name,
        action     => 'single_page',
      );
      $app->add_navigation_item({
        title => $static_entry->{file}->{title},
        path  => $static_entry->{route},    # Use URL route, not filesystem path
        order => $static_entry->{file}->{order},
      });
    }
  }

  sub single_page ($c) {
    my $home = Mojo::Home->new->detect;
    my $path = $c->req->url->path;
    $c->logger->debug(sprintf('observed request for "%s"', $path));

    my $page_path = $home->child(sprintf('share/pages/%s', $path));
    $page_path =~ s/\/\/+/\//g;

    if (-d $page_path) {
      $page_path = "$page_path/index.md";
    }
    else {
      $page_path = "${page_path}.md";
    }
    $c->logger->debug(sprintf('single_page looking for "%s"', $page_path));

    unless (-f $page_path) {
      $c->logger->debug(sprintf('cannot find "%s"', $page_path));
      return $c->helpers->reply->not_found;
    }

    return $c->render_markdown_page(
      $c->app,
      Mojo::File->new($page_path),
      { template => 'markdown' }
    );
  }

  sub build_routes ($self, $app, $path) {
    my $pages_dir =
      Mojo::File::Share::dist_dir('Game::EvonyTKR')->child("pages/$path");

    unless (-d $pages_dir) {
      $self->logger->error(sprintf(
        'static route building requested for "%s" which does not exist',
        $pages_dir));
      return ();
    }

    my @added_routes;

    my $rule = Path::Iterator::Rule->new;
    $rule->file->readable->nonempty->name('*.md');
    my $iter = $rule->iter($pages_dir);
    while (my $file = $iter->()) {
      my $file_path     = Mojo::File->new($file);
      my $relative_path = $file_path->to_rel($pages_dir);
      my $route_path    = $self->file_path_to_route($relative_path);
      $self->logger->debug(
        "Considering static route: $route_path for file: $relative_path");

      my $parsedFile = $self->parse_markdown_frontmatter($file_path);
      if ($parsedFile) {
        my $normalized_route = lc($route_path);
        my $has_conflict     = 0;

        my $existing_nav = $app->get_existing_navigation_items() || {};
        $self->logger->debug(sprintf(
          'comparing against %s existing nav entries.',
          scalar keys %$existing_nav));
        foreach my $existing_path (keys %$existing_nav) {
          if (fc($existing_path) eq fc($normalized_route)) {
            $has_conflict = 1;
            $self->logger->debug(sprintf(
              'Skipping static page navigation for "%s"'
                . ' - conflicts with existing "%s"',
              $route_path, $existing_path,
            ));
            last;
          }
        }

        unless ($has_conflict) {
          # Prefix with base path - handle index.md case where route_path is '/'
          my $full_route = $route_path eq '/' ? $path : "$path$route_path";
          $self->logger->debug(
            sprintf('Registering "%s" as static route, no conflicts present',
              $full_route)
          );
          push @added_routes,
            {
            route => $full_route,
            path  => $file_path,
            file  => $parsedFile,
            };
        }
      }
    }
    return @added_routes;
  }

  # Convert file path to route path
  sub file_path_to_route {
    my ($self, $path) = @_;

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

}
1;

__END__
