package Game::EvonyTKR::Role::StaticPages;
use v5.42.0;
use experimental qw(class);
use utf8::all;
use Moo::Role;

require YAML::PP;
require WebFramework::Role::Markdown;

use Mojo::Util;
use Mojo::File;
use Path::Iterator::Rule;
use Carp;

my %static_routes;

# Provide logger() method for MarkdownRenderer role interdependency

sub static_route_name_for ($c, $path) {
  return $static_routes{$path};
}

sub static_pages ($c, $app, $path) {
  $c->logger->info(sprintf(
    'static_pages function for %s with path "%s"',
    ref($c) ? ref($c) : $c, $path
  ));

  unless ($app) {
    $c->logger->error(sprintf('app undefined for static_pages called by %s',
      ref($c) ? ref($c) : $c));
    return;
  }

  my @parts           = split '::', ref($c) ? ref($c) : $c;
  my $controller_name = $parts[$#parts];
  foreach my $static_entry ($c->build_routes($app, $path)) {
    $c->logger->info(sprintf(
      'Adding route "%s" for file "%s"',
      $static_entry->{route},
      $static_entry->{path}
    ));
    $app->routes->get($static_entry->{route})
      ->to("${controller_name}#single_page");
    $app->add_navigation_item({
      title => $static_entry->{file}->{title},
      path  => $static_entry->{route},    # Use URL route, not filesystem path
      order => $static_entry->{file}->{order},
    });
  }
}

sub single_page ($c) {
  my $home = Mojo::Home->new->detect;
  my $path = Mojo::Util::url_unescape($c->req->url->path->to_string);
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

  return $c->render_markdown_page(Mojo::File->new($page_path),
    { template => 'markdown' });
}

sub build_routes ($c, $app, $path) {
  my $pages_dir =
    Mojo::File::Share::dist_dir('Game::EvonyTKR')->child("pages/$path");

  unless (-d $pages_dir) {
    $c->logger->error(sprintf(
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
    my $route_path    = $c->file_path_to_route($relative_path);
    $c->logger->debug(
      "Considering static route: $route_path for file: $relative_path");

    my $parsedFile = $c->parse_markdown_frontmatter($file_path);
    if ($parsedFile) {
      my $normalized_route = lc($route_path);
      my $has_conflict     = 0;

      my $existing_nav = $app->get_existing_navigation_items() || {};
      $c->logger->debug(sprintf('comparing against %s existing nav entries.',
        scalar keys %$existing_nav));
      foreach my $existing_path (keys %$existing_nav) {
        if (fc($existing_path) eq fc($normalized_route)) {
          $has_conflict = 1;
          $c->logger->debug(sprintf(
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
        $c->logger->debug(
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

1;

__END__
