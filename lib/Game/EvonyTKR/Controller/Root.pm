
package Game::EvonyTKR::Controller::Root;
use v5.42.0;
use utf8::all;
use Mooish::Base -standard;
extends 'Game::EvonyTKR::Controller::ControllerBase';
use Future::AsyncAwait;
require Path::Tiny;
require Path::Iterator::Rule;
use Carp;

has app_config => (
  is      => 'ro',
  default => sub {
    my $self = shift;
    return $self->app->config;
  },
);

has base_dir => (
  is      => 'ro',
  default => sub {
    my $self = shift;
    return Path::Tiny::path($self->app->config->{config}->{markdown_dir});
  },
);

sub build ($self) {
  $self->logger->info("Building Root controller");

  # Call parent to register common routes
  $self->SUPER::build();

  # Register root page
  $self->add_navigation_route('/', 'Home', { order => 0 });

  $self->router->add(
    '/',
    {
      to => async sub ($self, $ctx) {
        return await $self->index($ctx);
      },
      action => 'http.*',
    }
  );

  # Register Reference page
  $self->add_navigation_route('/Reference', 'Reference', { order => 104 });

  $self->router->add(
    '/Reference',
    {
      to => async sub ($self, $ctx) {
        return await $self->single_page($ctx, '/Reference');
      },
      action => 'http.*',
    }
  );

  # Register privacy policy
  $self->add_navigation_route(
    '/policy/privacy',
    'Privacy Policy',
    { order => 200 }
  );

  $self->router->add(
    '/policy/privacy',
    {
      to => async sub ($self, $ctx) {
        return await $self->single_page($ctx, '/policy/privacy');
      },
      action => 'http.*',
    }
  );

  my $tree   = $self->_build_Root_Tree;
  my @routes = sort keys %$tree;

  # Register all routes
  for my $route (@routes) {
    my $entry = $tree->{$route};

    # Add to navigation
    if ($route eq '/Harrypedia') {
      $self->add_navigation_route($route, $entry->{title}, { order => 10 });
    }
    elsif (defined($entry->{order})) {
      $self->add_navigation_route($route, $entry->{title},
        { order => $entry->{order} });
    }
    else {
      $self->add_navigation_route($route, $entry->{title}, { order => 50 });
    }

    $self->router->add(
      $route,
      {
        to => sub ($c, $ctx) {
          $self->static_page($ctx, $entry);
        },
        action => 'http.*',
      }
    );
  }

  $self->log(info => "Registered " . scalar(@routes) . " static Root routes");

  # Add catch-all route for directory gaps (AutoIndex)
  $self->router->add(
    '/*path',
    {
      to => sub ($self, $ctx, @args) {
        return $self->handle_directory_gap($ctx);
      },
      action => 'http.*',
    }
  );
}

async sub index ($self, $ctx) {
  my $index_path = Path::Tiny::path('share/pages/index.md');

  $self->logger->debug("Rendering root index from $index_path");

  unless ($index_path->exists) {
    $self->logger->error("Root index.md not found at $index_path");
    return $self->template(
      'root/index.tt',
      {
        content      => '<p>Welcome to EvonyTKR</p>',
        title        => 'EvonyTKR Guide',
        current_year => (localtime)[5] + 1900,
        sidebar      => 0,
        navigation   => $self->render_navigation($ctx->req->path),
        site_logo    => $self->site_logo(),
      }
    );
  }

  # Render markdown and get HTML content
  my $content_html = $self->retrieve_rendered_markdown($index_path);

  return $self->template(
    'root/index.tt',
    {
      content      => $content_html,
      title        => 'EvonyTKR Guide',
      current_year => (localtime)[5] + 1900,
      sidebar      => 0,
      navigation   => $self->render_navigation($ctx->req->path),
      site_logo    => $self->site_logo(),
    }
  );
}

async sub single_page ($self, $ctx, $route_path) {
  $self->logger->debug("Rendering single page for route: $route_path");

  # Convert route to file path
  my $file_path = $route_path;
  $file_path =~ s|^/||;    # Remove leading slash
  my $md_path = Path::Tiny::path('share/pages')->child("$file_path.md");

  unless ($md_path->exists) {
    $self->logger->warn("Markdown file not found: $md_path");
    return $self->render_error(404, "Page not found");
  }

  return $self->render_markdown_page(
    $md_path->stringify,
    $ctx->req->path,
    {
      template => 'markdown.tt',
      sidebar  => 1,
    }
  );
}

async sub static_page ($self, $ctx, $entry) {
  my $fm    = $self->parse_markdown_frontmatter($entry->{path});
  my $title = $fm->{title};
  if (!$title) {
    my $path_for_title = $entry->{route};
    $path_for_title =~ s|^/||;
    $title = $path_for_title;
    $title =~ s|/| - |g;
    $title =~ s/[-_]/ /g;
    $title =~ s/\b(\w)/\U$1/g;
  }

  my $current_year = (localtime)[5] + 1900;
  my $sidebar      = $fm->{layout} // 1;
  $sidebar = 0 if ($sidebar =~ /splash/);

  if (Path::Tiny::path($entry->{path})->slurp_utf8 =~ /classlisttable/i) {
    my $html = $self->retrieve_rendered_markdown($entry->{path});
    $html = await $self->render_classlist_tables($html);

    my $vars = {
      content      => $html,
      title        => $title,
      current_year => $current_year,
      css_files    => ['/css/navigation.css'],
      sidebar      => $sidebar,
      navigation   => $self->render_navigation($entry->{route}),
    };

    return $self->template('page/markdown.tt', $vars);
  }
  else {

    return $self->render_markdown_page(
      $entry->{path},
      $entry->{route},
      {
        site_logo  => $self->site_logo(),
        title      => $title,
        sidebar    => $sidebar,
        navigation => $self->render_navigation($entry->{route}),
      }
    );
  }

}

sub _build_Root_Tree ($self) {
  my %tree;

  $self->log(debug => sprintf('about to iterate over "%s"', $self->base_dir));
  my $rule = Path::Iterator::Rule->new;
  my $next = $rule->file->nonempty->name(qr/\.md/)->iter(
    $self->base_dir,
    {
      depthfirst      => -1,
      follow_symlinks =>  0,
      report_symlinks =>  0,
      sorted          =>  1,
    }
  );
  while (defined(my $file = $next->())) {
    $file = Path::Tiny::path($file);
    $self->log(debug => "Root Controller iterating over '$file'");

    # Fast frontmatter parsing - only read first 20 lines
    my $fm = $self->parse_markdown_frontmatter($file->absolute);
    unless ($fm) {
      $self->log(warn => "No frontmatter available for '$file'");
      next;
    }
    unless (ref($fm) eq 'HASH' && keys %$fm) {
      $self->log(warn => "Empty frontmatter for '$file'");
      next;
    }
    my $title = $fm->{title} // $file->basename(qr/.md/);
    my $route;
    my $order;

    if ($title =~ /index/) {
      $title = $file->parent->basename;
      my $rel_dir = $file->parent->relative($self->base_dir);
      if ($rel_dir eq '.') {
        # Root bookmarks directory
        $route = '/';
      }
      else {
        $route = "/$rel_dir";
      }
    }
    else {
      $route = $file->relative($self->base_dir)->stringify;
      $route =~ s/(.+)\.md$/\/$1/;
      $route =~ s/\/index$//;
    }

    if (exists $fm->{sidebar} && ref($fm->{sidebar}) eq 'HASH' && exists $fm->{sidebar}->{order}) {
      $order = $fm->{sidebar}->{order};
    }

    $tree{$route} = {
      title => $title,
      path  => $file,
      route => $route,
      order => $order,
    };
    $self->log(
      debug => sprintf('Registering route "%s" for file "%s"', $route, $file));
  }
  return \%tree;
}

sub handle_directory_gap ($self, $ctx) {
  my $path = $ctx->req->path;
  $path =~ s|^/||;    # Remove leading slash

  my $dir_path = $self->base_dir->child($path);

  # Check if this is a directory without index.md but has children
  if ( $dir_path->is_dir
    && $dir_path->children
    && !$dir_path->child('index.md')->exists) {
    my $entries = $self->generate_directory_index($dir_path);

    # Generate title from path
    my $title = $path || 'Home';
    $title =~ s|/| - |g;
    $title =~ s/[-_]/ /g;
    $title =~ s/\b(\w)/\U$1/g;

    my $current_year    = (localtime)[5] + 1900;
    my $navigation_html = $self->render_navigation($ctx->req->path);

    my $vars = {
      entries      => $entries,
      title        => $title,
      current_year => $current_year,
      css_files    => ['/css/navigation.css', '/css/directory-list.css'],
      sidebar      => 1,
      nav_html     => $navigation_html,
      site_logo    => $self->site_logo(),
    };

    return $self->template('page/autoindex.tt', $vars);
  }

  # Not found - set status and render error template
  $ctx->res->status(404);
  return $self->template(
    'error.tt',
    {
      title        => '404 - Page Not Found',
      message      => 'The requested page was not found.',
      current_year => (localtime)[5] + 1900,
    }
  );
}

1;

__END__

=pod

=head1 NAME

Game::EvonyTKR::Controller::Root - Controller for root landing page and static pages

=head1 DESCRIPTION

Handles:
- / - Homepage (renders share/pages/index.md)
- /Reference - Reference overview page
- /policy/privacy - Privacy policy page

=cut
