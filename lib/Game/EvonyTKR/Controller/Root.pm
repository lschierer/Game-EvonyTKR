use v5.42.0;
use utf8::all;
use File::FindLib 'lib';
use namespace::autoclean;

package Game::EvonyTKR::Controller::Root {
  use Mooish::Base -standard;
  extends 'Game::EvonyTKR::Controller::ControllerBase';
  with 'Game::EvonyTKR::Role::StaticPages';

  use Carp;
  use Future::AsyncAwait;
  use Path::Tiny qw(path);

  sub build ($self) {
    $self->logger->info("Building Root controller");

    # Call parent to register common routes
    $self->SUPER::build();

    # Register root page
    $self->add_navigation_route('/', 'Home', { order => 0 });

    $self->router->add('/', {
      to => async sub ($self, $ctx) {
        return await $self->index($ctx);
      },
      action => 'http.*',
    });

    # Register Reference page
    $self->add_navigation_route('/Reference', 'Reference', { order => 104 });

    $self->router->add('/Reference', {
      to => async sub ($self, $ctx) {
        return await $self->single_page($ctx, '/Reference');
      },
      action => 'http.*',
    });

    # Register privacy policy
    $self->add_navigation_route('/policy/privacy', 'Privacy Policy', { order => 200 });

    $self->router->add('/policy/privacy', {
      to => async sub ($self, $ctx) {
        return await $self->single_page($ctx, '/policy/privacy');
      },
      action => 'http.*',
    });
  }

  async sub index ($self, $ctx) {
    my $index_path = path('share/pages/index.md');

    $self->logger->debug("Rendering root index from $index_path");

    unless ($index_path->exists) {
      $self->logger->error("Root index.md not found at $index_path");
      return $self->render('root/index.tt', {
        content => '<p>Welcome to EvonyTKR</p>',
        title => 'EvonyTKR Guide',
        current_year => (localtime)[5] + 1900,
        sidebar => 0,
        navigation => $self->render_navigation($ctx->req->path),
        site_logo => $self->site_logo(),
      });
    }

    # Render markdown and get HTML content
    my $content_html = $self->retrieve_rendered_markdown($index_path);

    return $self->render('root/index.tt', {
      content => $content_html,
      title => 'EvonyTKR Guide',
      current_year => (localtime)[5] + 1900,
      sidebar => 0,
      navigation => $self->render_navigation($ctx->req->path),
      site_logo => $self->site_logo(),
    });
  }

  async sub single_page ($self, $ctx, $route_path) {
    $self->logger->debug("Rendering single page for route: $route_path");

    # Convert route to file path
    my $file_path = $route_path;
    $file_path =~ s|^/||;  # Remove leading slash
    my $md_path = path('share/pages')->child("$file_path.md");

    unless ($md_path->exists) {
      $self->logger->warn("Markdown file not found: $md_path");
      return $self->render_error(404, "Page not found");
    }

    return $self->render_markdown_page($md_path->stringify, $ctx->req->path, {
      template => 'markdown.tt',
      sidebar => 1,
    });
  }
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
