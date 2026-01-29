package Game::EvonyTKR::Controller::ControllerBase;
use v5.42.0;
use experimental qw(class);
use utf8::all;
use Mooish::Base -standard;
extends 'WebFramework::Controller::Base';
# Compose EvonyTKR-specific roles
with 'Game::EvonyTKR::Role::Common';
with 'Game::EvonyTKR::Role::JSON';
with 'WebFramework::Role::Logger';

require YAML::PP;
require Data::Printer;
use Carp;
use Future::AsyncAwait;

has app_config => (
  is      => 'ro',
  default => sub {
    my $self = shift;
    return $self->app->config;
  },
);

has standard_delay => (
  is      => 'ro',
  default => 30,
);

my $base = '';

sub getBase($self) {
  return $base;
}

sub getConstants {
  state $constants = Game::EvonyTKR::Shared::Constants->new();
  return $constants;
}

# Base build method - subclasses should call SUPER::build()
sub build ($self) {
  $self->logger->debug("ControllerBase build");

  # Register common routes that all controllers need
  $self->_register_common_routes();
}

# Common routes for all controllers
sub _register_common_routes ($self) {
  my $router = $self->router;

  # Sitemap route
  $router->add(
    '/sitemap.xml',
    {
      to => sub ($self, $ctx) {
        my $xml = $self->generate_sitemap_xml();
        $ctx->res->headers(content_type => 'application/xml; charset=utf-8');
        return $xml;
      },
      action => 'http.*',
    }
  );

  # Robots.txt route
  $router->add(
    '/robots.txt',
    {
      to => sub ($self, $ctx) {
        my $host   = $ctx->req->headers->{'host'} // '';
        my $is_dev = $host =~ /dev|localhost|127\.0\.0\.1/i;

        my $robots =
          $is_dev
          ? "User-agent: *\nDisallow: /\n"
          : "User-agent: *\nDisallow:\nSitemap: "
          . $ctx->req->base
          . "sitemap.xml\n";

        $ctx->res->headers(content_type => 'text/plain');
        return $robots;
      },
      action => 'http.*',
    }
  );

  # Health check route
  $router->add(
    '/health',
    {
      to => sub ($self, $ctx) {
        my $APP_START_TIME = $self->app->config->{config}->{APP_START_TIME}
          // time();
        my $deployment_env =
          $self->app->config->{config}->{'EvonyTKR-Environment'} // {};

        # Determine if we're in EC2 or container environment
        my $is_ec2 = !$deployment_env->{'IMAGE_TAG'};

        my $env_info = {};
        if ($is_ec2) {
          # EC2 deployment info
          $env_info = {
            deployment_type => 'ec2',
            hostname        => $deployment_env->{'HOSTNAME'} // `hostname`,
            git_commit      =>
              $self->app->config->{config}->{version}->{'git-commit'}
              // 'unknown',
            git_branch =>
              $self->app->config->{config}->{version}->{'git-branch'}
              // 'unknown',
            build_time =>
              $self->app->config->{config}->{version}->{'build-time'}
              // 'unknown',
            cdk_deployment_time => $deployment_env->{'DEPLOYMENT_TIME'}
              // 'unknown',
          };
          chomp $env_info->{hostname} if $env_info->{hostname};
        }
        else {
          # Container deployment info
          $env_info = {
            deployment_type     => 'container',
            cdk_deployment_time => $deployment_env->{'DEPLOYMENT_TIME'}
              // 'unknown',
          };
        }

        use JSON::MaybeXS;
        my $json = JSON::MaybeXS->new(utf8 => 1, pretty => 1);

        my $response = $json->encode({
          status             => 'ok',
          mode               => $self->app->env          // 'unknown',
          version            => $Game::EvonyTKR::VERSION // 'unknown',
          time               => scalar localtime,
          app_started_at     => scalar(localtime($APP_START_TIME)),
          app_uptime_seconds => time() - $APP_START_TIME,
          %$env_info,
        });

        $ctx->res->headers(content_type => 'application/json; charset=utf-8');
        return $response;
      },
      action => 'http.*',
    }
  );
}

# Subclasses override this to specify which collection they manage
sub collection_name ($self) {
  return '';
}

# Subclasses override to specify their base route
sub controller_name ($self) {
  return ref($self) =~ s/.*:://r;
}

# Render error responses
# Note: This is called from route handlers, so we need $ctx
sub render_error ($self, $ctx, $status, $message) {
  $self->logger->error("Error $status: $message");

  # Use Thunderhorse's render_error which knows how to format responses
  return $self->SUPER::render_error($ctx, $status, $message);
}

# Override template to automatically include is_production
sub template ($self, $template_name, $vars = {}) {
  # Add is_production to all template renders
  $vars->{is_production} //= ($self->app->env // '') eq 'production';
  return $self->SUPER::template($template_name, $vars);
}

# Default index action - subclasses should override
async sub index ($self, $ctx) {
  $self->logger->warn('using default index from ControllerBase');

  my $content = "Hello from the " . $self->getBase() . " Controller";

  return $self->template(
    'markdown.tt',
    {
      content      => $content,
      title        => $self->collection_name(),
      current_year => (localtime)[5] + 1900,
    }
  );
}

# Generate sitemap XML for SEO
sub generate_sitemap_xml ($self) {
  # TODO: Implement sitemap generation based on navigation
  my $base_url = $self->app->config->{config}->{base_url}
    // 'https://evonytkrguide.com';

  my $xml = qq{<?xml version="1.0" encoding="UTF-8"?>\n};
  $xml .= qq{<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">\n};

  # Add homepage
  $xml .= qq{  <url>\n};
  $xml .= qq{    <loc>$base_url/</loc>\n};
  $xml .= qq{    <priority>1.0</priority>\n};
  $xml .= qq{  </url>\n};

  # TODO: Iterate through navigation items and add to sitemap

  $xml .= qq{</urlset>\n};

  return $xml;
}

1;

__END__

=head1 NAME

Game::EvonyTKR::Controller::ControllerBase - Base controller for EvonyTKR Thunderhorse controllers

=head1 DESCRIPTION

Provides common functionality for all EvonyTKR controllers:
- Logging via WebFramework::Role::Logger
- Markdown rendering via WebFramework::Role::Markdown
- Common routes (/health, /robots.txt, /sitemap.xml)
- Helper methods for data loading and navigation

All EvonyTKR controllers should extend this base class.

=cut
