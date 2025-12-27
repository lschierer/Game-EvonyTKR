use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require Mojolicious::Controller;
require Mojolicious::Plugin;
require Game::EvonyTKR::Role::MarkdownRenderer;
use namespace::clean;

package Game::EvonyTKR::Controller::ControllerBase {
  use Mojo::Base 'Mojolicious::Controller';
  use Mojo::Base 'Mojolicious::Plugin',                    -role, -signatures;
  use Mojo::Base 'Game::EvonyTKR::Role::Logging',          -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Common',           -role;
  use Mojo::Base 'Game::EvonyTKR::Role::JSON',             -role;
  use Mojo::Base 'Game::EvonyTKR::Role::MarkdownRenderer', -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Persistence',      -role;
  require Mojo::File;
  require YAML::PP;
  require Data::Printer;
  use Carp;

  my $logger;
  my $base = '';
  my $routes;

  has standard_delay => 30;

  sub getBase($self) {
    return $base;
  }

  sub getConstants {
    state $constants = Game::EvonyTKR::Shared::Constants->new();
    return $constants;
  }

  sub register($c, $app, $config = {}) {
    $logger = Log::Log4perl->get_logger(__PACKAGE__);
    $logger->debug("ControllerBase register function");

    my $routes = $app->routes;

    # Add sitemap route
    $routes->get('/sitemap.xml')->to(cb => sub ($c) {
      my $xml = $c->generate_sitemap_xml;
      $c->render(data => $xml, format => 'xml');
    });

    # Add robots.txt route
    $routes->get('/robots.txt')->to(cb => sub ($c) {
      my $host = $c->req->headers->host // '';
      my $is_dev = $host =~ /dev|localhost|127\.0\.0\.1/i;
      
      my $robots = $is_dev 
        ? "User-agent: *\nDisallow: /\n"
        : "User-agent: *\nDisallow:\nSitemap: " . $c->req->url->base . "sitemap.xml\n";
      
      $c->render(data => $robots, format => 'txt');
    });

    $app->helper(
      outstanding_prereqs => sub($self, $prereqs) {
        if (ref($prereqs) && ref($prereqs) eq 'ARRAY') {
          return $c->are_prereqs_outstanding($app->minion, $prereqs);
        }
        else {
          $c->log_error('outstanding_prereqs requires an arrayref.');
          return 1;
        }
      }
    );

    $app->helper(
      check_prereqs_or_wait =>
        sub($self, $prereqs, $retry_delay = $self->standard_delay) {
        unless (ref($prereqs) && ref($prereqs) eq 'ARRAY') {
          $c->log_error('check_prereqs_or_wait requires an arrayref.');
          return 0;
        }

        my $outstanding = $self->outstanding_prereqs($prereqs);
        if ($outstanding) {
          $c->log_info(sprintf(
            'Prerequisites outstanding for route %s, rendering wait page',
            $self->req->url->path->to_string));

          my $current_url = $self->req->url->to_abs;
          $self->stash(
            retry_url   => $current_url,
            retry_delay => $retry_delay,
            prereqs     => $prereqs,
          );
          $self->render(template => 'prereqs_wait', status => 503);
          return 1;    # Rendered wait page, caller should return
        }
        return 0;      # Prerequisites met, caller should continue
        }
    );

    # Helper to load pairs asynchronously and show wait page until ready
    # Returns: (1, undef) if wait page rendered, (0, $pairs) if data ready
    $app->helper(
      load_pairs_async_or_wait =>
        sub($self, $type, $opts = {}, $retry_delay = 2) {
        # Use app defaults as per-worker cache for loaded pairs
        my $cache_key = "pairs_batch_$type";
        my $loading_key = "${cache_key}_loading";

        # Check if already loaded
        if ($app->defaults->{$cache_key} && @{$app->defaults->{$cache_key}}) {
          $self->log_debug("Pairs for $type already loaded, returning cached data");
          return (0, $app->defaults->{$cache_key});
        }

        # Start loading if not already in progress
        unless ($app->defaults->{$loading_key}) {
          $app->defaults->{$loading_key} = 1;
          $self->log_info("Starting async load for pairs type: $type");

          Mojo::IOLoop->timer(0.01 => sub {
            eval {
              my $loaded = $self->get_pairs_for_type_batch($type, $opts);
              $app->defaults->{$cache_key} = $loaded;
              $self->log_info(sprintf("Loaded %d pairs for type %s", scalar(@$loaded), $type));
            };
            if ($@) {
              $self->log_error("Failed to load pairs for $type: $@");
            }
            delete $app->defaults->{$loading_key};
          });
        }

        # Show wait page with auto-refresh
        my $current_url = $self->req->url->to_abs;
        $self->stash(
          retry_url   => $current_url,
          retry_delay => $retry_delay,
          type        => $type,
        );
        $self->render(template => 'pairs_loading', status => 503);
        return (1, undef);
        }
    );

    $routes->get('/health')->to(
      cb => sub($self) {
        my $APP_START_TIME = $app->config->{'APP_START_TIME'};

        # Get deployment environment info
        my $deployment_env = $app->config->{'EvonyTKR-Environment'} // {};

        # Determine if we're in EC2 or container environment
        my $is_ec2 = !$deployment_env->{'IMAGE_TAG'};

        # Build environment-specific info
        my $env_info = {};
        if ($is_ec2) {
          # EC2 deployment info
          $env_info = {
            deployment_type => 'ec2',
            hostname        => $deployment_env->{'HOSTNAME'} // `hostname`,
            git_commit      => $app->config->{'version'}->{'git-commit'}
              // 'unknown',
            git_branch => $app->config->{'version'}->{'git-branch'}
              // 'unknown',
            build_time => $app->config->{'version'}->{'build-time'}
              // 'unknown',
            cdk_deployment_time => $deployment_env->{'DEPLOYMENT_TIME'}
              // 'unknown',
          };
          chomp $env_info->{hostname} if $env_info->{hostname};
        }
        else {
          # Container deployment info (legacy)
          $env_info = {
            deployment_type     => 'container',
            cdk_deployment_time => $deployment_env->{'DEPLOYMENT_TIME'}
              // 'unknown',
          };
        }

        $self->render(
          json => {
            status             => 'ok',
            mode               => $app->mode // 'unknown',
            version            => $app->VERSION,
            time               => scalar localtime,
            app_started_at     => scalar(localtime($APP_START_TIME)),
            app_uptime_seconds => time() - $APP_START_TIME,
            %$env_info,
          },
          status => 200
        );
      }
    );

    # API endpoint to get all routes with metadata
    $routes->get('/api/routes')->to(
      cb => sub($self) {
        my @all_routes;

        my $walk_routes;
        $walk_routes = sub {
          my ($route, $parent_path) = @_;
          $parent_path //= "";

          my $pattern   = $route->pattern->unparsed // "";
          my $full_path = $parent_path . $pattern;
          my $name      = $route->name // undef;
          my $to        = $route->to   // {};

          # Only include non-empty paths
          if ($full_path && $full_path ne "/" && $full_path ne "") {
            my $route_methods = $route->methods;
            push @all_routes,
              {
              path       => $full_path,
              name       => $name,
              controller => $to->{controller} // undef,
              action     => $to->{action}     // undef,
              methods    => $route_methods ? [sort @{$route_methods}] : undef,
              };
          }

          # Recursively walk children
          foreach my $child (@{ $route->children }) {
            $walk_routes->($child, $full_path);
          }
        };

        $walk_routes->($app->routes);

        $self->render(
          json => {
            routes => \@all_routes,
            count  => scalar(@all_routes),
          }
        );
      }
    );
  }

  sub getRoutes($self) {
    return $routes;
  }

  sub index($self) {
    $self->log_warn('using index from controller base');
    $self->stash(
      base     => $self->getBase(),
      layout   => 'default',
      template => 'markdown',
      content  => "Hello from the $base Controller",
    );

    $self->render();
  }

}
1;
