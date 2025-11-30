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
  use Mojo::Base 'Game::EvonyTKR::Role::MarkdownRenderer', -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Persistence',      -role;
  require Mojo::File;
  require YAML::PP;
  require Data::Printer;
  use Carp;

  my $logger;
  my $base = '';
  my $routes;

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

    $app->helper(
      outstanding_prereqs => sub($self, $prereqs) {
        if (ref($prereqs) && ref($prereqs) eq 'ARRAY') {
          return $c->are_prereqs_outstanding($app->minion, $prereqs);
        }
        else {
          $c->logger->error('outstanding_prereqs requires an arrayref.');
          return 1;
        }
      }
    );

    $app->helper(
      check_prereqs_or_wait => sub($self, $prereqs, $retry_delay = 30) {
        unless (ref($prereqs) && ref($prereqs) eq 'ARRAY') {
          $c->logger->error('check_prereqs_or_wait requires an arrayref.');
          return 0;
        }

        my $outstanding = $self->outstanding_prereqs($prereqs);
        if ($outstanding) {
          $c->logger->info(sprintf(
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

    $routes->get('/health')->to(
      cb => sub($self) {
        my $APP_START_TIME = $app->config->{'APP_START_TIME'};
        $self->render(
          json => {
            status              => 'ok',
            mode                => $app->mode // 'unknown',
            version             => $app->VERSION,
            time                => scalar localtime,
            app_started_at      => scalar(localtime($APP_START_TIME)),
            app_uptime_seconds  => time() - $APP_START_TIME,
            build_time          => $app->config->{'version'}->{'build-time'},
            cdk_deployment_time =>
              $app->config->{'EvonyTKR-Environment'}->{'DEPLOYMENT_TIME'}
              // 'unknown',
            container_id => $app->config->{'EvonyTKR-Environment'}->{'HOSTNAME'}
              // 'unknown',    # ECS sets this automatically
            image_tag => $app->config->{'EvonyTKR-Environment'}->{'IMAGE_TAG'}
              // 'unknown',
            image_uri => $app->config->{'EvonyTKR-Environment'}->{'IMAGE_URI'}
              // 'unknown',
            version    => $app->VERSION,
            git_commit => $app->config->{'version'}->{'git-commit'},
          },
          status => 200
        );
      }
    );
  }

  sub getRoutes($self) {
    return $routes;
  }

  sub index($self) {
    $self->logger->warn('using index from controller base');
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
