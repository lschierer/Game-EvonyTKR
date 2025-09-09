use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require Mojolicious::Controller;
require Mojolicious::Plugin;
use namespace::clean;

package Game::EvonyTKR::Controller::ControllerBase {
  use Mojo::Base 'Mojolicious::Controller';
  use Mojo::Base 'Mojolicious::Plugin', -role, -signatures;
  use Log::Log4perl;
  require Mojo::File;
  require YAML::PP;
  require Data::Printer;
  use Carp;

  my $base = '';
  my $routes;

  sub getBase($self) {
    return $base;
  }

  sub register($self, $app, $config = {}) {
    my $logger = Log::Log4perl->get_logger(__PACKAGE__);
    $logger->info("ControllerBase register function");

    my $routes = $app->routes;

    $routes->get('/health')->to(
      cb => sub($c) {
        my $APP_START_TIME = $app->config->{'APP_START_TIME'};
        $c->render(
          json => {
            status              => 'ok',
            mode                => $app->mode // 'unknown',
            version             => $app->VERSION,
            time                => scalar localtime,
            app_started_at      => scalar(localtime($APP_START_TIME)),
            app_uptime_seconds  => time() - $APP_START_TIME,
            build_time          => $app->config->{'version'}->{'build-time'},
            cdk_deployment_time =>
              $app->config->{'HPFAN-Environment'}->{'DEPLOYMENT_TIME'}
              // 'unknown',
            container_id => $app->config->{'HPFAN-Environment'}->{'HOSTNAME'}
              // 'unknown',    # ECS sets this automatically
            image_tag => $app->config->{'HPFAN-Environment'}->{'IMAGE_TAG'}
              // 'unknown',
            image_uri => $app->config->{'HPFAN-Environment'}->{'IMAGE_URI'}
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
