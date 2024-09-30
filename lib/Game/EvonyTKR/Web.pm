use v5.40.0;
use experimental qw(class);
use utf8::all;
use File::ShareDir ':ALL';
use File::Spec;
use Data::Printer;
require Mojolicious::Routes;
use namespace::autoclean;

package Game::EvonyTKR::Web {
# ABSTRACT: package providing REST wrappers for the content created by this distribution
  use Carp;
  use Mojo::Base 'Mojolicious', -signatures;
  use Mojolicious::Routes::Route;
  use Mojo::File::Share qw(dist_dir dist_file);
  use Log::Log4perl::Level;
  use File::FindLib 'lib';
  use Game::EvonyTKR::Web::Logger;
  use Game::EvonyTKR::Logger::Config;

  my $logLevel = 'INFO';

  # This method will run once at server start
  sub startup ($self) {

    my $dist_dir = dist_dir('Game-EvonyTKR');

    my $confFile = File::Spec->catfile($dist_dir, 'game-evony_t_k_r-web.yml');

    my $logConf = new Game::EvonyTKR::Logger::Config;
    my $logPath = $logConf->path();
    $logLevel = $self->mode() eq 'production' ? 'INFO' : 'TRACE';

    # Load configuration from config file
    my $config =
      $self->plugins->register_plugin('Mojolicious::Plugin::NotYAMLConfig',
      Mojolicious->new, { file => $confFile });

    my $wlog = Game::EvonyTKR::Web::Logger->new(category => 'Web',);
    $wlog->logInit($self->mode());
    $self->log($wlog->webLogger());

    # Configure the application
    if (my $secrets = $config->{secrets}) {
      $self->secrets($secrets);
    }

    $self->renderer->paths([File::Spec->catfile($dist_dir, 'templates')]);

    # Router
    my $r = $self->routes;
    $r->namespaces(['Game::EvonyTKR::Web::Controller']);

    #let me use the DefaultHelpers
    $self->plugin('DefaultHelpers');

    # Normal route to controller
    $r->get('/')->to('Example#welcome');
    $self->plugin('Game::EvonyTKR::Web::Routes::General', $r);

    $self->log()->info('start up complete');
  }

}
1;

__END__
