use v5.40.0;
use experimental qw(class);
use utf8::all;
use File::ShareDir ':ALL';
use File::Spec;
use namespace::autoclean;

package Game::EvonyTKR::Web {
# ABSTRACT: package providing REST wrappers for the content created by this distribution
  use Carp;
  use Mojo::Base 'Mojolicious', -signatures;
  use Mojo::File::Share qw(dist_dir dist_file);
  use FindBin;
  use lib "$FindBin::Bin/../../../lib";
  use Game::EvonyTKR::Web::Logger;

  # This method will run once at server start
  sub startup ($self) {

    my $dist_dir = dist_dir('Game-EvonyTKR');

    my $confFile = 
      File::Spec->catfile($dist_dir, 'game-evony_t_k_r-web.yml');
    
    # Load configuration from config file
    my $config = $self->plugins->register_plugin(
      'Mojolicious::Plugin::NotYAMLConfig',
      Mojolicious->new, {file => $confFile} );

    my $wlog = Game::EvonyTKR::Web::Logger->new(
      category  => 'Web',
      level     => $self->mode() eq 'production' ? 'info' : 'trace',
    );
    $wlog->logInit();
    $self->log($wlog->logger());

    # Configure the application
    if(my $secrets = $config->{secrets}) {
      $self->secrets($secrets);
    }

    # Router
    my $r = $self->routes;

    # Normal route to controller
    $r->get('/')->to('Example#welcome');

    $self->plugin('Game::EvonyTKR::Web::Routes::General', $r);
    $self->log()->info('start up complete');
  }

}
1;
