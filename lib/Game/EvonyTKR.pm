use v5.40.0;
use feature 'try';
use experimental qw(class);
use utf8::all;
use MojoX::Log::Log4perl;
use Mojo::File qw(curfile);
use lib curfile->dirname->sibling('lib')->to_string;
use YAML::PP;

package Game::EvonyTKR {
  use Mojo::Base 'Mojolicious', -role, -signatures;
  use Mojo::File::Share qw(dist_dir dist_file);
  use Carp;
  use Game::EvonyTKR::Logger::Config;
  our $VERSION = '0.0';

  # This method will run once at server start
  sub startup ($self) {
    my $distDir = Mojo::File::Share::dist_dir('Game-EvonyTKR');
    my $home = Mojo::Home->new;
    $home->detect;

    my $logConfig;
    if($self->mode() eq 'production') {
      $logConfig = $home->rel_file('share/log4perl.conf');
    }
    else {
      $logConfig = $home->rel_file('share/log4perl.development.conf');
    }

    $self->log( MojoX::Log::Log4perl->new($logConfig->to_string(), 10) );

    # Load configuration from config file
    my $config = $self->plugin('NotYAMLConfig' => {
      module  => 'YAML::PP',
    });

    # Configure the application
    $self->secrets($config->{secrets});

    $self->renderer->paths([$home->rel_file('share/templates')]);

    # Router
    my $r = $self->routes;

    push @{$self->plugins->namespaces}, 'Game::EvonyTKR::Plugin';

    $self->plugin('DefaultHelpers');
    $self->plugin('Route::Base');

  }

  sub preSeedGenerals($app, $dd) {
    my $generalDir = $dd->child('generals');
    my $generalGlob = $generalDir->to_string() . "/*.yaml";
    $app->log()->trace(sprintf('generalGlob pattern is %s', $generalGlob));
    my @generalFiles = glob($generalGlob);
    $app->log()->trace(sprintf('glob returned %d generals', scalar @generalFiles));
    foreach my $filename (@generalFiles) {
      if( -f $filename) {
        $app->log()->trace(sprintf('ready to read %s', $filename));
      }
    }
  }

}

1;
