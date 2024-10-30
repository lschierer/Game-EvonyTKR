use v5.40.0;
use feature 'try';
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';

require MojoX::Log::Log4perl;
use YAML::PP;
use namespace::clean;

package Game::EvonyTKR {
  use Mojo::Base 'Mojolicious', -role, -signatures;
  use Mojo::File::Share qw(dist_dir dist_file);
  use Carp;
  use Game::EvonyTKR::Logger::Config;
  use File::FindLib 'lib';
  our $VERSION = 'v0.30.0';

  # This method will run once at server start
  sub startup ($self) {
    my $distDir = Mojo::File::Share::dist_dir('Game-EvonyTKR');
    my $home    = Mojo::Home->new;
    $home->detect;

    my $logConfig;
    if ($self->mode() eq 'production') {
      $logConfig = $home->rel_file('share/log4perl.conf');
    }
    else {
      $logConfig = $home->rel_file('share/log4perl.development.conf');
    }

    $self->log(MojoX::Log::Log4perl->new($logConfig->to_string(), 10));

    # Load configuration from config file
    my $config = $self->plugin(
      'NotYAMLConfig' => {
        module => 'YAML::PP',
      }
    );

    # Configure the application
    $self->secrets($config->{secrets});

    $self->renderer->paths([$home->rel_file('share/templates')]);

    # Router
    my $r = $self->routes;

    push @{ $self->plugins->namespaces }, 'Game::EvonyTKR::Plugin';

    $self->plugin('DefaultHelpers');
    $self->plugin('Route::Base');

  }

}

1;

__END__

#ABSTRACT: The main Mojolicious configuration, command, and control module

=pod

=head1 DESCRIPTION

this module contains the primary Mojolicious command, control and configuration.

=cut
