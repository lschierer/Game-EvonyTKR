use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require YAML::PP;
require Minion::Backend::SQLite;
require Mojolicious::Plugin::Minion;

#require Game::EvonyTKR::Controller::Root;
require Game::EvonyTKR::Controller::ControllerBase;
require Game::EvonyTKR::External::JobBase;
require Game::EvonyTKR::External::Buff::Worker;
require Game::EvonyTKR::External::Prebuild;
require Game::EvonyTKR::Log::Config;

require GitRepo::Reader;

package Game::EvonyTKR {
  use Mojo::Base 'Mojolicious', -strict, -signatures;
  use Mojo::File::Share qw(dist_dir );
  use Log::Log4perl;
  use Log::Any::Adapter;
  use Carp;
  use Env qw(DEPLOYMENT_TIME HOSTNAME IMAGE_TAG IMAGE_URI);
  our $VERSION = 'v0.50.0';

  sub startup ($self) {

    my $config  = $self->plugin('NotYAMLConfig' => { module => 'YAML::PP' });
    my $distDir = dist_dir('Game::EvonyTKR');
    my $mode    = $self->mode;
    $self->config(APP_START_TIME => time());
    Env::import();
    $self->config(distDir => $distDir);
    $self->config(
      'EvonyTKR-Environment' => {
        DEPLOYMENT_TIME => $DEPLOYMENT_TIME,
        HOSTNAME        => $HOSTNAME,
        IMAGE_TAG       => $IMAGE_TAG,
        IMAGE_URI       => $IMAGE_URI,
      }
    );
    my $home = Mojo::Home->new->detect;

    # Template and static paths
    push @{ $self->renderer->paths }, $distDir->child('templates')->to_string;
    push @{ $self->static->paths },   $distDir->child('public')->to_string;

    # Load YAML config

    $self->secrets($config->{secrets});
    $self->plugin('DefaultHelpers');

    $self->defaults(layout => 'default');

    # Logging setup
    my $l4p = Game::EvonyTKR::Log::Config->logger();
    Log::Any::Adapter->set('Log4perl');

    $self->plugin('Log::Any' => { logger => 'Log::Log4perl' });

    $self->log->info(sprintf('Mojolicious Logging initialized'));

    my $RepoData = GitRepo::Reader->new(source_dir => $distDir,);

    $self->helper(get_repo_data => sub { return $RepoData });

    foreach my $envkey (keys %{ $self->config->{'EvonyTKR-Environment'} }) {
      if (defined $envkey) {
        my $envValue = $self->config->{'EvonyTKR-Environment'}->{$envkey}
          // 'Undefined';
        $self->log->info("EvonyTKR-Environnment variable $envkey is $envValue");
      }
      else {
        $self->log->warn('undefined envkey in EvonyTKR-Environment!');
      }
    }

    # Set namespaces
    push @{ $self->routes->namespaces },  'Game::EvonyTKR::Controller';
    push @{ $self->plugins->namespaces }, 'Game::EvonyTKR::Plugins';
    push @{ $self->plugins->namespaces }, 'Game::EvonyTKR::Controller';
    push @{ $self->preload_namespaces },  'Game::EvonyTKR::Controller';

    # Register infrastructure plugins in specific order

    # First Plugins that provide helpers but do not define routes
    my $dbPath = Mojo::File->new('minion.db');

    $self->log->debug("dbPath is $dbPath");
    $self->plugin(Minion => { SQLite => "sqlite:$dbPath" });
    if ($mode eq 'development') {
      $self->minion->remove_after(7200);
    }
    # Minion worker
    $self->plugin('Game::EvonyTKR::External::Buff::Worker');

    #I need the admin dashboard to debug, but its a risk in production
    if ($self->mode eq 'development') {
      $self->plugin('Minion::Admin');
    }

    $self->plugin('Game::EvonyTKR::External::Prebuild');
    # Markdown
    $self->plugin('Game::EvonyTKR::Plugins::Markdown');
    # Navigation
    $self->plugin('Game::EvonyTKR::Plugins::Navigation');

    # Then Controller Plugins
    $self->plugin(
      'Module::Loader' => {
        plugin_namespaces => ['Game::EvonyTKR::Controller']
      }
    );

    # Last the Static Pages
    # Register last for lowest priority
    $self->plugin('Game::EvonyTKR::Plugins::StaticPages');

    # configure to tell it that I will be behind an ELB/ALB.
    #$self->reverse_proxy(1);
    Mojo::IOLoop->next_tick(sub ($ioloop) {
      if (Scalar::Util::blessed($self) eq 'Game::EvonyTKR') {
        $self->log->info('mojo_worker_started');
        $self->plugins->emit(mojo_worker_started => { app => $self });
      }
    });
  }
};

1;

__END__

#ABSTRACT: The main Mojolicious configuration, command, and control module

=pod

=head1 DESCRIPTION

this module contains the primary Mojolicious command, control and configuration.

=cut
