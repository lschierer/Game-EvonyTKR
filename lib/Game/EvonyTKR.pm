use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require YAML::PP;
require Game::EvonyTKR::Model::Logger;
require Game::EvonyTKR::Logger::MojoLog4Perl;
require Game::EvonyTKR::Logger::Config;
require Log::Log4perl;

#require Game::EvonyTKR::Controller::Root;
require Game::EvonyTKR::Controller::ControllerBase;
require Game::EvonyTKR::Model::EvonyTKR::Manager;
require GitRepo::Reader;

package Game::EvonyTKR {
  use Mojo::Base 'Mojolicious', -strict, -signatures;
  use Mojo::File::Share qw(dist_dir );
  use Carp;
  our $VERSION = 'v0.50.0';

  sub startup ($self) {

    my $config  = $self->plugin('NotYAMLConfig' => { module => 'YAML::PP' });
    my $distDir = dist_dir('Game::EvonyTKR');
    my $mode    = $self->mode;
    $self->config(distDir => $distDir);
    my $home = Mojo::Home->new->detect;

    # Template and static paths
    push @{ $self->renderer->paths }, $distDir->child('templates')->to_string;
    push @{ $self->static->paths },   $distDir->child('public')->to_string;

    # Load YAML config

    $self->secrets($config->{secrets});
    $self->plugin('DefaultHelpers');

    $self->defaults(layout => 'default');

    # Logging setup

    my $lc              = Game::EvonyTKR::Logger::Config->new('Game-EvonyTKR');
    my $log4perl_logger = $lc->init($mode);
    my $app_log         = Game::EvonyTKR::Logger::MojoLog4Perl->new(
      l4p => Log::Log4perl->get_logger('Game-EvonyTKR'),);
    $self->log($app_log);

    $self->helper(
      logger => sub ($c, $cat) {
        if (length($cat) == 0) {
          $self->log->error('got a logger request with zero length cat!');
          $cat = 'Game-EvonyTKR-Unknown';
        }
        else {
          $self->log->info("got a cat '$cat'");
        }
        Log::Log4perl::Config->utf8(1);
        my $logger = Log::Log4perl->get_logger($cat);
        return $logger;
      }
    );

    $self->log->info("Mojolicious Logging initialized");
    my $RootManager =
      Game::EvonyTKR::Model::EvonyTKR::Manager->new(SourceDir => $distDir,);

    my $RepoData = GitRepo::Reader->new(source_dir => $distDir,);

    $self->helper(get_root_manager => sub { return $RootManager });
    $self->helper(get_repo_data    => sub { return $RepoData });

    # Instantiate and attach shared model manager
    # Run rootImport once on first dispatch
    $self->hook(
      before_server_start => sub {
        state $initialized = do {
          $self->log->info("⚙️  Running rootImport...");
          $RootManager->rootImport();
          $self->log->info("✅ rootImport completed.");
          $self->plugins->emit(evonytkrtips_initialized => $RootManager);
          1;
        };
      }
    );
    # Set namespaces
    push @{ $self->routes->namespaces },  'Game::EvonyTKR::Controller';
    push @{ $self->plugins->namespaces }, 'Game::EvonyTKR::Plugins';
    push @{ $self->plugins->namespaces }, 'Game::EvonyTKR::Controller';
    push @{ $self->preload_namespaces },  'Game::EvonyTKR::Controller';

    # Register infrastructure plugins in specific order

    # First Plugins that provide helpers but do not define routes
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
    $self->plugin('Game::EvonyTKR::Plugins::StaticPages')
      ;    # Register last for lowest priority

    # configure to tell it that I will be behind an ELB/ALB.
    #$self->reverse_proxy(1);
  }
};

1;

__END__

#ABSTRACT: The main Mojolicious configuration, command, and control module

=pod

=head1 DESCRIPTION

this module contains the primary Mojolicious command, control and configuration.

=cut
