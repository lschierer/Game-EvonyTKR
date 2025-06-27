use v5.40.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require YAML::PP;
require Game::EvonyTKR::Model::Logger;
require Game::EvonyTKR::Logger::Config;
#require Game::EvonyTKR::Controller::Root;
require Game::EvonyTKR::Controller::ControllerBase;
require Game::EvonyTKR::Model::EvonyTKR::Manager;
require GitRepo::Reader;

use namespace::clean;

package Game::EvonyTKR {
  use Mojo::Base 'Mojolicious', -strict, -signatures;
  use Mojo::File::Share qw(dist_dir );
  use Log::Log4perl;
  use Log::Log4perl::Config;
  Log::Log4perl::Config->utf8(1);
  use Carp;
  our $VERSION = 'v0.50.0';

  sub startup ($self) {
    Log::Log4perl::Config->utf8(1);
    my $distDir = dist_dir('Game::EvonyTKR');
    my $home    = Mojo::Home->new->detect;

    # Template and static paths
    push @{ $self->renderer->paths }, $distDir->child('templates')->to_string;
    push @{ $self->static->paths },   $distDir->child('public')->to_string;

    # Load YAML config
    my $config = $self->plugin('NotYAMLConfig' => { module => 'YAML::PP' });
    $self->secrets($config->{secrets});
    $self->plugin('DefaultHelpers');

    $self->defaults(layout => 'default');

    # Logging setup
    my $mode         = $self->mode;
    my $loggerConfig = Game::EvonyTKR::Logger::Config->new($mode);
    my $logConfig;

    eval {
      $logConfig = $loggerConfig->path($mode, $distDir);
      1;
    } or do {
      my $error = $@ || "Unknown error";
      $self->log->warn("Could not find Log4perl config: $error");
    };

    if ($logConfig && -f $logConfig) {
      say "init for log4perl at $logConfig";
      Log::Log4perl::init($logConfig);
      my $log4perl_logger = Log::Log4perl->get_logger('Game.EvonyTKR');

      my %logLevel = (
        development => 'ALL',
        production  => 'INFO',
      );

      $self->log->handle(undef);    # Disable default Mojo logger
      $self->log->level('debug');
      $self->log->on(
        message => sub ($log, $level, @lines) {
          my $msg = join "\n", @lines;
          $log4perl_logger->$level($msg) if $log4perl_logger->can($level);
        }
      );

      $self->log->info("✅ Log4perl initialized from $logConfig");
    }

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

  }
};

1;

__END__

#ABSTRACT: The main Mojolicious configuration, command, and control module

=pod

=head1 DESCRIPTION

this module contains the primary Mojolicious command, control and configuration.

=cut
