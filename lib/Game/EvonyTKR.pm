use v5.40.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require YAML::PP;
require Game::EvonyTKR::Model::Logger;
require Game::EvonyTKR::Logger::Config;
require Game::EvonyTKR::Plugins::ControllerBase;
require Game::EvonyTKR::Plugins::CollectionBase;

use namespace::clean;

package Game::EvonyTKR {
  use Mojo::Base 'Mojolicious', -strict, -signatures;
  use Mojo::File::Share qw(dist_dir dist_file);
  use Log::Log4perl;
  use Log::Log4perl::Config;
  Log::Log4perl::Config->utf8(1);

  use Carp;
  our $VERSION = 'v0.50.0';

  my $cg;
  my $ascension;
  my $generals;
  my $skillBooks;
  my $specialities;
  my $logger;

  sub startup ($self) {
    my $distDir = Mojo::File::Share::dist_dir('Game::EvonyTKR');
    my $home    = Mojo::Home->new;
    $home->detect;
    Log::Log4perl::Config->utf8(1);
    $self->log->debug("Mojo Home is $home");
    $self->log->debug("Distribution directory is $distDir");

# Set up paths for templates, layouts, and static files from the distribution directory
# Configure template paths
    push @{ $self->renderer->paths }, $distDir->child('templates')->to_string;

    # Configure static file paths
    push @{ $self->static->paths }, $distDir->child('public')->to_string;

    # Load configuration from config file
    my $config = $self->plugin(
      'NotYAMLConfig' => {
        module => 'YAML::PP',
      }
    );

    # Configure the application
    $self->secrets($config->{secrets});
    $self->plugin('DefaultHelpers');

    $self->defaults(layout => 'default');

    my $r = $self->routes;
    push @{ $self->routes->namespaces }, 'Game::EvonyTKR::Controller';
    push @{ $self->routes->namespaces }, 'Game::EvonyTKR::Plugins';
    $r->any("/")->to('Root#index');

    my %logLevel = (
      development => 'ALL',
      production  => 'INFO',
    );

    my $mode = $self->app->mode();
    say "starting with mode $mode";

    # Use the Logger::Config module to get the log configuration path

    my $loggerConfig = Game::EvonyTKR::Logger::Config->new($mode);
    my $logConfig;

    # Use traditional Perl error handling with eval
    eval {
      $logConfig = $loggerConfig->path($mode, $distDir);
      1;    # Ensure success
    } or do {
      my $error = $@ || "Unknown error";
      $self->log->warn("Could not find Log4perl config: $error");
    };

    if (defined $logConfig && -f $logConfig) {
      $self->log->info("Using Log4perl config from $logConfig");

      # Initialize Log4perl with the config file

      Log::Log4perl::init($logConfig);

      # Get the logger instance
      $logger = Log::Log4perl->get_logger('Game.EvonyTKR');

      # Configure Mojo to use Log4perl
      $self->log->handle(undef);
      $self->log->level('debug');
      $self->log->on(
        message => sub {
          my ($log, $level, @lines) = @_;
          my $message = join "\n", @lines;

          # Map Mojo log levels to Log4perl levels
          if    ($level eq 'trace') { $logger->trace($message) }
          elsif ($level eq 'debug') { $logger->debug($message) }
          elsif ($level eq 'info')  { $logger->info($message) }
          elsif ($level eq 'warn')  { $logger->warn($message) }
          elsif ($level eq 'error') { $logger->error($message) }
          elsif ($level eq 'fatal') { $logger->fatal($message) }
        }
      );

      $logger->info("startup");
      $logger->info("Logging configured with $logConfig");
    }
    push @{ $self->plugins->namespaces }, 'Game::EvonyTKR::Plugins';
    push @{ $self->preload_namespaces },  'Game::EvonyTKR::Plugins';
    $self->plugin(
      'Module::Loader' => {
        plugin_namespaces => ['Game::EvonyTKR::Plugins']
      }
    );

    $self->start(@ARGV);

  }

  sub importer ($self, $distDir) {

    my $input       = $distDir->child('collections');
    my $generalsDir = $input->child("generals");

    if (!-d $generalsDir) {
      croak("no generals collection available at $generalsDir");
    }

    my $gi =
      Game::EvonyTKR::Model::General::Importer->new(inputDir => $generalsDir,);
    $generals = $gi->importAll();
    my $json =
      JSON::PP->new()
      ->utf8()
      ->pretty()
      ->indent()
      ->canonical()
      ->convert_blessed();
    $logger->info("imported generals: " . $json->encode($generals));
  }

};

1;

__END__

#ABSTRACT: The main Mojolicious configuration, command, and control module

=pod

=head1 DESCRIPTION

this module contains the primary Mojolicious command, control and configuration.

=cut
