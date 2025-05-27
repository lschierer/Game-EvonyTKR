use v5.40.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
use YAML::PP;
use namespace::clean;
use Mojolicious::Plugin::DefaultHelpers;


package Game::EvonyTKR {
  use Mojo::Base 'Mojolicious', -role, -signatures;
  use Mojo::File::Share qw(dist_dir dist_file);
  require Game::EvonyTKR::Logger;
  require Game::EvonyTKR::Logger::Config;
  require Game::EvonyTKR::General::Importer;
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
    $self->log->debug("Mojo Home is $home");

    # Load configuration from config file
    my $config = $self->plugin(
      'NotYAMLConfig' => {
        module => 'YAML::PP',
      }
    );

    # Configure the application
    $self->secrets($config->{secrets});
    $self->plugin('DefaultHelpers');

    my $r = $self->routes;
    push @{$self->routes->namespaces}, 'Game::EvonyTKR::Controller';
    $r->any("/")->to('Root#index');

    my $SystemLogger = Game::EvonyTKR::Logger->new();
    my $logFile2     = $SystemLogger->getLogfileName();

    my %logLevel = (
      development => 'ALL',
      production  => 'INFO',
    );

    my $mode = $self->app->mode();
    say "starting with mode $mode";
    my $logConfig;
    if($mode eq 'production') {
      $logConfig = $distDir->child('log4perl.conf');
    }
    else {
      $logConfig = $distDir->child('log4perl.development.conf');
    }

    my $level = $logLevel{'production'};
    Log::Log4perl::init("$logConfig");

    Log::Log4perl::init_and_watch("$logConfig");
    $logger = Log::Log4perl->get_logger('Game-EvonyTKR');
    $logger->info("startup");
    $logger->info('Logging based on $logConfig');

    $self->importer($distDir);

    $self->start(@ARGV);

  }


  sub importer ($self, $distDir) {

    my $input = $distDir->child('collections');
    my $generalsDir = $input->child("generals");

    if (! -d $generalsDir) {
      croak("no generals collection available at $generalsDir");
    }

    my $gi = Game::EvonyTKR::General::Importer->new(inputDir => $generalsDir,);
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
