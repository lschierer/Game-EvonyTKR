use v5.40.0;
use feature 'try';
use experimental qw(class);
use utf8::all;
require Path::Tiny;

use YAML::PP;
require JSON::PP;
require File::Share;
require Log::Log4perl;
use namespace::clean;

package Game::EvonyTKR {
  use parent qw(App::Cmd::Simple);
  use namespace::autoclean;
  use Carp;
  use File::FindLib 'lib';
  use Game::EvonyTKR::Logger::Config;
  require Game::EvonyTKR::General::Importer;
  our $VERSION = 'v0.40.0';

  my $cg;
  my $ascension;
  my $generals;
  my $skillBooks;
  my $specialities;
  my $logger;

  sub opt_spec {
    return (
      ["output|o=s",    "output directory", { required => 1 }],
      ["conflicts|c=s", "conflicts file",   { required => 1 }],
      [
        "input|i=s", "input directory containing collections", { required => 1 }
      ],
      ["mode|m=s", "mode=production|development", { required => 0 }],
    );
  }
  my $logFile = File::Spec->catfile($logDir, 'dancer2.log');
  if (!-e $logFile) {
    File::Touch::touch($logFile);
    chmod(0600, $logFile);
  }
  my $SystemLogger = Game::EvonyTKR::Logger->new();
  my $logFile2     = $SystemLogger->getLogfileName();

  my %logLevel = (
    development => 'ALL',
    production  => 'INFO',
  );

  my $level = $logLevel{'production'};

  my %conf = (
    "log4perl.category.Game.EvonyTKR" => "$level, logFile2",

    "log4perl.appender.logFile"             => "Log::Log4perl::Appender::File",
    "log4perl.appender.logFile.utf8"        => 1,
    "log4perl.appender.logFile.filename"    => $logFile,
    "log4perl.appender.Logfile.DatePattern" => "yyyy-MM-dd",
    "log4perl.appender.Logfile.TZ"          => "UTC",
    "log4perl.appender.logFile.mode"        => "append",
    "log4perl.appender.logFile.layout"      =>
      "Log::Log4perl::Layout::PatternLayout",
    "log4perl.appender.logFile.layout.ConversionPattern" =>
      "[%p] %d (%C line %L) %m%n",

    "log4perl.appender.logFile2"             => "Log::Log4perl::Appender::File",
    "log4perl.appender.logFile2.utf8"        => 1,
    "log4perl.appender.logFile2.filename"    => $logFile2,
    "log4perl.appender.logFile2.DatePattern" => "yyyy-MM-dd",
    "log4perl.appender.logFile2.TZ"          => "UTC",
    "log4perl.appender.logFile2.mode"        => "append",
    "log4perl.appender.logFile2.layout"      =>
      "Log::Log4perl::Layout::PatternLayout",
    "log4perl.appender.logFile2.layout.ConversionPattern" =>
      "[%p] %d (%C line %L) %m%n",
  );
  # ... passed as a reference to init()
  Log::Log4perl::init(\%conf);
  return np %conf;
}

  sub validate_args {
    my ($self, $opt, $args) = @_;
    # no args allowed but options!
    $self->usage_error("No args allowed") if @$args;
    if (Path::Tiny::path($opt->{input})->exists()) {
      if (Path::Tiny::path($opt->{input})->is_dir()) {
        if (defined($logger)) {
          $logger->debug(
            "input directory $opt->{input} is in fact a directory.");
        }
      }
      else {
        croak("input $opt->{input} is not a directory");
      }
    }
    else {
      croak("input $opt->{input} does not exist");
    }
  }

  sub execute {
    my ($self, $opt, $args) = @_;

    my $distDir = Path::Tiny::path(File::Share::dist_dir('Game-EvonyTKR'));

    my $logConfig;
    if (exists $opt->{mode} && $opt->{mode} eq 'production') {
      $logConfig = $distDir->child('log4perl.conf');
    }
    else {
      $logConfig = $distDir->child('log4perl.development.conf');
    }

    Log::Log4perl::init_and_watch($logConfig->stringify());
    $logger = Log::Log4perl->get_logger('Game-EvonyTKR');
    $logger->info("startup");
    $logger->info('Logging based on $logConfig');

    $self->importer($opt->{input}, $opt->{conflicts});
  }

  sub importer ($self, $input, $conflicts) {
    $input = Path::Tiny::path($input);
    my $generalsDir = $input->child("generals");

    if (!$generalsDir->is_dir()) {
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
