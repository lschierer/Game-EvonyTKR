use v5.40.0;
use experimental qw(class);
use utf8::all;
use FindBin;
use lib "$FindBin::Bin/../../../../lib";

class Game::EvonyTKR::Web::Logger {
# PODNAME: Game::EvonyTKR::Logger
# ABSTRACT: Set up and manage logging for the distribution

  use Carp;
  use Data::Printer;
  use File::ShareDir ':ALL';
  use File::Spec;
  use File::HomeDir;
  use File::Path qw(make_path);
  use File::Touch;
  use Game::EvonyTKR::Logger;
  use MojoX::Log::Log4perl;
  use Util::Any -all;
  use namespace::autoclean;
 
  field $category : reader : param = __CLASS__;

  field $logger : reader;

  field $level :reader :param = 'info';

  field @logLevels = qw(
    fatal
    error
    warn
    info
    debug
    trace
  );

  my $internal_l4p = Game::EvonyTKR::Logger->new();

  ADJUST {
    if (not defined $category) {
      $category = __CLASS__;
    }
    if(any {$_ =~ /$level/i } @logLevels) {
      $level = uc($level);
    }
    else {
      croak("failed to initialize logging with invalid level $level");
    }
  }
  
  method logInit() {
    my $home   = File::HomeDir->my_home;
    my $logDir = File::Spec->catdir($home, 'var/log/Perl/dist/Game-Evony/');
    if (!-r -w -x -o -d $logDir) {
      make_path($logDir, "0770");
    }
    my $logFile = File::Spec->catfile($logDir, 'web.log');
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

    my %conf = (
      "log4perl.category.Game.EvonyTKR" => "$level, logFile2",
      "log4perl.category.Web"           => "$level, logFile",
      "log4perl.category.Dancer2"       => "$level, logFile",

      "log4perl.appender.logFile"          => "Log::Log4perl::Appender::File",
      "log4perl.appender.logFile.utf8"     => 1,
      "log4perl.appender.logFile.filename" => $logFile,
      "log4perl.appender.Logfile.DatePattern" => "yyyy-MM-dd",
      "log4perl.appender.Logfile.TZ"          => "UTC",
      "log4perl.appender.logFile.mode"        => "append",
      "log4perl.appender.logFile.layout"      =>
        "Log::Log4perl::Layout::PatternLayout",
      "log4perl.appender.logFile.layout.ConversionPattern" =>
        "[%p] %d (%C line %L) %m%n",

      "log4perl.appender.logFile2"          => "Log::Log4perl::Appender::File",
      "log4perl.appender.logFile2.utf8"     => 1,
      "log4perl.appender.logFile2.filename" => $logFile2,
      "log4perl.appender.logFile2.DatePattern" => "yyyy-MM-dd",
      "log4perl.appender.logFile2.TZ"          => "UTC",
      "log4perl.appender.logFile2.mode"        => "append",
      "log4perl.appender.logFile2.layout"      =>
        "Log::Log4perl::Layout::PatternLayout",
      "log4perl.appender.logFile2.layout.ConversionPattern" =>
        "[%p] %d (%C line %L) %m%n",
    );
    # ... passed as a reference to init()
    return Data::Printer::np %conf;
  }
} 
1;
__END__

