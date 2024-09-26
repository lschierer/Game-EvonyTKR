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
  use Game::EvonyTKR::Logger::Config;
  use Util::Any -all;
  use namespace::autoclean;
  use MojoX::Log::Log4perl;
 
  field $category : reader : param = __CLASS__;

  field $logger : reader;

  field $systemLog: reader;

  field $confFile :reader;

  field @logLevels = qw(
    fatal
    error
    warn
    info
    debug
    trace
  );

  ADJUST {
    if (not defined $category) {
      $category = __CLASS__;
    }

  }


  
  method logInit($mode = 'production') {
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

    my $rootLog = File::Spec->catfile($logDir, 'root.log');
    if (!-e $rootLog) {
      File::Touch::touch($rootLog);
      chmod(0600, $rootLog);
    }
    my $logConf = Game::EvonyTKR::Logger::Config->new();
    $confFile = $logConf->path($mode);
    $logger = MojoX::Log::Log4perl->new($confFile, 10);
    $systemLog = Game::EvonyTKR::Logger->new();

  }
} 
1;
__END__

