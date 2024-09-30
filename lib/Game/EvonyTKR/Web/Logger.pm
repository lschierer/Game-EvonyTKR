use v5.40.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';

class Game::EvonyTKR::Web::Logger : isa(Game::EvonyTKR::Logger) {
# PODNAME: Game::EvonyTKR::Logger

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
# VERSION
  use File::FindLib 'lib';
  use MojoX::Log::Log4perl;

  field $webLogger : reader;

  field $confFile : reader;

  field @logLevels = qw(
    fatal
    error
    warn
    info
    debug
    trace
  );

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
    $confFile  = $logConf->path($mode);
    $webLogger = MojoX::Log::Log4perl->new($confFile, 10);
    $self->get_logger();
  }
}
1;
__END__

# ABSTRACT: Set up and manage logging for the distribution
