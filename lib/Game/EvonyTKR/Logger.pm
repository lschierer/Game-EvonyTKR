use v5.40.0;
use experimental qw(class);
use utf8::all;

class Game::EvonyTKR::Logger {
# PODNAME: Game::EvonyTKR::Logger
# ABSTRACT: Set up and manage logging for the distribution
  use Carp;
  use Class::ISA;
  use Types::Common qw( t is_Num is_Str is_Int);
  use Type::Utils "is";
  use Util::Any -all;
  use List::MoreUtils;
  use File::ShareDir ':ALL';
  use File::Spec;
  use File::HomeDir;
  use File::Path qw(make_path);
  use File::Touch;
  use Log::Log4perl;
  use namespace::autoclean;

  field $logger :reader;

  field $location: reader;

  ADJUST {
    $location = $self->_getLogfileName();
    $self->_logInit();
    $logger = Log::Log4perl->get_logger("Game::EvonyTKR");
  }

  method _logInit() {
    my $conf = qq(
      log4perl.rootLogger                 = DEBUG, logFile

      log4perl.appender.logFile           = Log::Log4perl::Appender::File
      log4perl.appender.logFile.utf8      = 1
      log4perl.appender.logFile.filename  = $location
      log4perl.appender.logFile.mode      = append
      log4perl.appender.logFile.layout    = Log::Log4perl::Layout::PatternLayout
      log4perl.appender.logFile.layout.ConversionPattern = [%p] (%C line %L) %m%n

    );
    # ... passed as a reference to init()
    Log::Log4perl::init( \$conf );
  }

  method _getLogfileName($name = 'system.log') {
    my $home      = File::HomeDir->my_home;
    my $logDir    = File::Spec->catdir($home , 'var/log/Perl/dist/Game-Evony/');
    if(! -r -w  -x -o -d $logDir) {
        make_path($logDir,"0770");
      }
    my $logFile   = File::Spec->catfile($logDir, $name);
    if( ! -e $logFile) {
      touch($logFile);
      chmod(0600, $logFile);
    } 
    say "Logging to $logFile";
    return $logFile;
  }

}
1;
__END__

=method new()

instantiate the class. This will also configure the logging. 
=cut

=method logger()

returns the logger for use in logging messages. 
=cut
