use v5.40.0;
use experimental qw(class);
use utf8::all;
use Util::Any -all;
use List::MoreUtils;
use File::ShareDir ':ALL';
use File::HomeDir;
require Path::Tiny;
use Log::Log4perl;
use namespace::autoclean;

class Game::EvonyTKR::Model::Logger {
# PODNAME: Game::EvonyTKR::Model::Logger
  use Carp;
  our $VERSION = 'v0.30.0';

  field $location : reader;

  field $logger : reader;

  field $category : reader : param = __CLASS__;

  ADJUST {
    $self->get_logger();
  }

  method get_logger() {
    if (not defined $category) {
      $category = __CLASS__;
    }
    $logger = Log::Log4perl->get_logger($category);
    return $logger;
  }

  method getLogfileName($name = 'system.log') {
    my $home = File::HomeDir->my_home;
    my $logDir =
      Path::Tiny::path($home)->child('var/log/Perl/dist/Game-Evony/');
    if (!-r -w -x -o -d $logDir) {
      $logDir = Path::Tiny::path($logDir);
      $logDir->mkdir({ mode => 0770 });
    }
    my $logFile = $logDir->child($name);
    if (!$logFile->is_file()) {
      $logFile->touch();
      $logFile->chmod(0600);
    }
    return $logFile;
  }

  method toHashRef {
    return {};
  }

  method TO_JSON {
    return $self->toHashRef();
  }

}
1;

__END__

# ABSTRACT: Set up and manage logging for the distribution

=pod

=head1 DESCRIPTION

This is intended as an abstract class of sorts from which to inherit uniform methods
for using logging.

=cut

=head1 METHODS

=method new()

instantiate the class. This will also configure the logging.

=cut

=method logger()

returns the logger for use in logging messages.

=cut
=cut
