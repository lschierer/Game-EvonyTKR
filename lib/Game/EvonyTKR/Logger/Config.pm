use v5.40.0;
use experimental qw(class);
use utf8::all;
use FindBin;
use lib "$FindBin::Bin/../../../../lib";
use namespace::autoclean;

package Game::EvonyTKR::Logger::Config {
# ABSTRACT: manage the log4perl configuration
  use Carp;
  use File::HomeDir;
  use File::ShareDir ':ALL';
  use File::Spec;
  use Util::Any -all;
  use namespace::autoclean;
# VERSION
  use FindBin;
  use lib "$FindBin::Bin/../../../../lib";

  my @logLevels = qw(
    FATAL
    ERROR
    WARN
    INFO
    DEBUG
    TRACE
  );

  sub new ($class, $m = 'production') {
    my $self = { mode => $m, };
    bless $self, $class;
  }

  sub path($self, $m = 'production') {
    my $confFile;
    if ($m ne $self->{mode}) {
      $self->{mode} = $m;
    }
    if ($self->{mode} eq 'production') {
      $confFile = File::Spec->catfile(File::ShareDir::dist_dir('Game-EvonyTKR'),
        'log4perl.conf');

    }
    else {
      my $mode = $self->{mode};
      $confFile = File::Spec->catfile(File::ShareDir::dist_dir('Game-EvonyTKR'),
        "log4perl.$mode.conf");
    }
    if (!-T -s -r $confFile) {
      croak("$confFile does not exist");
    }
    return $confFile;
  }

  sub getLogDir {
    my $home   = File::HomeDir->my_home;
    my $logDir = File::Spec->catdir($home, 'var/log/Perl/dist/Game-Evony/');
    return $logDir;
  }

}
1;
