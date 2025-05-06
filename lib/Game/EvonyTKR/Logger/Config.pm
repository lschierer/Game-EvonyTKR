use v5.40.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
use File::HomeDir;
use File::ShareDir ':ALL';
use File::Spec;
use Util::Any -all;
use namespace::autoclean;

package Game::EvonyTKR::Logger::Config {
  use Carp;
  use File::FindLib 'lib';
  our $VERSION = 'v0.30.0';

  my @logLevels = qw(
    FATAL
    ERROR
    WARN
    INFO
    DEBUG
    TRACE
  );

  sub new ($class, $m = 'production') {
    say "Game::EvonyTKR::Logger::Config new sub";
    my $self = { mode => $m, };
    bless $self, $class;
  }

  sub path($self, $m = 'production') {
    my $confFile;
    if ($m ne $self->{mode}) {
      $self->{mode} = $m;
    }
    my $dd = File::ShareDir::dist_dir('Game-EvonyTKR');
    if ($self->{mode} eq 'production') {
      $confFile = File::Spec->catfile($dd, 'log4perl.conf');

    }
    else {
      my $mode = $self->{mode};
      $confFile = File::Spec->catfile($dd, "log4perl.$mode.conf");
    }
    if (!-T -s -r $confFile) {
      croak("$confFile does not exist");
    }
    return $confFile;
  }

  sub getLogDir {
    my $home   = File::HomeDir->my_home;
    my $logDir = File::Spec->catdir($home, 'var/log/Perl/dist/Game-Evony/');
    say "getLogDir returning $logDir";
    return $logDir;
  }

}
1;

__END__

# ABSTRACT: manage the log4perl configuration

=pod

=head1 DESCRIPTION

Configuration information for Log4Perl including methods for finding what directory
to write logs to, and for finding the log4perl configuration file.

=cut
