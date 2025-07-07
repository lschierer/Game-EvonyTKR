use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
use File::HomeDir;
require File::Share;
require Path::Tiny;
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

  sub path(
    $self,
    $m = 'production',
    $dd = File::Share::dist_dir('Game-EvonyTKR')
  ) {
    my $confFile;
    if ($m ne $self->{mode}) {
      $self->{mode} = $m;
    }
    if ($self->{mode} eq 'production') {
      $confFile = Path::Tiny::path($dd)->child('log4perl.conf');
    }
    else {
      my $mode = $self->{mode};
      $confFile = Path::Tiny::path($dd)->child("log4perl.$mode.conf");
    }
    if (!-T -s -r $confFile) {
      croak("$confFile does not exist");
    }
    return $confFile;
  }

  sub getLogDir {
    my $home = File::HomeDir->my_home;
    my $logDir =
      Path::Tiny::path($home)->child('var/log/Perl/dist/Game-Evony/');
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
