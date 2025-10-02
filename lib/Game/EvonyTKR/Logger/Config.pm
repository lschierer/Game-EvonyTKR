use v5.42.0;
use utf8::all;
use experimental qw(class);
use File::FindLib 'lib';
require File::Share;
use File::HomeDir;
require Path::Tiny;
require Log::Log4perl;
require Log::Log4perl::Config;

package Game::EvonyTKR::Logger::Config {
  use Carp;
  use Log::Log4perl qw(:levels);

  our $config_file;
  our $logger;
  our $dist_name = '';

  sub new ($class, $provided_dist_name) {
    unless (defined($provided_dist_name) && length($provided_dist_name)) {
      croak("distname is required!!");
    }
    my $self = {};
    $dist_name = $provided_dist_name;
    bless $self, $class;
  }

  sub get_config_file ($self, $mode = 'production') {

    my $dir = Path::Tiny::path(File::Share::dist_dir($dist_name));
    $config_file = $dir->child("log4perl.${mode}.conf");
    if (!-f -r $config_file) {
      Log::Log4perl->easy_init($ERROR);
      my $logger = Log::Log4perl->get_logger($dist_name);
      $logger->logcroak("$config_file does not exist or is not readable.");
    }
    return $config_file;
  }

  sub getLogDir {
    my $home    = File::HomeDir->my_home;
    my $segment = $dist_name =~ s/::/-/gr;
    my $logDir  = Path::Tiny::path($home)->child("/var/log/Perl/dist/$segment");
    return $logDir;
  }

  sub init ($self, $mode = 'production') {
    my $cf;
    if (Log::Log4perl->initialized()) {
      $logger->error("Log4Perl already Initialized!!!");
    }
    unless (defined $config_file) {
      $cf = $self->get_config_file($mode);
    }
    else {
      $cf = $config_file;
    }

    # set up the target directory
    my $target = $self->getLogDir();
    $target->mkdir({ mode => 0755 });

    if ($mode =~ /production/i) {
      Log::Log4perl::init_and_watch($config_file->absolute->canonpath, 10);
    }
    else {
      Log::Log4perl::init_and_watch($config_file->absolute->canonpath,20);
    }
    Log::Log4perl::Config->utf8(1);
    $logger = Log::Log4perl->get_logger($dist_name);
    $logger->info("logging initialized using $cf for mode $mode");
    return $logger;
  }

  sub get_logger ($self) {
    unless (defined $logger) {
      $self->init();
    }
    return $logger;
  }

}
1;
__END__
