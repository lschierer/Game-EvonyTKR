package Game::EvonyTKR::Role::Logging;
use v5.42.0;
use experimental qw(class);
use utf8::all;
use Mojo::Base -role, -signatures;
use Log::Log4perl;
use Log::Log4perl::Level;
use Mojo::Loader      qw(find_modules);
use File::HomeDir::Tiny ();
require Data::Printer;
use List::AllUtils qw( uniq );
use Carp;

my $logLevelOverrides;

BEGIN {
  our $DEBUG_LOGGING = $ENV{LOG_DEBUG} // 0;
}

has logger => sub ($package) {
  return get_logger($package);
};

sub get_logger ($package) {
  my $pn = ref($package) ? blessed($package) : $package;

  # Strip __WITH__ role composition from package name
  $pn =~ s/__WITH__.+$//;


  my $l4p = Log::Log4perl->get_logger($pn);
  if(exists $logLevelOverrides->{$pn}){
    unless(Log::Log4perl::Level::to_level($l4p->level()) eq $logLevelOverrides->{$pn}) {
      $l4p->level($logLevelOverrides->{$pn});
    }
  }
  if ($Game::EvonyTKR::Role::Logging::DEBUG_LOGGING) {
    warn sprintf(
      'package "%s" is requesting a logger. Returning one with level %s',
      $pn, Log::Log4perl::Level::to_level($l4p->level())
    );
  }
  return $l4p;
}

sub debug_log_level ($caller) {
  return sprintf(
    'log level for %s is %s',
    ref($caller) ? ref($caller) : $caller,
    Log::Log4perl::Level::to_level($caller->logger->level())
  );
}

sub debug_log_category ($caller) {
  return sprintf(
    'log category for %s is %s',
    ref($caller) ? ref($caller) : $caller,
    $caller->logger->category()
  );
}

sub logFileLocation {
  my $mode = $ENV{'MOJO_MODE'} // 'production';
  warn "mode is $mode\n" if $Game::EvonyTKR::Role::Logging::DEBUG_LOGGING;
  my $userHome = File::HomeDir::Tiny::home;
  my @parts    = split '::', __PACKAGE__;
  my $base     = join '-', @parts[0 .. 1];
  my $logDir =
    Mojo::File->new(sprintf('%s/var/log/Perl/dist/%s', $userHome, $base));
  # Create directory if needed
  $logDir->make_path({ mode => 0711 })
    unless -d $logDir->to_abs->to_string;
  return $logDir;
}

sub appender_setup {
  my $logDir = __PACKAGE__->logFileLocation();
  my $config = qq(
    log4perl.rootLogger = WARN, LOGFILE
    log4perl.appender.LOGFILE = Log::Log4perl::Appender::File
    log4perl.appender.LOGFILE.filename = $logDir/app-$$.log
    log4perl.appender.LOGFILE.mode = append
    log4perl.appender.LOGFILE.utf8 = 1
    log4perl.appender.LOGFILE.layout = Log::Log4perl::Layout::PatternLayout
    log4perl.appender.LOGFILE.layout.ConversionPattern = [%p] %d (%C line %L) %m%n
  );
  return $config;
}

BEGIN {
  my $mode        = $ENV{'MOJO_MODE'} // 'production';
  my $defaultMode = $mode eq 'development' ? 'DEBUG' : 'WARN';

  #(ALL|FATAL|TRACE|DEBUG|INFO|WARN|ERROR|FATAL|OFF)
  $logLevelOverrides = {
    'Game::EvonyTKR'                                => 'DEBUG',
    'Game::EvonyTKR::Controller::Generals'          => 'DEBUG',
    'Game::EvonyTKR::External::General::Summarizer' => 'DEBUG',
    'Game::EvonyTKR::Model::AscendingAttributes'    => 'INFO',
    'Game::EvonyTKR::Model::BasicAttribute'         => 'INFO',
    'Game::EvonyTKR::Model::BasicAttributes'        => 'INFO',
    'Game::EvonyTKR::Model::Book'                   => 'INFO',
    'Game::EvonyTKR::Model::Buff'                   => 'WARN',
    'Game::EvonyTKR::Model::Buff::Value'            => 'WARN',
    'Game::EvonyTKR::Model::Covenant'               => 'INFO',
    'Game::EvonyTKR::Model::General'                => 'INFO',
    'Game::EvonyTKR::Model::Specialty'              => 'INFO',
    'Game::EvonyTKR::Shared::Logger'                => 'INFO',
    'Test::Package'                                 => 'TRACE',
    'Test'                                          => 'TRACE',
  };

  if ($Game::EvonyTKR::Role::Logging::DEBUG_LOGGING) {
    warn "Override keys: " . join(', ', keys %$logLevelOverrides) . "\n";
  }

  my $config = __PACKAGE__->appender_setup();

  my @packages = find_modules('Game::EvonyTKR', {recursive => 1});
  push @packages, keys %$logLevelOverrides;

  my @upn = sort {$a cmp $b} uniq @packages;

  foreach my $package (@upn) {
    $package = ref($package) ? blessed($package) : $package;
    if (exists $logLevelOverrides->{$package}){
      my $level = $logLevelOverrides->{$package};
      $config .= "log4perl.logger.$package = ${level}\n";
    }else {
      $config .= "log4perl.logger.$package = ${defaultMode}\n";
    }
  }

  unless(Log::Log4perl->initialized()){
    if ($Game::EvonyTKR::Role::Logging::DEBUG_LOGGING) {
      warn "=== INITIALIZING LOG4PERL ===\n";
      warn "Mode: $mode, Default: $defaultMode\n";
      warn "Config:\n$config\n";
      warn "=== END CONFIG ===\n";
    }
    Log::Log4perl->init(\$config);
  }

  state $wrapperRegistered = 0;
  unless($wrapperRegistered){
    if ($Game::EvonyTKR::Role::Logging::DEBUG_LOGGING) {
      warn sprintf('registering "%s" as a log4perl wrapper', __PACKAGE__) . "\n";
    }
    Log::Log4perl->wrapper_register(__PACKAGE__);
    $wrapperRegistered = 1;
  }
}

1;
__END__
