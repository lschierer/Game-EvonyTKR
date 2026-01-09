package Game::EvonyTKR::Role::Logging;
use v5.42.0;
use experimental qw(class);
use utf8::all;
use Moo::Role;
use Log::Handler;
use File::HomeDir::Tiny ();
require Data::Printer;
use List::AllUtils qw( uniq );
use Carp;

my $logLevelOverrides;

BEGIN {
  our $DEBUG_LOGGING = $ENV{LOG_DEBUG} // 0;
}

has logger => (
  is => 'ro',
  lazy => 1,
  default => sub {
    my ($self) = @_;
    return Log::Handler->create_logger($self);
  }
);

##############################################################################
# Logging wrapper methods that use caller() to get the actual logging package
# This allows both roles and classes to have their own log level control
##############################################################################

sub log_trace ($self, @msg) {
  my $caller_package = caller(0);
  my $logger         = get_logger($caller_package);
  $logger->trace(@msg);
}

sub log_debug ($self, @msg) {
  my $caller_package = caller(0);
  my $logger         = get_logger($caller_package);
  $logger->debug(@msg);
}

sub log_info ($self, @msg) {
  my $caller_package = caller(0);
  my $logger         = get_logger($caller_package);
  $logger->info(@msg);
}

sub log_warn ($self, @msg) {
  my $caller_package = caller(0);
  my $logger         = get_logger($caller_package);
  $logger->warn(@msg);
}

sub log_error ($self, @msg) {
  my $caller_package = caller(0);
  my $logger         = get_logger($caller_package);
  $logger->error(@msg);
}

sub log_fatal ($self, @msg) {
  my $caller_package = caller(0);
  my $logger         = get_logger($caller_package);
  $logger->fatal(@msg);
}

sub log_logcroak ($self, @msg) {
  my $caller_package = caller(0);
  my $logger         = get_logger($caller_package);
  $logger->logcroak(@msg);
}

# Check if log levels are enabled
sub is_trace ($self) {
  my $caller_package = caller(0);
  my $logger         = get_logger($caller_package);
  return $logger->is_trace();
}

sub is_debug ($self) {
  my $caller_package = caller(0);
  my $logger         = get_logger($caller_package);
  return $logger->is_debug();
}

sub is_info ($self) {
  my $caller_package = caller(0);
  my $logger         = get_logger($caller_package);
  return $logger->is_info();
}

sub is_warn ($self) {
  my $caller_package = caller(0);
  my $logger         = get_logger($caller_package);
  return $logger->is_warn();
}

sub is_error ($self) {
  my $caller_package = caller(0);
  my $logger         = get_logger($caller_package);
  return $logger->is_error();
}

sub is_fatal ($self) {
  my $caller_package = caller(0);
  my $logger         = get_logger($caller_package);
  return $logger->is_fatal();
}

sub get_logger ($package) {
  my $pn = ref($package) ? blessed($package) : $package;

  # Strip __WITH__ role composition from package name
  $pn =~ s/__WITH__.+$//;

  my $l = Log::Handler->create_logger($pn);

  return $l;
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


BEGIN {
  my $mode        = $ENV{'MOJO_MODE'} // 'production';
  my $defaultMode = $mode eq 'development' ? 'DEBUG' : 'WARN';

  #(ALL|FATAL|TRACE|DEBUG|INFO|WARN|ERROR|FATAL|OFF)
  $logLevelOverrides = {
    'Game::EvonyTKR'                                              => 'DEBUG',
    'Game::EvonyTKR::Controller::Books'                           => 'WARN',
    'Game::EvonyTKR::Controller::Generals'                        => 'WARN',
    'Game::EvonyTKR::Controller::Pairs'                           => 'DEBUG',
    'Game::EvonyTKR::External::AscendingAttributes'               => 'WARN',
    'Game::EvonyTKR::External::General::Summarizer'               => 'DEBUG',
    'Game::EvonyTKR::External::General::Pair::Summarizer'         => 'DEBUG',
    'Game::EvonyTKR::External::General::Pair::LoadAllPairBuilder' => 'DEBUG',
    'Game::EvonyTKR::External::General::Pair::ReduceCoordinator'  => 'DEBUG',
    'Game::EvonyTKR::External::General::Pair::CreatePairs'        => 'DEBUG',
    'Game::EvonyTKR::External::General::ComputeBuffCache'         => 'INFO',
    'Game::EvonyTKR::External::General::Pair::BatchSummarizer'    => 'DEBUG',
    'Game::EvonyTKR::External::Prebuild'                          => 'DEBUG',
    'Game::EvonyTKR::External'                                    => 'WARN',
    'Game::EvonyTKR::Model::Buff'                                 => 'WARN',
    'Game::EvonyTKR::Model::Buff::Matcher'                        => 'WARN',
    'Game::EvonyTKR::Model::Buff::Summarizer'                     => 'WARN',
    'Game::EvonyTKR::Model::Buff::Value'                          => 'WARN',
    'Game::EvonyTKR::Model::General'                              => 'WARN',
    'Game::EvonyTKR::Model'                                       => 'WARN',
    'Game::EvonyTKR::Plugins::Navigation'                         => 'WARN',
    'Game::EvonyTKR::Role::Persistence::Core'                     => 'WARN',
    'Game::EvonyTKR::Role::Persistence::Pairs'                    => 'DEBUG',
    'Game::EvonyTKR::Role::Persistence'                           => 'WARN',
    'Game::EvonyTKR::Role'                                        => 'WARN',
    'Game::EvonyTKR::Service::DynamoDBPersistence'                => 'WARN',
    'Game::EvonyTKR::Service::Persistence'                        => 'INFO',
    'Game::EvonyTKR::Service'                                     => 'WARN',
    'Game::EvonyTKR::Shared::Logger'                              => 'WARN',
    'Test::Package'                                               => 'TRACE',
    'Test'                                                        => 'TRACE',
    'LinkChecker'                                                 => 'DEBUG',
  };

  if ($Game::EvonyTKR::Role::Logging::DEBUG_LOGGING) {
    warn "Override keys: " . join(', ', keys %$logLevelOverrides) . "\n";
  }

}

1;
__END__
