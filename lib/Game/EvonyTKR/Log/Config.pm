use v5.42.0;
use utf8::all;

package Game::EvonyTKR::Log::Config;
use Mojo::Base -role, -signatures;
use Log::Log4perl;
use Log::Log4perl::Level;
require File::HomeDir::Tiny;
require Data::Printer;
use Carp;

has logger => sub ($self) {
  my $ec = ref($self) ? ref($self) : $self;
  return __PACKAGE__->get_logger($ec);
};

state $rl;

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

sub get_effective_caller {
  my $depth = 1;
  while (my $caller = caller($depth++)) {
    # Ignore known non-class contexts (e.g., eval)
    next if $caller =~ /^(eval|main)$/;

    # Return first valid class found
    if ($caller->can('logger')) {
      return $caller unless ($caller eq 'Game::EvonyTKR::Log::Config');
    }
  }
  # Fallback to a default strategy
  return blessed(shift) || ref(shift);
}

sub logFileLocation {
  my $mode = $ENV{'MOJO_MODE'} // 'production';
  say "mode is $mode";
  my $userHome = File::HomeDir::Tiny::home();
  my @parts    = split '::', __PACKAGE__;
  my $base     = join '-', @parts[0 .. 2];
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

#(ALL|FATAL|TRACE|WARN|WARN|OFF|ERROR|WARN)
our $logLevels = {
  'Game::EvonyTKR'                                             => 'DEBUG',
  'Game::EvonyTKR::Control::Generals::Routing'                 => 'DEBUG',
  'Game::EvonyTKR::Controller::AscendingAttributes'            => 'WARN',
  'Game::EvonyTKR::Controller::ConflictGroups'                 => 'WARN',
  'Game::EvonyTKR::Controller::ControllerBase'                 => 'WARN',
  'Game::EvonyTKR::Controller::Covenants'                      => 'WARN',
  'Game::EvonyTKR::Controller::Generals'                       => 'DEBUG',
  'Game::EvonyTKR::Controller::Glossary'                       => 'WARN',
  'Game::EvonyTKR::Controller::Pairs'                          => 'WARN',
  'Game::EvonyTKR::Controller::Root'                           => 'DEBUG',
  'Game::EvonyTKR::Controller::Role::AscendingAttributes'      => 'WARN',
  'Game::EvonyTKR::Controller::Role::Books'                    => 'WARN',
  'Game::EvonyTKR::Controller::Role::Generals'                 => 'WARN',
  'Game::EvonyTKR::Controller::Role::Pairs'                    => 'WARN',
  'Game::EvonyTKR::Controller::Role::Specialties'              => 'WARN',
  'Game::EvonyTKR::Controller::Role::Covenants'                => 'WARN',
  'Game::EvonyTKR::Controller::SkillBooks'                     => 'WARN',
  'Game::EvonyTKR::Controller::Specialties'                    => 'WARN',
  'Game::EvonyTKR::Converter'                                  => 'WARN',
  'Game::EvonyTKR::Converter::AscendingAttributes'             => 'WARN',
  'Game::EvonyTKR::Converter::Covenant'                        => 'WARN',
  'Game::EvonyTKR::Converter::General'                         => 'WARN',
  'Game::EvonyTKR::Converter::Helpers'                         => 'WARN',
  'Game::EvonyTKR::Converter::Images'                          => 'WARN',
  'Game::EvonyTKR::Converter::SkillBook'                       => 'WARN',
  'Game::EvonyTKR::Converter::Specialty'                       => 'WARN',
  'Game::EvonyTKR::External::AscendingAttributes::LoadAll'     => 'WARN',
  'Game::EvonyTKR::External::AscendingAttributes::Loader'      => 'WARN',
  'Game::EvonyTKR::External::Book::LoadAllBuiltins'            => 'WARN',
  'Game::EvonyTKR::External::Book::LoadAllGenerics'            => 'WARN',
  'Game::EvonyTKR::External::Book::Loader'                     => 'WARN',
  'Game::EvonyTKR::External::Book::Loader'                     => 'WARN',
  'Game::EvonyTKR::External::Book::Loader'                     => 'WARN',
  'Game::EvonyTKR::External::Buff::Computer'                   => 'WARN',
  'Game::EvonyTKR::External::Buff::Worker'                     => 'WARN',
  'Game::EvonyTKR::External::Common'                           => 'WARN',
  'Game::EvonyTKR::External::Covenant::LoadAll'                => 'WARN',
  'Game::EvonyTKR::External::Covenant::Loader'                 => 'WARN',
  'Game::EvonyTKR::External::General::LoadAll'                 => 'WARN',
  'Game::EvonyTKR::External::General::Pair::CreatePairs'       => 'WARN',
  'Game::EvonyTKR::External::General::Pair::ReduceBatch '      => 'WARN',
  'Game::EvonyTKR::External::General::Pair::ReduceCoordinator' => 'WARN',
  'Game::EvonyTKR::External::General::Pair::Summarizer'        => 'WARN',
  'Game::EvonyTKR::External::General::Pair::Workflow'          => 'WARN',
  'Game::EvonyTKR::External::JobBase'                          => 'WARN',
  'Game::EvonyTKR::External::Prebuild'                         => 'WARN',
  'Game::EvonyTKR::External::Specialties::LoadAllSpecialties'  => 'WARN',
  'Game::EvonyTKR::External::Specialties::Loader'              => 'WARN',
  'Game::EvonyTKR::Log::Config'                                => 'INFO',
  'Game::EvonyTKR::Markdown::SpectrumHandler'                  => 'WARN',
  'Game::EvonyTKR::Model::AscendingAttributes'                 => 'WARN',
  'Game::EvonyTKR::Model::BasicAttribute'                      => 'WARN',
  'Game::EvonyTKR::Model::BasicAttributes'                     => 'WARN',
  'Game::EvonyTKR::Model::Book'                                => 'WARN',
  'Game::EvonyTKR::Model::Book::Builtin'                       => 'WARN',
  'Game::EvonyTKR::Model::Book::Manager'                       => 'WARN',
  'Game::EvonyTKR::Model::Book::SkillBook'                     => 'WARN',
  'Game::EvonyTKR::Model::Buff'                                => 'WARN',
  'Game::EvonyTKR::Model::Buff::Matcher'                       => 'WARN',
  'Game::EvonyTKR::Model::Buff::Summarizer'                    => 'WARN',
  'Game::EvonyTKR::Model::Buff::Value'                         => 'WARN',
  'Game::EvonyTKR::Model::Covenant'                            => 'WARN',
  'Game::EvonyTKR::Model::Data'                                => 'WARN',
  'Game::EvonyTKR::Model::General'                             => 'WARN',
  'Game::EvonyTKR::Model::General::Conflict'                   => 'WARN',
  'Game::EvonyTKR::Model::General::Conflict::Book'             => 'WARN',
  'Game::EvonyTKR::Model::General::ConflictGroup'              => 'WARN',
  'Game::EvonyTKR::Model::General::Importer'                   => 'WARN',
  'Game::EvonyTKR::Model::General::Pair'                       => 'WARN',
  'Game::EvonyTKR::Model::General::Pair::Manager'              => 'WARN',
  'Game::EvonyTKR::Model::Glossary'                            => 'WARN',
  'Game::EvonyTKR::Model::Glossary::Manager'                   => 'WARN',
  'Game::EvonyTKR::Model::Logger'                              => 'WARN',
  'Game::EvonyTKR::Model::Role::Book'                          => 'WARN',
  'Game::EvonyTKR::Model::Role::Book::Builtin'                 => 'WARN',
  'Game::EvonyTKR::Model::Role::Book::SkillBook'               => 'WARN',
  'Game::EvonyTKR::Model::Specialty'                           => 'WARN',
  'Game::EvonyTKR::Plugins::Navigation'                        => 'WARN',
  'Game::EvonyTKR::Role::StaticPages'                          => 'DEBUG',
  'Game::EvonyTKR::Role::AutoTOJSON'                           => 'WARN',
  'Game::EvonyTKR::Role::BasicAttribute'                       => 'WARN',
  'Game::EvonyTKR::Role::BasicAttributes'                      => 'WARN',
  'Game::EvonyTKR::Role::Buff'                                 => 'WARN',
  'Game::EvonyTKR::Role::Buff::Value'                          => 'WARN',
  'Game::EvonyTKR::Role::Common'                               => 'WARN',
  'Game::EvonyTKR::Role::Constants::AscendingAttributes'       => 'WARN',
  'Game::EvonyTKR::Role::Constants::BuffConstants'             => 'WARN',
  'Game::EvonyTKR::Role::Constants::GeneralConstants'          => 'WARN',
  'Game::EvonyTKR::Role::Constants::Specialties'               => 'WARN',
  'Game::EvonyTKR::Role::General'                              => 'WARN',
  'Game::EvonyTKR::Log::Config'                                => 'WARN',
  'Game::EvonyTKR::Role::MarkdownRenderer'                     => 'WARN',
  'Game::EvonyTKR::Service::Cache'                             => 'WARN',
  'Game::EvonyTKR::Shared::Constants'                          => 'WARN',
  'Game::EvonyTKR::Shared::Logger'                             => 'WARN',
  'Game::EvonyTKR::Shared::Parser'                             => 'WARN',
  'Game::EvonyTKR::Util::Buff::Matcher'                        => 'WARN',
  'Game::EvonyTKR::Util::Buff::Summarizer'                     => 'WARN',
  'GitRepo::Reader'                                            => 'WARN',
  'LinkChecker::Command'                                       => 'WARN',
  'PairBuilderLogic'                                           => 'WARN',
  'Test::Package'                                              => 'WARN',
  'WorkerLogic'                                                => 'WARN',
};

sub get_logger ($class, $caller = undef) {
  state $I_Have_Init;
  $caller //= 'undef::package';

  my $mode    = $ENV{'MOJO_MODE'} // 'production';
  my $default = $mode eq 'development' ? 'DEBUG' : 'WARN';

  # MUST initialize Log4perl BEFORE any logging calls
  unless (Log::Log4perl->initialized()) {
    $I_Have_Init = 1;
    my $config = __PACKAGE__->appender_setup();
    foreach my $package (keys %$logLevels) {
      my $level = $logLevels->{$package};
      $config .= "log4perl.logger.$package = $level\n";
    }
    Log::Log4perl->init(\$config);
  }
  elsif (!$I_Have_Init) {
    $I_Have_Init = 1;
    # Force re-initialization to override any auto-config from another source
    foreach my $package (keys %$logLevels) {
      my $level = $logLevels->{$package};
      my $ll    = Log::Log4perl->get_logger($package);
      $ll->level(Log::Log4perl::Level::to_priority($level));
    }
  }

  # NOW safe to get loggers and log - Log4perl is initialized
  my $l4p = Log::Log4perl->get_logger($class);
  $class = ref($class) ? blessed($class) : $class;
  my $ll =
    exists $logLevels->{$class}
    ? Log::Log4perl::Level::to_priority($logLevels->{$class})
    : $default;
  $l4p->level($ll);
  $l4p->debug(
    sprintf('in %s, $class is %s, $caller is %s', __PACKAGE__, $class, $caller)
  );

  my $cl = Log::Log4perl->get_logger($caller);
  $l4p->debug(sprintf(
    'returning logger in %s, $class is %s, for %s at level %s',
    __PACKAGE__, $class,
    $caller,     Log::Log4perl::Level::to_level($cl->level()),
  ));
  return $cl;
}

sub debug { shift->_fwd(debug => @_) }
sub info  { shift->_fwd(info  => @_) }
sub warn  { shift->_fwd(warn  => @_) }
sub error { shift->_fwd(error => @_) }
sub fatal { shift->_fwd(fatal => @_) }

# (optional) Mojolicious also calls ->trace in some versions
sub trace { shift->_fwd(trace => @_) }    # map to debug if you want

sub _fwd {
  my ($self, $level, @lines) = @_;
  my $msg = join('', map { ref($_) ? "$_" : $_ } @lines);
  $self->logger->$level($msg);
  return $self;
}

1;
    __END__
