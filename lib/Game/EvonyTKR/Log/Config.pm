use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require File::HomeDir::Tiny;
require File::HomeDir::Tiny;
require Mojo::File;

package Game::EvonyTKR::Log::Config {
  use Log::Log4perl;
  use Mojo::File::Share qw(dist_dir );
  use Carp;

  sub logFileLocation {
    my $mode = $ENV{'MOJO_MODE'} // 'production';
    say "mode is $mode";
    my $userHome = File::HomeDir::Tiny::home();
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

  sub logger {
    my $caller = shift // __PACKAGE__;
    my $levels = __PACKAGE__->logLevels();
    unless (Log::Log4perl->initialized()) {
      my $config = __PACKAGE__->appender_setup();
      foreach my $package (keys %$levels) {
        my $level = $levels->{$package};
        $config .= "log4perl.logger.$package = $level\n";
      }

      Log::Log4perl->init(\$config);
    }
    else {
      # Force re-initialization to override any auto-config
      foreach my $package (keys %$levels) {
        my $level = $levels->{$package};
        my $ll    = Log::Log4perl->get_logger($package);
        $ll->level(Log::Log4perl::Level::to_priority($level));
      }
    }

    my $l4p = Log::Log4perl->get_logger('Game::EvonyTKR');
    if (ref($caller)) {
      $l4p->WARN(sprintf(
        'Logging initialized in %s by %s',
        __PACKAGE__, ref($caller) ? blessed($caller) : $caller
      ));
    }
    return $l4p;
  }

  #(ALL|FATAL|TRACE|WARN|WARN|OFF|ERROR|WARN)
  sub logLevels {
    return {
      'Game::EvonyTKR'                                             => 'DEBUG',
      'Game::EvonyTKR::Control::Generals::Routing'                 => 'WARN',
      'Game::EvonyTKR::Controller::AscendingAttributes'            => 'WARN',
      'Game::EvonyTKR::Controller::ConflictGroups'                 => 'DEBUG',
      'Game::EvonyTKR::Controller::ControllerBase'                 => 'WARN',
      'Game::EvonyTKR::Controller::Covenants'                      => 'DEBUG',
      'Game::EvonyTKR::Controller::Generals'                       => 'WARN',
      'Game::EvonyTKR::Controller::Glossary'                       => 'WARN',
      'Game::EvonyTKR::Controller::Pairs'                          => 'DEBUG',
      'Game::EvonyTKR::Controller::Role::AscendingAttributes'      => 'WARN',
      'Game::EvonyTKR::Controller::Role::Books'                    => 'WARN',
      'Game::EvonyTKR::Controller::Role::Generals'                 => 'WARN',
      'Game::EvonyTKR::Controller::Role::Pairs'                    => 'DEBUG',
      'Game::EvonyTKR::Controller::Role::Specialties'              => 'WARN',
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
      'Game::EvonyTKR::External::General::Loader'                  => 'WARN',
      'Game::EvonyTKR::External::General::LoadAll'                 => 'WARN',
      'Game::EvonyTKR::External::General::Pair::CreatePairs'       => 'DEBUG',
      'Game::EvonyTKR::External::General::Pair::ReduceBatch '      => 'DEBUG',
      'Game::EvonyTKR::External::General::Pair::ReduceCoordinator' => 'DEBUG',
      'Game::EvonyTKR::External::General::Pair::Summarizer'        => 'WARN',
      'Game::EvonyTKR::External::General::Pair::Workflow'          => 'WARN',
      'Game::EvonyTKR::External::JobBase'                          => 'WARN',
      'Game::EvonyTKR::External::Prebuild'                         => 'DEBUG',
      'Game::EvonyTKR::External::Specialties::LoadAllSpecialties'  => 'WARN',
      'Game::EvonyTKR::External::Specialties::Loader'              => 'WARN',
      'Game::EvonyTKR::Log::Config'                                => 'WARN',
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
      'Game::EvonyTKR::Model::General::Conflict'                   => 'DEBUG',
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
      'Game::EvonyTKR::Plugins::Markdown'                          => 'WARN',
      'Game::EvonyTKR::Plugins::Navigation'                        => 'WARN',
      'Game::EvonyTKR::Plugins::StaticPages'                       => 'WARN',
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
      'Game::EvonyTKR::Role::Logger'                               => 'WARN',
      'Game::EvonyTKR::Service::Cache'                             => 'WARN',
      'Game::EvonyTKR::Shared::Constants'                          => 'WARN',
      'Game::EvonyTKR::Shared::Logger'                             => 'WARN',
      'Game::EvonyTKR::Shared::Parser'                             => 'WARN',
      'Game::EvonyTKR::Util::Buff::Matcher'                        => 'WARN',
      'Game::EvonyTKR::Util::Buff::Summarizer'                     => 'WARN',
      'GitRepo::Reader'                                            => 'WARN',
      'LinkChecker::Command'                                       => 'WARN',
      'Log::Any::Adapter::PerPackage'                              => 'WARN',
      'PairBuilderLogic'                                           => 'WARN',
      'Test::Package'                                              => 'DEBUG',
      'WorkerLogic'                                                => 'WARN',
    };
  }

}
1;
__END__
