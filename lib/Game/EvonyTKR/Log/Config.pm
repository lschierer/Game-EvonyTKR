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

  sub logger {
    unless (Log::Log4perl->initialized()) {
      my $mode = $ENV{'MOJO_MODE'} // 'production';
      say "mode is $mode";
      my $userHome = File::HomeDir::Tiny::home();
      my @parts    = split '::', __PACKAGE__;
      my $base     = join '-', @parts[0 .. 1];
      my $logFile  = Mojo::File->new(
        sprintf('%s/var/log/Perl/dist/%s/root.log', $userHome, $base));
      # Create directory if needed
      $logFile->dirname->make_path({ mode => 0711 })
        unless -d $logFile->dirname;

      my $config = "log4perl.rootLogger = INFO, LOGFILE\n";
      $config .= "log4perl.appender.LOGFILE = Log::Log4perl::Appender::File\n";
      $config .= "log4perl.appender.LOGFILE.filename = $logFile\n";
      $config .= "log4perl.appender.LOGFILE.mode = append\n";
      $config .= "log4perl.appender.LOGFILE.utf8 = 1\n";
      $config .=
"log4perl.appender.LOGFILE.layout = Log::Log4perl::Layout::PatternLayout\n";
      $config .=
"log4perl.appender.LOGFILE.layout.ConversionPattern = [%p] %d (%C line %L) %m%n\n";
      my $levels = __PACKAGE__->logLevels();

      foreach my $package (keys %$levels) {
        my $level = $levels->{$package};
        $config .= "log4perl.logger.$package = $level\n";
      }

      Log::Log4perl->init(\$config);
    }

    my $l4p = Log::Log4perl->get_logger('Game::EvonyTKR');
    $l4p->info(sprintf('Logging initialized in %s', __PACKAGE__));
    my $testExternalCommonLog =
      Log::Log4perl->get_logger('Game::EvonyTKR::External::Common');
    $l4p->debug(
      sprintf('testExternalCommonLog is at log level %s',
        Log::Log4perl::Level::to_level($testExternalCommonLog->level()))
    );
    return $l4p;
  }

  #(ALL|FATAL|TRACE|DEBUG|WARN|OFF|ERROR|INFO)
  sub logLevels {
    return {
      'Game::EvonyTKR'                                    => 'DEBUG',
      'Game::EvonyTKR::Control::Generals::Routing'        => 'WARN',
      'Game::EvonyTKR::Controller::AscendingAttributes'   => 'INFO',
      'Game::EvonyTKR::Controller::ConflictGroups'        => 'WARN',
      'Game::EvonyTKR::Controller::ControllerBase'        => 'DEBUG',
      'Game::EvonyTKR::Controller::Covenants'             => 'WARN',
      'Game::EvonyTKR::Controller::Generals'              => 'DEBUG',
      'Game::EvonyTKR::Controller::Glossary'              => 'WARN',
      'Game::EvonyTKR::Controller::Pairs'                 => 'DEBUG',
      'Game::EvonyTKR::Controller::SkillBooks'            => 'INFO',
      'Game::EvonyTKR::Controller::Specialties'           => 'INFO',
      'Game::EvonyTKR::Converter'                         => 'WARN',
      'Game::EvonyTKR::External::Buff::Computer'          => 'DEBUG',
      'Game::EvonyTKR::External::Buff::Worker'            => 'DEBUG',
      'Game::EvonyTKR::External::Common'                  => 'INFO',
      'Game::EvonyTKR::External::General::Pair::Builder'  => 'INFO',
      'Game::EvonyTKR::External::General::Pair::Workflow' => 'INFO',
      'Game::EvonyTKR::External::Prebuild'                => 'INFO',
      'Game::EvonyTKR::Markdown::SpectrumHandler'         => 'WARN',
      'Game::EvonyTKR::Model::AscendingAttributes'        => 'WARN',
      'Game::EvonyTKR::Model::Book'                       => 'WARN',
      'Game::EvonyTKR::Model::Book::Builtin'              => 'WARN',
      'Game::EvonyTKR::Model::Book::Manager'              => 'WARN',
      'Game::EvonyTKR::Model::Book::SkillBook'            => 'INFO',
      'Game::EvonyTKR::Model::Buff'                       => 'WARN',
      'Game::EvonyTKR::Model::Covenant'                   => 'WARN',
      'Game::EvonyTKR::Model::Data'                       => 'WARN',
      'Game::EvonyTKR::Model::General'                    => 'WARN',
      'Game::EvonyTKR::Model::Glossary'                   => 'WARN',
      'Game::EvonyTKR::Model::Glossary::Manager'          => 'WARN',
      'Game::EvonyTKR::Model::Specialty'                  => 'WARN',
      'Game::EvonyTKR::Plugins::Markdown'                 => 'WARN',
      'Game::EvonyTKR::Plugins::Navigation'               => 'WARN',
      'Game::EvonyTKR::Plugins::StaticPages'              => 'WARN',
      'Game::EvonyTKR::Role::AutoTOJSON'                  => 'WARN',
      'Game::EvonyTKR::Shared::Logger'                    => 'WARN',
      'GitRepo::Reader'                                   => 'WARN',
      'PairBuilderLogic'                                  => 'WARN',
      'WorkerLogic'                                       => 'WARN',
    };
  }

}
1;
__END__
