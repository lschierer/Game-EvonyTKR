use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;

package Game::EvonyTKR::Shared::Logger {
  use Sys::Syslog;
  use Log::Fast;
  use File::HomeDir::Tiny ();
  use Mojo::File;
  use Carp;

  our @EXPORT_OK = qw( get_logger );
  our $lfConfig;

  BEGIN {
    my $home    = Mojo::File->new(File::HomeDir::Tiny::home);
    my $logPath = $home->child('/var/log/Perl/dist/Game-EvonyTKR/root.log');
    $logPath->dirname->make_path({ mode => 0711 });
    $logPath->touch;
    my $fh = $logPath->open('>>');
    $fh->autoflush(1);
    $lfConfig = {
      level  => 'DEBUG',
      type   => 'fh',
      fh     => $fh,
      prefix => '[%L] %D %T %P %F: ',
    };
  }

  # Possible values are: 'ERR', 'WARN', 'NOTICE', 'INFO', 'DEBUG'
  my $module_log_masks = {
    'Game::EvonyTKR'                                  => 'DEBUG',
    'Game::EvonyTKR::Role::AutoTOJSON'                => 'WARN',
    'Game::EvonyTKR::Plugins::StaticPages'            => 'WARN',
    'Game::EvonyTKR::Plugins::Markdown'               => 'WARN',
    'Game::EvonyTKR::Plugins::Navigation'             => 'WARN',
    'Game::EvonyTKR::Markdown::SpectrumHandler'       => 'WARN',
    'Game::EvonyTKR::Shared::Logger'                  => 'WARN',
    'Game::EvonyTKR::Converter'                       => 'WARN',
    'Game::EvonyTKR::Controller::SkillBooks'          => 'WARN',
    'Game::EvonyTKR::Controller::Pairs'               => 'WARN',
    'Game::EvonyTKR::Controller::Covenants'           => 'WARN',
    'Game::EvonyTKR::Controller::Generals'            => 'WARN',
    'Game::EvonyTKR::Controller::ConflictGroups'      => 'WARN',
    'Game::EvonyTKR::Controller::AscendingAttributes' => 'DEBUG',
    'Game::EvonyTKR::Controller::Specialties'         => 'WARN',
    'Game::EvonyTKR::Controller::ControllerBase'      => 'WARN',
    'Game::EvonyTKR::Controller::Glossary'            => 'WARN',
    'Game::EvonyTKR::External::Conflicts::Worker'     => 'WARN',
    'Game::EvonyTKR::External::Buff::Worker'          => 'WARN',
    'Game::EvonyTKR::External::Prebuild'              => 'WARN',
    'Game::EvonyTKR::Model::Book::SkillBook'          => 'WARN',
    'Game::EvonyTKR::Model::Book::Builtin'            => 'WARN',
    'Game::EvonyTKR::Model::Book::Manager'            => 'WARN',
    'Game::EvonyTKR::Model::Glossary::Manager'        => 'WARN',
    'Game::EvonyTKR::Model::Glossary'                 => 'WARN',
    'Game::EvonyTKR::Model::Data'                     => 'WARN',
    'Game::EvonyTKR::Model::General'                  => 'WARN',
    'Game::EvonyTKR::External::General::PairBuilder'  => 'INFO',
    'Game::EvonyTKR::External::Prebuild'              => 'INFO',
    'PairBuilderLogic'                                => 'WARN',
    'ConflictWorkerLogic'                             => 'WARN',
    'WorkerLogic'                                     => 'WARN',
    'Game::EvonyTKR::Control::Generals::Routing'      => 'WARN',
    'GitRepo::Reader'                                 => 'WARN',
  };

  sub get_logger ($caller) {
    my $log_level = $module_log_masks->{$caller} // 'WARN';
    my $logger    = Log::Fast->new($lfConfig);
    $logger->config({
      level => $log_level,
    });
    return $logger;
  }

}
1;
__END__
