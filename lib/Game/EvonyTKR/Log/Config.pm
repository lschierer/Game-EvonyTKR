use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require File::HomeDir::Tiny;

package Game::EvonyTKR::Log::Config {

  sub getLogDir {
    my @parts = split '::', __PACKAGE__;
    my $base = join '-', @parts[0..1];
    return sprintf('%s/var/log/Perl/dist/%s/', File::HomeDir::Tiny::home(), $base );
  }

  sub logLevels {
    return {
    'Game::EvonyTKR'                                  => 'DEBUG',
    'Game::EvonyTKR::Role::AutoTOJSON'                => 'WARN',
    'Game::EvonyTKR::Plugins::StaticPages'            => 'WARN',
    'Game::EvonyTKR::Plugins::Markdown'               => 'WARN',
    'Game::EvonyTKR::Plugins::Navigation'             => 'WARN',
    'Game::EvonyTKR::Markdown::SpectrumHandler'       => 'WARN',
    'Game::EvonyTKR::Shared::Logger'                  => 'WARN',
    'Game::EvonyTKR::Converter'                       => 'WARN',
    'Game::EvonyTKR::Controller::SkillBooks'          => 'INFO',
    'Game::EvonyTKR::Controller::Pairs'               => 'WARN',
    'Game::EvonyTKR::Controller::Covenants'           => 'WARN',
    'Game::EvonyTKR::Controller::Generals'            => 'INFO',
    'Game::EvonyTKR::Controller::ConflictGroups'      => 'WARN',
    'Game::EvonyTKR::Controller::AscendingAttributes' => 'INFO',
    'Game::EvonyTKR::Controller::Specialties'         => 'INFO',
    'Game::EvonyTKR::Controller::ControllerBase'      => 'WARN',
    'Game::EvonyTKR::Controller::Glossary'            => 'WARN',
    'Game::EvonyTKR::External::Conflicts::Worker'     => 'WARN',
    'Game::EvonyTKR::External::Buff::Worker'          => 'WARN',
    'Game::EvonyTKR::External::Common'                => 'INFO',
    'Game::EvonyTKR::External::Prebuild'              => 'INFO',
    'Game::EvonyTKR::Model::AscendingAttributes'      => 'WARN',
    'Game::EvonyTKR::Model::Buff'                     => 'WARN',
    'Game::EvonyTKR::Model::Book::SkillBook'          => 'INFO',
    'Game::EvonyTKR::Model::Book::Builtin'            => 'WARN',
    'Game::EvonyTKR::Model::Book::Manager'            => 'WARN',
    'Game::EvonyTKR::Model::Glossary::Manager'        => 'WARN',
    'Game::EvonyTKR::Model::Glossary'                 => 'WARN',
    'Game::EvonyTKR::Model::Data'                     => 'WARN',
    'Game::EvonyTKR::Model::General'                  => 'WARN',
    'Game::EvonyTKR::Model::Specialty'                => 'WARN',
    'Game::EvonyTKR::External::General::PairBuilder'  => 'DEBUG',
    'Game::EvonyTKR::External::Prebuild'              => 'INFO',
    'PairBuilderLogic'                                => 'WARN',
    'WorkerLogic'                                     => 'WARN',
    'Game::EvonyTKR::Control::Generals::Routing'      => 'WARN',
    'GitRepo::Reader'                                 => 'WARN',
    };
  }

}
1;
__END__
