use v5.40.0;
use utf8::all;
use Carp;
use Module::Build;

my $builder = Module::Build->new(
  module_name   => 'Game::EvonyTKR::Data',
  dist_author   => 'Luke Schierer <luke.schierer@gmail.com>',
  dist_abstract =>
'create (and provide) data files for consumption by other Game::EvonyTKR Modules',
  create_makefile_pl => 0,                          #'traditional',
  script_files       => 'bin/game-evonytkr-data',
  create_readme      => 0,
  recommends         => {
    'Log::Log4perl' => '0',
  },
  configure_requires => {
    'Module::Build' => '0.42'
  },
  build_requires => {
    'App::podweaver'                  => '0',
    'Pod::Weaver'                     => '0',
    'Pod::Weaver::Section::Leftovers' => '0',
    'Perl::Version'                   => '0',
    'Log::Any::Adapter'               => '0',
  },
  test_requires => {
    'Test::More'   => '0',
    'Perl::Critic' => '0',
  },
  # Add other metadata and dependencies here
  requires => {
    'App::Cmd::Simple'     => '0',
    'Carp'                 => '0',
    'Data::Printer'        => '0',
    'Devel::Local'         => '0',
    'File::FindLib'        => '0',
    'File::Share'          => '0',
    'GD'                   => '0',
    'HTML::TreeBuilder'    => '0',
    'HTTP::Tiny'           => '0',
    'IO::Socket::IP'       => '0',
    'IO::Socket::SSL'      => '0',
    'JSON::PP'             => '0',
    'List::AllUtils'       => '0',
    'List::Util'           => '0',
    'namespace::autoclean' => '0',
    'Path::Tiny'           => '0',
    'Readonly'             => '0',
    'Text::CSV'            => '0',
    'UUID'                 => '0',
    'X500::DN'             => '0',
    'X500::RDN'            => '0',
    'YAML::PP'             => '0',
    'namespace::autoclean' => '0',
    'namespace::clean'     => '0',
    'overload'             => '0',
    'utf8::all'            => '0',
  }
);

$builder->create_build_script;
