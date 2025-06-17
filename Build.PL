use v5.40.0;
use utf8::all;
use Carp;
use Module::Build;

my $builder = Module::Build->new(
    module_name         => 'Game::EvonyTKR',
    license             => 'ISC',
    dist_author         => 'Luke Schierer <luke.schierer@gmail.com>',
    dist_abstract       => 'Display and Evaluate information about Generals, Buffs, and DeBuffs for the Evony TKR Game',
    create_makefile_pl  => 0,     #'traditional',
    script_files        => 'scripts/game-evonytkr',
    create_readme       => 0,
    requires            => {
      'App::Cmd::Simple'            => '0',
      'Carp'                        => '0',
      'Cwd'                         => '0',
      'Data::Printer'               => '0',
      'File::FindLib'               => '0',
      'File::HomeDir'               => '0',
      'File::Path'                  => '0',
      'File::Share'                 => '0',
      'File::ShareDir'              => '0',
      'File::Spec'                  => '0',
      'File::Touch'                 => '0',
      'Hash::Util'                  => '0',
      'JSON::MaybeXS'               => '0',
      'JSON::PP'                    => '0',
      'List::AllUtils'              => '0',
      'List::MoreUtils'             => '0',
      'Log::Dispatch::FileRotate'   => '0',
      'Log::Log4perl'               => '0',
      'Log::Log4perl::Level'        => '0',
      'Math::Round'                 => '0',
      'MCE::Flow'                   => '0',
      'Module::Find'                => '0',
      'Module::Load'                => '0',
      'Mojo::Base'                  => '0',
      'Mojo::File'                  => '0',
      'Mojo::File::Share'           => '0',
      'Mojolicious::Commands'       => '0',
      'Mojolicious::Plugin::Module::Loader' => '0',
      'Mojolicious::Routes'         => '0',
      'Mojolicious::Routes::Route'  => '0',
      'Path::Iterator::Rule'        => '0',
      'Path::Tiny'                  => '0',
      'Readonly'                    => '0',
      'Role::Tiny'            => '0',
      'Role::Tiny::With'            => '0',
      'Scalar::Util'                => '0',
      'Sereal'                      => '0',
      'Sereal::Decoder'             => '0',
      'Sereal::Encoder'             => '0',
      'Storable'                    => '0',
      'Text::CSV'		    => '0',
      'Text::MultiMarkdown'         => '0',
      'Type::Tiny::Enum'            => '0',
      'Type::Utils'                 => '0',
      'Types::Common'               => '0',
      'Types::Standard'             => '0',
      'Util::Any'                   => '0',
      'UUID'                        => '0',
      'X500::DN'                    => '0',
      'X500::RDN'                   => '0',
      'YAML::PP'                    => '0',
      'YAML::XS'                    => '0',
      'lib::relative'               => '0',
      'namespace::autoclean'        => '0',
      'namespace::clean'            => '0',
      'overload'                    => '0',
      'utf8::all'                   => '0',
    },
    recommends          => {
      'Log::Log4perl'         => '0',
    },
    configure_requires => {
      'Module::Build'   => '0.42'
    },
    build_requires      => {
      'App::podweaver'                  => '0',
      'Pod::Weaver'                     => '0',
      'Pod::Weaver::Section::Leftovers' => '0',
      'Perl::Version'                   => '0',
      'Log::Any::Adapter'               => '0',
    },
    test_requires       => {
      'Test::More'    => '0',
      'Perl::Critic'  => '0',
    },
    meta_merge => {
        resources => {
            repository => 'https://github.com/szabgab/App',
            bugtracker => 'https://github.com/szabgab/App/issues'
        }
    },
);

$builder->create_build_script();
