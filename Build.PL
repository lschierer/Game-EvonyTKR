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
    script_files        => 'script/game-evonytkr',
    create_readme       => 0,
    requires            => {
      'Carp'                                    => '0',
      'Data::Printer'                           => '0',
      'File::FindLib'                           => '0',
      'File::HomeDir'                           => '0',
      'File::ShareDir'                          => '0',
      'File::Path'                              => '0',
      'File::Spec'                              => '0',
      'File::Touch'                             => '0',
      'HTML::FromANSI'                          => '0',
      'JSON::Schema::Modern'                    => '0',
      'JSON::Schema::Modern::Document::OpenAPI' => '0',
      'List::MoreUtils'                         => '0',
      'Mojo::Base'                              => '0',
      'Mojo::File'                              => '0',
      'Mojo::File::Share'                       => '0',
      'Mojo::JSON'                              => '0',
      'Mojolicious::Commands'                   => '0',
      'Mojolicious::Plugin::DefaultHelpers'     => '0',
      'Mojolicious::Plugin::OpenAPI::Modern'    => '0',
      'MojoX::Model'                            => '0',
      'MojoX::MIME::Types'                      => '0',
      'OpenAPI::Modern'                         => '0',
      'Path::Tiny'                              => '0',
      'Role::Tiny'                              => '0',
      'Sereal::Decoder'                         => '0',
      'Sereal::Encoder'                         => '0',
      'Type::Utils'                             => '0',
      'Types::Common'                           => '0',
      'Types::Common::Numeric'                  => '0',
      'Types::Standard'                         => '0',
      'UUID'                                    => '0',
      'Util::Any'                               => '0',
      'X500::DN'                                => '0',
      'X500::RDN'                               => '0',
      'YAML::PP'                                => '0',
    },
    recommends          => {
      'Log::Log4perl'         => '0',
      'MojoX::Log::Log4perl'  => '0',
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
      'Test::Mojo'  => '0',
      'Test::More'  => '0',
    },
    meta_merge => {
        resources => {
            repository => 'https://github.com/szabgab/App',
            bugtracker => 'https://github.com/szabgab/App/issues'
        }
    },
);

$builder->create_build_script();
