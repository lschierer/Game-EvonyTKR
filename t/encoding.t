use v5.42.0;
use utf8::all;
use Test2::V0;
use Devel::Local;
use File::FindLib 'lib';
use Mojo::File;
require Data::Printer;
require YAML::PP;

require Game::EvonyTKR;
require Game::EvonyTKR::Log::Config;
use List::AllUtils qw( any none uniq );
use Carp;
use diagnostics;
use Sereal::Encoder;
use Sereal::Decoder;

my $logger = Game::EvonyTKR::Log::Config->logger();

my $home = Mojo::Home->new->detect('Game::EvonyTKR');

require Game::EvonyTKR::Model::Buff;

our $decoder = Sereal::Decoder->new();
our $encoder = Sereal::Encoder->new({
  canonical          => 1,
  no_shared_hashkeys => 1,
  refuse_objects     => 1
});

subtest 'BasicAttribute Encode/Decode' => sub {
  require Game::EvonyTKR::Model::BasicAttribute;

  my $ba = Game::EvonyTKR::Model::BasicAttribute->new(
    attribute_name  => 'attack',
    base            => 1.1,
    increment       => 1.2,
  );

  my $data = $encoder->encode($ba);
  ok($data, 'Encoding BasicAttribute');
  my $ba2 = $decoder->decode($data);
  ok(defined($ba2) &&
  $ba2->isa('Game::EvonyTKR::Model::BasicAttribute'),
    'Decoding BasicAttribute');

  done_testing();
};

subtest 'Buff Encode/Decode' => sub {
  require Game::EvonyTKR::Model::Buff;
  require Game::EvonyTKR::Model::Buff::Value;

  my $o = Game::EvonyTKR::Model::Buff->from_wire_hash({
    attribute => 'Attack',
    value     => {
      number  => 10,
      unit    => 'percentage',
    },
  });

  my $data = $encoder->encode($o);
  ok($data, 'Encoding Buff');
  my $o2 = $decoder->decode($data);
  ok(defined($o2) &&
  $o2->isa('Game::EvonyTKR::Model::Buff'),
    'Decoding Buff');

  done_testing();
};

subtest 'Book Encode/Decode' => sub {
  require Game::EvonyTKR::Model::Book;

  my $o = Game::EvonyTKR::Model::Book->from_wire_hash({
    _roles    => [
      'Game::EvonyTKR::Model::Role::Book::Builtin',
    ],
    name      => 'Attack Book',
    text      => 'This book provides a 10% Attack buff.',
    buffs     => [
      {
      attribute => 'Attack',
      value     => {
        number  => 10,
        unit    => 'percentage',
      },
      }
    ],
  });

  my $data = $encoder->encode($o);
  ok($data, 'Encoding Book');
  my $o2 = $decoder->decode($data);
  ok(defined($o2) &&
  $o2->isa('Game::EvonyTKR::Model::Book'),
    'Decoding Book');

  done_testing();
};

subtest 'Specialty Encode/Decode' => sub {
  require Game::EvonyTKR::Model::Specialty;
  my $dir = $home->child('share/collections/data/specialties');

  ok(-d -r $dir, 'Specialty Collection Directory Exists');

  my ($file) =  $dir->list->grep(sub { $_ =~ /\.ya?ml$/ && -f -r $_ })
  ->head(1)->each;

  ok(-f -r $file, 'Specialty Test File Exists');

  my $hashdata       = $file->slurp('UTF-8');
  my $hashObject = YAML::PP->new(
    schema       => [qw/ + Perl /],
    yaml_version => ['1.2', '1.1'],
  )->load_string($hashdata);

  my $o = Game::EvonyTKR::Model::Specialty->from_hash($hashObject);

  ok($o->isa('Game::EvonyTKR::Model::Specialty'), 'Creating Specialty with from_hash');

  my $o2 = Game::EvonyTKR::Model::Specialty->from_wire_hash({
    name      => 'Attack Specialty',
    id        => 'Attack Specialty',
    levels    => {
      none    => {
        text  => 'no buff',
        buffs => [],
      },
      green   => {
        text  => 'Provides a 1% Attack Buff.',
        buffs => [
          {
            attribute => 'Attack',
            value     => {
              number  => 1,
              unit    => 'percentage',
            },
          },
        ],
      },
    },
  });
  ok($o2->isa('Game::EvonyTKR::Model::Specialty'), 'Creating Specialty with from_wire_hash');

  my $data2 = $encoder->encode($o2);
  ok($data2, 'Encoding Specialty $o2');

  my $data1 = $encoder->encode($o);
  ok($data1, 'Encoding Specialty $o');

  my $o3 = $decoder->decode($data1);
  ok(defined($o3) &&
  $o3->isa('Game::EvonyTKR::Model::Specialty'),
    'Decoding Specialty $o');



  my $o4 = $decoder->decode($data2);
  ok(defined($o4) &&
  $o4->isa('Game::EvonyTKR::Model::Specialty'),
    'Decoding Specialty $o2');

  done_testing();
};

done_testing();
