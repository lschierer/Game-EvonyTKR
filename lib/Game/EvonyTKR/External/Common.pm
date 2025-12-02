use v5.40;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require File::Share;
require JSON::PP;
require YAML::PP;
require MIME::Base64;
require Path::Tiny;
require Game::EvonyTKR;
require Game::EvonyTKR::Shared::Constants;
require Game::EvonyTKR::Model::General;
require Game::EvonyTKR::Model::Book;

class Game::EvonyTKR::External::Common : isa(Game::EvonyTKR::Shared::Constants)
{
  use Unicode::Normalize;
  use Unicode::CaseFold qw(fc);
  use Encode            qw(is_utf8 decode_utf8 encode_utf8);
  use Carp;

  field $app : param : reader;
  field $tasks : reader = {};
  field $collectionDir;

  ADJUST {
    my $mh = Mojo::Home->new;
    $mh->detect('Game::EvonyTKR');
    $collectionDir =
      Mojo::File->new($mh->to_string)->child('share/collections/data/');
  }

  field $generals : reader = {};
  #there needs to be a reader so child classes can see it.
  field $conflictDetector : reader = Game::EvonyTKR::Service::Conflicts->new(
    build_index      => 1,
    asst_has_dragon  => 1,
    asst_has_spirit  => 1,
    allow_wall_buffs => 1,
  );

}
1;
