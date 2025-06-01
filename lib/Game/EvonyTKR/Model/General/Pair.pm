use v5.40.0;
use experimental qw(class);
use utf8::all;

class Game::EvonyTKR::Model::General::Pair :isa(Game::EvonyTKR::Model::Data) {
  field $primary : reader : param;
  field $secondary : reader : param;

}
1;
