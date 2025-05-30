use v5.40.0;
use experimental qw(class);

class Game::EvonyTKR::Model::General::Pair {
  field $primary : reader : param;
  field $secondary : reader : param;

}
1;
