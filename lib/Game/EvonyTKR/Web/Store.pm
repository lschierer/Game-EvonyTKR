use v5.40.0;
use utf8::all;
use experimental qw(class defer);
use FindBin;
use lib "$FindBin::Bin/../../../../lib";

package Game::EvonyTKR::Web::Store {
#ABSTRACT: Common in-memory data storage for the Dancer2 instance for the Game::EvonyTKR distribution
  use Carp;
  use Data::Printer;
  use Game::EvonyTKR::Logger;
  use Log::Log4perl;
  use Util::Any -all;
  use namespace::clean;

  my $store;

  sub _init_store {
    if (not defined($store)) {
      $store = {};
    }
  }

  sub get_store {
    if (not defined($store)) {
      _init_store();
    }
    return $store;
  }
}

true;
