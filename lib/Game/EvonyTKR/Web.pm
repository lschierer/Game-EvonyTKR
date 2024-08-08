use v5.40.0;
use utf8::all;
use experimental qw(class);

use Log::Log4perl;
use Game::EvonyTKR::Web::General;

package Game::EvonyTKR::Web {
  use Carp;
  use Util::Any -all;
  use namespace::autoclean;
  use Dancer2 appname => 'Game::EvonyTKR';

  get '/' => sub {
    status_ok('success');
  };

  prefix '/general' => sub {
    get ''  => sub {
      my $handler = Game::EvonyTKR::Web::General::get();
    };
  };

}

true;
