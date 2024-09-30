use v5.40.0;
use utf8::all;
use experimental qw{class defer};
use FindBin;
use lib "$FindBin::Bin/../../../../lib";

package Game::EvonyTKR::Web::Root {
# ABSTRACT: Root configuration for Dancer2 for Game::EvonyTKR::Web
  use namespace::clean;
  use FindBin;
  use lib "$FindBin::Bin/../../../../lib";
  use Dancer2 appname => 'Game::EvonyTKR';
  use Dancer2::Plugin::REST;
  use Game::EvonyTKR::Web::General;
  use Game::EvonyTKR::Web::SkillBook;

  set engines => {
    serializer => {
      JSON => {
        allow_blessed   => 1,
        allow_nonref    => 1,
        allow_tags      => 1,
        canonical       => 1,
        convert_blessed => 1,
        max_depth       => 8,
        pretty          => 0,
        utf8            => 1,
      }
    }
  };

  set serializer   => 'JSON';
  set content_type => 'application/json';

  get '/' => sub {
    status_ok('success');
  };

}
true;

__END__
