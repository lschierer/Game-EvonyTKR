use v5.40.0;
use utf8::all;
use Carp;
use experimental qw(class);
use Data::Printer;
use File::ShareDir ':ALL';
use File::Spec;
use File::HomeDir;
use File::Path qw(make_path);
use File::Touch;
use YAML::XS qw{LoadFile Load};
use Util::Any -all;
use Devel::Peek;
use FindBin ':ALL';
use Log::Log4perl;
use namespace::autoclean;

package Game::EvonyTKR::REST;
use Dancer2;
use Dancer2::Plugin::REST;

prepare_serializer_for_format;

get '/' => sub {
  status_ok('success');
};

get '/generals' => sub {
  
  status_ok('success');
};

true;
