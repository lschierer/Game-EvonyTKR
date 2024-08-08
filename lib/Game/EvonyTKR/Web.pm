use v5.40.0;
use utf8::all;
use Carp;
use experimental qw(class);
use Data::Printer;
use File::Spec;
use File::ShareDir qw{ dist_dir dist_file };
use File::HomeDir;
use File::Path qw(make_path);
use File::Touch;
use YAML::XS qw{LoadFile Load};
use Util::Any -all;
use Devel::Peek;
use FindBin ':ALL';
use Log::Log4perl;
use Game::EvonyTKR::Web::General;

package Game::EvonyTKR::Web;
use Dancer2 appname => 'Game::EvonyTKR';

prepare_serializer_for_format;

get '/' => sub {
  status_ok('success');
};

prefix '/general' => sub {
  get ''  => sub {
    my $handler = Game::EvonyTKR::Web::General::get();
  };
};

true;
