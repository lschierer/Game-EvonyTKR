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
use namespace::autoclean;

package Game::EvonyTKR::REST;
use Dancer2;
use Dancer2::Plugin::REST;

prepare_serializer_for_format;

get '/' => sub {
  status_ok('success');
};

get '/generals' => sub {
  my %generals = read_generals();
  status_ok('success');
};

sub read_generals() {
  info 'starting read_generals';
  my $general_share = File::Spec->catfile(File::ShareDir::dist_dir('Game-EvonyTKR'), 'generals');

  my @found = grep { -T -s -r } glob("$general_share/*.yaml");
  my $message = "general_share: " . scalar @found;
  info "general_share '$general_share' contained " . scalar @found . " generals";
  
}

true;
