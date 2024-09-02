use v5.40.0;
use utf8::all;
use experimental qw{class defer};
use FindBin;
use lib "$FindBin::Bin/../../../../lib";

package Game::EvonyTKR::Web::General::Pair {
# ABSTRACT: Route Handler for the /general/pair route.
  use Carp;
  use Data::Printer;
  use Devel::Peek;
  use File::HomeDir;
  use File::Path     qw{ make_path };
  use File::ShareDir qw{ dist_dir dist_file };
  use File::Spec;
  use File::Touch;
  use Game::EvonyTKR::Ascending;
  use Game::EvonyTKR::General::Pair;
  use Game::EvonyTKR::General::Pair::Creator;
  use Game::EvonyTKR::General::Ground;
  use Game::EvonyTKR::General::Mounted;
  use Game::EvonyTKR::General::Ranged;
  use Game::EvonyTKR::General::Siege;
  use Game::EvonyTKR::SkillBook::Special;
  use Game::EvonyTKR::Speciality;
  use Game::EvonyTKR::Web::Store;
  use Log::Log4perl;
  use Util::Any -all;
  use YAML::XS qw{ LoadFile Load };
  use namespace::clean;
  use Dancer2 appname => 'Game::EvonyTKR';
  use Dancer2::Plugin::REST;

  my $store;
  my $logger = Log::Log4perl::get_logger('Web::General');

  sub _init {
    $store = Game::EvonyTKR::Web::Store::get_store();
    if(not exists $$store{'pairs'} ) {
      ${$store->{'pairs'}} = {};
      if(exists $$store{'generals'}) {
        if(scalar %{$store->{'generals'}} >= 2 ) {
          my $pairCreator = Game::EvonyTKR::General::Pair::Creator->new();

          #todo: figure out conflicts

          $pairCreator->set_source(%{$store->{'generals'}});
          my %pairs = $pairCreator.getPairs();
          while (my ($key, $value) = each %pairs) {
            if(exists $store->{'generals'}->{$key}) {
                $store->{pairs}->{$key} = $value;
            }
            else {
              $logger->debug("not using key $key when inializing pairs.");
            }
          }
        }
        else {
          $logger->debug("cannot create pairs with only one general");
        }
      }
      else {
        $logger->debug("no generals loaded yet")
      }
    }
  }

  prefix '/pair' => sub {
    get '/list'                 => \&_list;
    get '/:primary/:secondary'  => \&_specificPair;
  };

  sub _list {
    _init();
    my @list;
    while (my ($key, $value) = each %{$store->{'pairs'}}) {
      push @list, $value;
    }
    return \@list;
  }

  sub __specificPair {
    _init();

  }
}

true;
