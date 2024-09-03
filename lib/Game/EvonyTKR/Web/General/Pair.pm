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
  use Game::EvonyTKR::Buff::Data;
  use Game::EvonyTKR::General::Pair;
  use Game::EvonyTKR::General::Pair::Creator;
  use Game::EvonyTKR::General::Ground;
  use Game::EvonyTKR::General::Mounted;
  use Game::EvonyTKR::General::Ranged;
  use Game::EvonyTKR::General::Siege;
  use Game::EvonyTKR::SkillBook::Special;
  use Game::EvonyTKR::Speciality;
  use Game::EvonyTKR::Web::General;
  use Game::EvonyTKR::Web::Store;
  use Log::Log4perl;
  use Util::Any -all;
  use YAML::XS qw{ LoadFile Load };
  use namespace::clean;
  use Dancer2 appname => 'Game::EvonyTKR';
  use Dancer2::Plugin::REST;

  my $store;
  my $pairs;
  my $generals;
  my $data = Game::EvonyTKR::Buff::Data->new();
  my $logger = Log::Log4perl::get_logger('Web::General');

  prefix '/generals'              => sub {
    prefix '/pair'                => sub {
      prefix '/list'              => sub {
        get ''                    => \&_list;
        get '/details'            => \&_details;
      };
      get '/:primary/:secondary'  => \&_specificPair;
    };
  };

  sub _init {
    $store = Game::EvonyTKR::Web::Store::get_store();
    if(not exists $$store{'pairs'} ) {
      $store->{'pairs'} = {};
      $pairs = $store->{'pairs'};

      if(not exists $store->{'generals'}) {
        Game::EvonyTKR::Web::General::_init();
      }

      if(exists $store->{'generals'}) {
        $generals = $store->{'generals'};
        if( scalar %$generals < 2 ) {
          $logger->debug("cannot create pairs with only one general");
        }
        else {

          my $pairCreator = Game::EvonyTKR::General::Pair::Creator->new();

          #todo: figure out conflicts

          $pairCreator->set_generals(%$generals);
          my %pairs = $pairCreator->getPairs();
          my @buffClasses = $data->BuffClasses();
          my @GeneralKeys = $data->GeneralKeys();
          while (my ($key, $value) = each %pairs) {
            if(any { $_ =~ /$key/i } @GeneralKeys) {
                $pairs->{$key} = $value;
            }
            else {
              $logger->debug("not using key $key when inializing pairs.");
            }
          }
        }
      }
      else {
        $logger->debug("no generals loaded yet")
      }
    }
  }

  sub _details {
    $logger->trace('calling _list from _details');
    _listWorker(1);
  }

  sub _list {
    _listWorker();
  }

  sub _listWorker($verbose = 0) {
    $logger->trace("in _listWorker, verbose is set to $verbose");
    _init();
    my @list;
    my @GeneralKeys = $data->GeneralKeys();
    for my $key (@GeneralKeys) {
      $logger->debug("key of pairs is $key");
      #$logger->debug('which points to ' . np $pairs->{$key});
      if(exists $pairs->{$key}) {
        my @subList = @{$pairs->{$key}};
        for my $entry (@subList) {
          $logger->debug('entry is ' . np $entry);
          push @list, $entry->toHashRef($verbose);
        }
      }
      else {
        $logger->trace("$key does not exist in $pairs");
      }
    }
    return \@list;
  }

  sub __specificPair {
    _init();

  }
}

true;
