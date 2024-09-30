use v5.40.0;
use utf8::all;
use experimental qw{class defer};
use File::FindLib 'lib';

package Game::EvonyTKR::Web::General::Pair {
# VERSION
  use Array::Diff;
  use Carp;
  use Data::Printer;
  use Devel::Peek;
  use File::HomeDir;
  use File::Path     qw{ make_path };
  use File::ShareDir qw{ dist_dir dist_file };
  use File::Spec;
  use File::Touch;
  use Game::EvonyTKR::Ascending;
  use Game::EvonyTKR::Data;
  use Game::EvonyTKR::General::Pair;
  use Game::EvonyTKR::General::Pair::Creator;
  use Game::EvonyTKR::General::Ground;
  use Game::EvonyTKR::General::Mounted;
  use Game::EvonyTKR::General::Ranged;
  use Game::EvonyTKR::General::Siege;
  use Game::EvonyTKR::SkillBook::Special;
  use Game::EvonyTKR::Speciality;
  require Game::EvonyTKR::Web::General;
  use Game::EvonyTKR::Web::Store;
  use Log::Log4perl;
  use Util::Any ':all';
  use YAML::XS qw{ LoadFile Load };
  use namespace::clean;
  use Dancer2 appname => 'Game::EvonyTKR';
  use Dancer2::Plugin::REST;

  my $store;
  my $pairs;
  my $generals;
  my $data   = Game::EvonyTKR::Data->new();
  my $logger = Log::Log4perl::get_logger('Web::General');

  prefix '/generals' => sub {
    prefix '/pair' => sub {
      prefix '/list' => sub {
        get ''         => \&_list;
        get '/details' => \&_details;
      };
      get '/:primary/:secondary' => \&_specificPair;
    };
  };

  sub _initPairs {
    $store = Game::EvonyTKR::Web::Store::get_store();
    if (not exists $$store{'pairs'}) {
      $store->{'pairs'} = {};
      $pairs = $store->{'pairs'};

      if (not exists $store->{'generals'}) {
        Game::EvonyTKR::Web::General::_init();
      }

      if (exists $store->{'generals'}) {
        $generals = $store->{'generals'};
        if (scalar %$generals < 2) {
          $logger->debug("cannot create pairs with only one general");
        }
        else {

          my $pairCreator = Game::EvonyTKR::General::Pair::Creator->new();

          #todo: figure out conflicts

          $pairCreator->set_generals(%$generals);
          my %result = $pairCreator->getPairs();

          while (my ($key, $value) = each %result) {
            $pairs->{$key} = $value;
          }
        }
      }
      else {
        $logger->debug("no generals loaded yet");
      }
    }
  }

  sub _specificPair {
    _initPairs();
    my $primaryName   = route_parameters->get('primary');
    my $secondaryName = route_parameters->get('secondary');

    if (not exists $generals->{$primaryName}) {
      status_400({
        error => "$primaryName is not a valid General Id"
      });
    }
    elsif (not exists $generals->{$secondaryName}) {
      status_400({
        error => "$secondaryName is not a valid General Id"
      });
    }
    elsif (not exists $pairs->{$primaryName}) {
      my @GeneralKeys = $data->GeneralKeys();
      my @pk          = keys %$pairs;
      my $diff        = Array::Diff->diff(\@GeneralKeys, \@pk);
      my @available   = $diff->added();
      $logger->error(sprintf(
        '%s has no available pairs. valid options are %s',
        $primaryName, Data::Printer::np @pk
      ));
      status_400({
        error => "$primaryName has no available pairs."
      });
    }
    else {
      my @gp = @{ $pairs->{$primaryName} };
      for my $thisPair (@gp) {
        if ($thisPair->secondary->name() eq $secondaryName) {
          return $thisPair->toHashRef(1);
          last();
        }
      }
      status_400({
        "No pair for $primaryName with secondary general $secondaryName found.",
      });
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
    _initPairs();
    my @list;
    my @GeneralKeys = $data->GeneralKeys();
    for my $key (@GeneralKeys) {
      $logger->debug("key of pairs is $key");
      #$logger->debug('which points to ' . np $pairs->{$key});
      if (exists $pairs->{$key}) {
        my @subList = @{ $pairs->{$key} };
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

}

true;

__END__
# ABSTRACT: Route Handler for the /general/pair route.