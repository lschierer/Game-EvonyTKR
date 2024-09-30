use v5.40.0;
use utf8::all;
use experimental qw{class defer};
use FindBin;
use lib "$FindBin::Bin/../../../../lib";

package Game::EvonyTKR::Web::General {
# ABSTRACT: Route Handler for the /general and routes.
  use Carp;
  use Data::Printer;
  use Devel::Peek;
  use File::HomeDir;
  use File::Path     qw{ make_path };
  use File::ShareDir qw{ dist_dir dist_file };
  use File::Spec;
  use File::Touch;
  use Game::EvonyTKR::Ascending;
  use Game::EvonyTKR::General::Ground;
  use Game::EvonyTKR::General::Mounted;
  use Game::EvonyTKR::General::Ranged;
  use Game::EvonyTKR::General::Siege;
  use Game::EvonyTKR::SkillBook::Special;
  use Game::EvonyTKR::Speciality;
  use Game::EvonyTKR::Web::Store;
  use Log::Log4perl;
  use Util::Any ':all';
  use namespace::clean;
  use FindBin;
  use lib "$FindBin::Bin/../../../../lib";
  use Game::EvonyTKR::Web::General::Pair;
  use Dancer2 appname => 'Game::EvonyTKR';
  use Dancer2::Plugin::REST;

  my $store;
  my $generals;
  my $logger = Log::Log4perl::get_logger('Web::General');

  prefix '/general' => sub {
    prefix '/list' => sub {
      get ''  => \&_list;
      get '/' => \&_list;
      prefix '/details' => sub {
        get '' => \&_details;
      };
    };
    get '/:id' => \&_by_id;
  };

  sub _init {
    $store = Game::EvonyTKR::Web::Store::get_store();
    if (not exists $$store{'generals'}) {
      $store->{'generals'} = {};
      $logger->trace('store now holds' . Data::Printer::np $store);
    }
    $generals = $store->{'generals'};
    $logger->trace('generals now holds ' . Data::Printer::np $generals);
    $logger->trace('generals now holds ' . Data::Printer::np %$generals);
    my $gencount = scalar keys %$generals;
    if ($gencount == 0) {
      _read_generals();
      $gencount = scalar keys %$generals;
      $logger->debug("After Reading, I have $gencount generals available.");
    }
    else {
      $logger->debug(
        "I have $gencount generals already available; reading unnecessary");
    }
  }

  sub _list {
    _init();
    $logger->debug('_list function called');

    my $withTypes = query_parameters->get('types');
    if (defined $withTypes) {
      if ($withTypes ne 'false') {
        $withTypes = 1;
      }
    }
    my @list;
    while (my ($key, $value) = each %$generals) {
      if ($withTypes) {
        push @list,
          {
          name => $value->name(),
          type => $value->generalType(),
          };
      }
      else {
        push @list, $value->name();
      }
    }
    return \@list;
  }

  sub _details {
    _init();
    my @data = ();
    $logger->debug("sub _all has " . scalar %$generals . " generals");

    my $verbose = query_parameters->get('verbose');
    if (defined $verbose) {
      if ($verbose ne 'false') {
        $verbose = 1;
      }
      else {
        $verbose = 0;
      }
    }
    else {
      $verbose = 0;
    }
    my $level = query_parameters->get('level');
    my $ascendingLevel;
    if (query_parameters->get('ascendingLevel') =~ /([1-5](Red|Purple))/) {
      $ascendingLevel = $1;
    }
    else {
      $ascendingLevel = 'None';
    }

    while (my ($key, $value) = each %$generals) {
      # allowable specialityLevels may vary per general
      my @specialityLevels = qw( None None None None None );

      $logger->trace("getting data for " . $value->name());
      my $name = $value->name();

      $value->setLevel($level);
      $value->ascendingAttributes()->setActiveLevel($ascendingLevel);

      for my $sl (1 .. 4) {
        my $sp = query_parameters->get("specialityLevel$sl");
        if (defined $sp) {
          my @lv = $value->specialityLevels();
          if (any { $_ =~ /$sp/i } @lv) {
            $logger->debug(
              "setting $sp at specialityLevel$sl for " . $value->name());
            $value->changeActiveSpecialityLevel($sl, $sp);
          }
          else {
            $logger->warn(
              "invalid specialityLevel $sp at $sl for " . $value->name());
            $logger->warn("valid values are " . Data::Printer::np @lv);
          }
        }
      }

      my $hashedGeneral = $value->toHashRef($verbose);
      $logger->trace("$name has a hashed size of " . scalar %$hashedGeneral);
      $hashedGeneral->{'id'} = $hashedGeneral->{'name'};
      push @data, $hashedGeneral;
    }
    $logger->debug('returning data array with size ' . scalar @data);
    #$logger->debug(Data::Printer::np @data);
    return \@data;
  }

}
1;

__END__
