use v5.40.0;
use utf8::all;
use experimental qw{class defer};
use Data::Printer;
use Devel::Peek;
use File::HomeDir;
use File::Path     qw{ make_path };
use File::ShareDir qw{ dist_dir dist_file };
use File::Spec;
use File::Touch;
use Log::Log4perl;

use FindBin;
use lib "$FindBin::Bin/../../../../lib";
use Game::EvonyTKR::General::Ground;
use Game::EvonyTKR::General::Mounted;
use Game::EvonyTKR::General::Ranged;
use Game::EvonyTKR::General::Siege;
use Game::EvonyTKR::SkillBook::Special;
use Game::EvonyTKR::Speciality;
use Game::EvonyTKR::Ascending;

package Game::EvonyTKR::Web::General {
# ABSTRACT: Route Handler for the /generals route.
  use Carp;
  use Util::Any -all;
  use YAML::XS qw{ LoadFile Load };
  use Game::EvonyTKR::Web::Store;
  use namespace::clean;
  use Dancer2 appname => 'Game::EvonyTKR';
  use Dancer2::Plugin::REST;

  my $store;
  my $logger = Log::Log4perl::get_logger('Web::General');

  sub _init {
    $store = Game::EvonyTKR::Web::Store::get_store();
    if(not exists $$store{'generals'} ) {
      $$store{'generals'} = {};
    }
    my $gencount = scalar keys %{$store->{'generals'}};
    if ($gencount == 0) {
      _read_generals();
      $gencount = scalar keys %{$store->{'generals'}};
      $logger->debug("After Reading, I have $gencount generals available.");
    } else {
      $logger->debug("I have $gencount generals already available; reading unnecessary");
    }
  }

  prefix '/generals' => sub {
    get '/details' => \&_all;
    get '/list'    => \&_list;
  };

  prefix '/general' => sub {
    get '/:id'  => \&_by_id;
  };

  sub _list {
    _init();
    my @list;
    while (my ($key, $value) = each %{$store->{'generals'}}) {
      push @list, $value->name();
    }
    return \@list;
  }

  sub _all {
    _init();
    my @data = ();
    $logger->debug("sub _all has " . scalar %{$store->{'generals'}} . " generals");

    while (my ($key, $value) = each %{$store->{'generals'}}) {
      $logger->trace("getting data for ". $value->name());
      my $name = $value->name();
      my $hashedGeneral = $value->toHashRef();
      $logger->trace("$name has a hashed size of " . scalar %$hashedGeneral);
      push @data, $hashedGeneral;
    }
    $logger->debug('returning data array with size ' . scalar @data);
    #$logger->debug(Data::Printer::np @data);
    return \@data ;
  }

  sub _by_id {
    _init();
    my $id = route_parameters->get('id');
    if (exists $store->{'generals'}->{$id} ) {
      my $general = $store->{'generals'}->{$id};

      my $level = query_parameters->get('level');
      if (defined $level) {
        $logger->debug("Query level is '$level'");
        $general->setLevel(0+ $level);
      }
      else {
        $logger->debug("Query level is not defined.");
      }

      my @specialityLevels = qw( None None None None None );
      for my $sl (1..4) {
        my $sp = query_parameters->get("specialityLevel$sl");
        if (defined $sp) {
          my @lv = $general->specialityLevels();
          if(any {$_ =~ /$sp/i} @lv) {
            $logger->debug("setting $sp at specialityLevel$sl for $id");
            $general->changeActiveSpecialityLevel($sl, $sp);
          } else {
            $logger->warn("invalid specialityLevel $sp at $sl for $id");
            $logger->warn("valid values are " . Data::Printer::np @lv);
          }
        }
      }
      my $ascendingLevel = query_parameters->get('ascendingLevel');

      if (defined $ascendingLevel) {
        $logger->debug("Query ascendingLevel is '$ascendingLevel'");
        if ($ascendingLevel =~ /[1-5](Red|Purple)/) {
          $logger->trace("setting $ascendingLevel");
          $general->ascendingAttributes()->setActiveLevel($ascendingLevel);
        } else {
          $logger->warn(
            "Detected bad input '$ascendingLevel' reading ascendingLevel");
        }
      }
      else {
        $logger->debug("Query ascendingLevel is not defined.");
      }

      my $verbose = query_parameters->get('verbose');
      if(defined $verbose and $verbose ){
        return $general->toHashRef( 1);
      } else {
        return $general->toHashRef();
      }
    } else {
      return { general => "Not Found" };
    }
  }

  sub _read_generals() {
    $logger->info('starting read_generals');
    my $general_share =
      File::Spec->catfile(File::ShareDir::dist_dir('Game-EvonyTKR'),
      'generals');

    my @found   = grep { -T -s -r } glob("$general_share/*.yaml");
    my $message = "general_share: " . scalar @found;
    $logger->info("general_share '$general_share' contained "
        . scalar @found
        . " generals");

    foreach my $tg (@found) {
      open(my ($fh), '<', $tg) or $logger->logcroak("$!");
      close $fh;
      my $data = LoadFile($tg);
      my $name = $data->{'general'}->{'name'};
      $logger->info($name);

      my %generalClass = (
        'Ground'  => 'Game::EvonyTKR::General::Ground',
        'Mounted' => 'Game::EvonyTKR::General::Mounted',
        'Ranged'  => 'Game::EvonyTKR::General::Ranged',
        'Siege'   => 'Game::EvonyTKR::General::Siege',
      );

      my @generalClassKey;
      my @scoreType = @{ $data->{'general'}->{'score_as'} };
      if (any { $_ =~ /Ground/ } @scoreType) {
        $logger->trace('this can be a Ground General');
        push @generalClassKey => 'Ground';
      }
      if (any { $_ =~ /Mounted/ } @scoreType) {
        $logger->trace('this can be a Mounted General');
        push @generalClassKey => 'Mounted';
      }
      if (any { $_ =~ /Ranged/ or $_ =~ /Archers/ } @scoreType) {
        $logger->trace('this can be a Ranged General');
        push @generalClassKey => 'Ranged';
      }
      if (any { $_ =~ /Siege/ } @scoreType) {
        $logger->trace('this can be a Siege General');
        push @generalClassKey => 'Siege';
      }
      if (any { $_ =~ /Mayor/ } @scoreType) {
        $logger->trace('this can be a Mayor');
        next;
      }
      if (scalar @generalClassKey != scalar @scoreType) {
        $logger->logcroak($data->{'general'}->{'name'}
            . " is of unknown general type "
            . Data::Printer::np @scoreType);
      }

      if(scalar @generalClassKey == 0 ) {
        $logger->logcroak($data->{'general'}->{'name'} .
          ' has no general classes at all');
      }
      else {
        $logger->trace( $data->{'general'}->{'name'} .
          ' has ' . scalar @generalClassKey .
          'classes.'
        );
      }

      for (@generalClassKey) {
        my $thisClass = $_;
        $store->{'generals'}->{$name} = $generalClass{$thisClass}->new(
          name => $data->{'general'}->{'name'},
        );

        $logger->debug("created " . $name);

        $store->{'generals'}->{$name}->readFromFile();

        $logger->debug("added " . Data::Printer::np $store->{'generals'}->{$name});
      }

    }
  }
}
1;

__END__
