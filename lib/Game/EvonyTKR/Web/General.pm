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

  sub _by_id {
    _init();
    my $id = route_parameters->get('id');
    if (exists $generals->{$id}) {
      my $general = $generals->{$id};

      my $level = query_parameters->get('level');
      if (defined $level) {
        $logger->debug("Query level is '$level'");
        $general->setLevel(0+ $level);
      }
      else {
        $logger->debug("Query level is not defined.");
      }

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

      my @specialityLevels = qw( None None None None None );
      for my $sl (1 .. 4) {
        my $sp = query_parameters->get("specialityLevel$sl");
        if (defined $sp) {
          my @lv = $general->specialityLevels();
          if (any { $_ =~ /$sp/i } @lv) {
            $logger->debug("setting $sp at specialityLevel$sl for $id");
            $general->changeActiveSpecialityLevel($sl, $sp);
          }
          else {
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
        }
        else {
          $logger->warn(
            "Detected bad input '$ascendingLevel' reading ascendingLevel");
        }
      }
      else {
        $logger->debug("Query ascendingLevel is not defined.");
      }

      return $general->toHashRef($verbose);
    }
    else {
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
      if (any { $_ =~ /Ground/i } @scoreType) {
        $logger->trace('this can be a Ground General');
        push @generalClassKey => 'Ground';
      }
      if (any { $_ =~ /Mounted/i } @scoreType) {
        $logger->trace('this can be a Mounted General');
        push @generalClassKey => 'Mounted';
      }
      if (any { $_ =~ /Ranged/i or $_ =~ /Archers/i } @scoreType) {
        $logger->trace('this can be a Ranged General');
        push @generalClassKey => 'Ranged';
      }
      if (any { $_ =~ /Siege/i } @scoreType) {
        $logger->trace('this can be a Siege General');
        push @generalClassKey => 'Siege';
      }
      if (any { $_ =~ /Mayor/i } @scoreType) {
        $logger->trace('this can be a Mayor');
        next;
      }
      if (scalar @generalClassKey != scalar @scoreType) {
        $logger->logcroak($data->{'general'}->{'name'}
            . " is of unknown general type "
            . Data::Printer::np @scoreType);
      }

      if (scalar @generalClassKey == 0) {
        $logger->logcroak(
          $data->{'general'}->{'name'} . ' has no general classes at all');
      }
      else {
        $logger->trace($data->{'general'}->{'name'} . ' has '
            . scalar @generalClassKey
            . 'classes.');
      }

      for (@generalClassKey) {
        my $thisClass = $_;
        $generals->{$name} =
          $generalClass{$thisClass}->new(name => $data->{'general'}->{'name'},);

        $logger->debug("created " . $name);

        $store->{'generals'}->{$name}->readFromFile();

        $logger->debug(
          "added " . Data::Printer::np $store->{'generals'}->{$name});
      }

    }
  }
}
1;

__END__
