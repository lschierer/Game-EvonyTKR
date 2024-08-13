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
  use namespace::clean;
  use Dancer2 appname => 'Game::EvonyTKR';

  my %generals;
  my $logger = Log::Log4perl::get_logger('Web::General');

  sub _init {
    my $gencount = scalar keys %generals;
    if ($gencount == 0) {
      _read_generals();
      $gencount = scalar keys %generals;
      $logger->debug("After Reading, I have $gencount generals available.");
    } else {
      $logger->debug("I have $gencount generals available.");
    }
  }

  prefix '/general' => sub {
    prefix '/all' => sub {
      get '' => \&_all;
    };
    prefix '/by_id' => sub {
      get '/:id' => \&_by_id;
    };
  };

  sub _all {
    _init();
    my @data;
    while (my ($key, $value) = each %generals) {
      push @data, $value->toHashRef();
    }
    return \@data;
  }

  sub _by_id {
    _init();
    my $id = route_parameters->get('id');
    if (exists $generals{$id}) {
      my $general = $generals{$id};

      my $level = query_parameters->get('level');
      $logger->debug("Query level is '$level'");
      if (defined $level) {
        $general->setLevel(0+ $level);
      }

      my $ascendingLevel = query_parameters->get('ascendingLevel');
      $logger->debug("Query ascendingLevel is '$ascendingLevel'");
      if (defined $ascendingLevel) {
        if ($ascendingLevel =~ /[1-5](Red|Purple)/) {
          $logger->trace("setting $ascendingLevel");
          $general->ascendingAttributes()->setActiveLevel($ascendingLevel);
        } else {
          $logger->warn(
            "Detected bad input '$ascendingLevel' reading ascendingLevel");
        }
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
        push @generalClassKey => 'Ground';
      }
      if (any { $_ =~ /Mounted/ } @scoreType) {
        push @generalClassKey => 'Mounted';
      }
      if (any { $_ =~ /Ranged/ or $_ =~ /Archers/ } @scoreType) {
        push @generalClassKey => 'Ranged';
      }
      if (any { $_ =~ /Siege/ } @scoreType) {
        push @generalClassKey => 'Siege';
      }
      if (any { $_ =~ /Mayor/ } @scoreType) {
        next;
      }
      if (scalar @generalClassKey != scalar @scoreType) {
        $logger->logcroak($data->{'general'}->{'name'}
            . " is of unknown general type "
            . Data::Printer::np @scoreType);
      }

      for (@generalClassKey) {
        $generals{$name} = $generalClass{$_}->new(
          name                 => $data->{'general'}->{'name'},
        );
        
        $logger->debug("created " . $generals{$name});

        $generals{$name}->readFromFile();

        $logger->debug("added " . Data::Printer::np $generals{$name});
      }

    }
  }
}
1;

__END__
