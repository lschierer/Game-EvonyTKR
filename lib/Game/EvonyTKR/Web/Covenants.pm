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

package Game::EvonyTKR::Web::Covenants {
# ABSTRACT: Route Handler for the /covenants route.
  use Carp;
  use Util::Any -all;
  use YAML::XS qw{ LoadFile Load };
  use namespace::clean;
  use Dancer2 appname => 'Game::EvonyTKR';

  my %covenants;
  my $logger = Log::Log4perl::get_logger('Web::General');

  prefix '/covenant' => sub {
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
    while (my ($key, $value) = each %covenants) {
      push @data, $value->toHashRef();
    }
    return \@data;
  }

  sub _by_id {
    _init();
  }

  sub _init {
    my $covenantCount = scalar keys %covenants;
    if ($covenantCount == 0) {
      _read_covenants();
      $covenantCount = scalar keys %covenants;
      $logger->debug("After Reading, I have $covenantCount covenants available.");
    } else {
      $logger->debug("I have $covenantCount covenants available.");
    }
  }

  sub _read_covenants() {
    $logger->info('starting read_covenants');
    my $covenantShare = File::Spec->catfile(dist_dir('Game-EvonyTKR'), 'covenants');

    my @found = grep { -T -s -r } glob("$covenantShare/*.yaml");
    $logger->info(sprintf(
      "%s contained %d generals.",
      $covenantShare,
      scalar @found,
      ));

    for my $tc (@found) {
      open(my ($fh), '<', $tc) or $logger->logcroak("$!");
      close $fh;
      my $data = LoadFile($tc);
      
    }
  }

}
1;

__END__
