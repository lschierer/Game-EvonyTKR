use v5.40.0;
use experimental qw(class);
use utf8::all;

use File::FindLib 'lib';
require Data::Printer;
require Path::Tiny;
require YAML::PP;

class Game::EvonyTKR::Model::General::Importer :
  isa(Game::EvonyTKR::Model::Data) {
  use Carp;
  use Cwd;
  use List::AllUtils qw( any none );
  use Data::Printer;
  use Hash::Util;
  use Storable qw(dclone);
  use namespace::autoclean;
  require Game::EvonyTKR::Model::General;
# VERSION
  my $debug = 1;

  field $inputDir : reader : param;

  field $id;

  ADJUST {
    my $testPath;

    $testPath = Path::Tiny::path($inputDir);
    if ($testPath->exists()) {
      if ($testPath->is_dir()) {
        $id = $testPath;
      }
      else {
        croak "$inputDir is not a directory!!";
      }
    }
    else {
      croak "$inputDir does not exist!";
    }
  }

  method importAll () {
    my @generalFiles = $id->children(qr/\.yaml\z/);

    my $ypp = YAML::PP->new(
      schema       => [qw/ + Perl /],
      yaml_version => ['1.2', '1.1'],
    );

    my $generals = {};

    foreach my $generalFile (@generalFiles) {
      $generalFile =
        Path::Tiny::path(Encode::decode('utf8', $generalFile->stringify()));
      my $generalData = $generalFile->slurp_utf8;
      my $go          = $ypp->load_string($generalData);

      next unless ref $go eq 'HASH' && $go->{name};

      my $general = Game::EvonyTKR::Model::General->new(
        name            => $go->{name},
        type            => $go->{type},
        ascending       => $go->{ascending},
        builtInBookName => $go->{book},
        specialtyNames  => $go->{specialties},
      );
      $general->basicAttributes()
        ->attack()
        ->setBase($go->{basic_attributes}->{attack}->{base});
      $general->basicAttributes()
        ->attack()
        ->setIncrement($go->{basic_attributes}->{attack}->{increment});

      $general->basicAttributes()
        ->defense()
        ->setBase($go->{basic_attributes}->{defense}->{base});
      $general->basicAttributes()
        ->defense()
        ->setIncrement($go->{basic_attributes}->{defense}->{increment});

      $general->basicAttributes()
        ->leadership()
        ->setBase($go->{basic_attributes}->{leadership}->{base});
      $general->basicAttributes()
        ->leadership()
        ->setIncrement($go->{basic_attributes}->{leadership}->{increment});

      $general->basicAttributes()
        ->politics()
        ->setBase($go->{basic_attributes}->{politics}->{base});
      $general->basicAttributes()
        ->politics()
        ->setIncrement($go->{basic_attributes}->{politics}->{increment});

      $generals->{ $go->{name} } = $general;
    }
    my $generalCount = scalar keys %{$generals};
    $self->logger->debug("returning $generalCount generals");
    return $generals;
  }

  };
1;
