use v5.40.0;
use experimental qw(class);
use File::FindLib 'lib';
require Data::Printer;
require Path::Tiny;
require YAML::PP;

class Game::EvonyTKR::General::Importer : isa(Game::EvonyTKR::Data) {
  use Carp;
  use Cwd;
  use List::AllUtils qw( any none );
  use Data::Printer;
  use Hash::Util;
  use Storable qw(dclone);
  use namespace::autoclean;
  require Game::EvonyTKR::General;
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

      my $general = Game::EvonyTKR::General->new(
        name            => $go->{name},
        type            => $go->{type},
        ascending       => $go->{ascending},
        builtInBookName => $go->{book},
        specialityNames => $go->{specialities},
      );
      $general->basicAttributes()
        ->attack()
        ->setBase($go->{basic_attributes}->{attack}->{base});
      $generals->{ $go->{name} } = $general;
    }

    return $generals;
  }

};
1;
