use v5.40.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Encode;
require Data::Printer;
require Path::Tiny;
require Path::Iterator::Rule;
require YAML::PP;
require Game::EvonyTKR::Model::Buff;
require Game::EvonyTKR::Model::Buff::Value;
require Game::EvonyTKR::Model::Speciality;
use namespace::clean;

class Game::EvonyTKR::Model::Speciality::Manager :
  isa(Game::EvonyTKR::Model::Data) {
  # PODNAME: Game::EvonyTKR::Model::Speciality::Manager
  use Carp;
  use overload
    'fallback' => 0;

  my $specialities = {};

  method getSpeciality ($name) {
    if (exists $specialities->{$name}) {
      return $specialities->{$name};
    }
    return 0;
  }

  method importAll ($SourceDir) {
    $SourceDir = Path::Tiny::path($SourceDir);
    if (!$SourceDir->is_dir()) {
      $self->logger->logcroak(
        "Model::Speciality::Manager requires a directory, not $SourceDir");
    }
    my $rule = Path::Iterator::Rule->new();
    $rule->name(qr/\.ya?ml$/);
    $rule->file->nonempty;
    my $iter = $rule->iter(
      $SourceDir,
      {
        follow_symlinks => 0,
        sorted          => 1,
      }
    );
    while (defined(my $file = $iter->())) {
      # work around for UTF8 filenames not importing correctly by default.
      $file = Path::Tiny::path(Encode::decode('utf8', $file));
      $self->logger->debug("Speciality::Manager importing $file");
      my $basename = $file->basename('.yaml');
      my $name     = $basename;

      my $data   = $file->slurp_utf8;
      my $object = YAML::PP->new(
        schema       => [qw/ + Perl /],
        yaml_version => ['1.2', '1.1'],
      )->load_string($data);
      if (exists $object->{name}) {
        if ($name ne $object->{name}) {
          $self->logger->error(
"filename and internal name do not match for file '$file' with name '$object->{name}'"
          );
        }
        $name = $object->{name};
      }

      $specialities->{$name} =
        Game::EvonyTKR::Model::Speciality->new(name => $name,);
      foreach my $ol (@{ $object->{levels} }) {
        my $level = $ol->{level};
        foreach my $ob (@{ $ol->{buff} }) {
          my $v = Game::EvonyTKR::Model::Buff::Value->new(
            number => $ob->{number},
            unit   => $ob->{unit},
          );
          my $b;
          if (exists $ob->{class}) {
            $b = Game::EvonyTKR::Model::Buff->new(
              value         => $v,
              attribute     => $ob->{attribute},
              targetedTypes => [$ob->{class}],
            );
          }
          else {
            $b = Game::EvonyTKR::Model::Buff->new(
              value     => $v,
              attribute => $ob->{attribute},
            );
          }
          if (exists $ob->{condition}) {
            foreach my $c (@{ $ob->{condition} }) {
              $b->set_condition($c);
            }
          }
          $specialities->{$name}->addBuff($level, $b);
        }
      }
    }
    my $countImported = scalar keys %$specialities;
    $self->logger->info(
      "Model::Speciality::Manager imported $countImported specialities");
    return $countImported;
  }
}
1;
