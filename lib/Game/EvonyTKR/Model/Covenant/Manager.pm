use v5.40.0;
use experimental qw(class);
use utf8::all;

use File::FindLib 'lib';
require JSON::PP;
require Encode;
require Data::Printer;
require Path::Tiny;
require Path::Iterator::Rule;
require YAML::PP;
require Game::EvonyTKR::Model::Buff;
require Game::EvonyTKR::Model::Buff::Value;
use namespace::clean;


class Game::EvonyTKR::Model::Covenant : isa(Game::EvonyTKR::Model::Data) {
# PODNAME: Game::EvonyTKR::Model::Covenant
  use builtin         qw(indexed);
  require Data::Printer;
  require Readonly;
  use namespace::autoclean;
  use File::FindLib 'lib';
  use List::AllUtils qw( any none );
  use Carp;
  use overload
    'bool'     => sub { $_[0]->_isTrue() },
    "fallback" => 1;

  my $debug = 1;

  field $rootManager :param;

  field $covenants = {};

  method getCovenant ($name) {
    if (exists $covenants->{$name}) {
      return $covenants->{$name};
    }
    $self->logger->warn("failed to find covenant $name");
    return 0;
  }

  method importAll ($SourceDir) {
    $SourceDir = Path::Tiny::path($SourceDir);
    if (!$SourceDir->is_dir()) {
      $self->logger->logcroak(
        "Model::Covenant::Manager requires a directory, not $SourceDir");
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
      $self->logger->debug("Covenant::Manager importing $file");
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
      my $primary = $rootManager->generalManager->getGeneral($name);
      if($primary) {
        $covenants->{$name} = Game::EvonyTKR::Model::Covenant->new(
          primary => $primary,
        );
        my @secondaryKeys = $covenants->{$name}->secondaryKeys();
        foreach my $ki ( 0..scalar(@secondaryKeys) ) {
          my $key = @secondaryKeys[$ki];
          my $sgn = $object->{generals}->[$ki];
          my $sg = $rootManager->generalManager->getGeneral($sgn);
          if($sg) {
            $covenants->$name->setSecondary($key, $sg);
          }
        }
        foreach my $oc ( @{ $object->category }) {
          foreach my $ob ( @{ })
          my $v = Game::EvonyTKR::Model::Buff::Value->new(
            number => $ob->{value}->{number},
            unit   => $ob->{value}->{unit},
          );
          my $buff = Game::EvonyTKR::Model::Buff->new(
            number => $object->{buff}
          );
        }
      }

    }
  }
};
1;
