use v5.42.0;
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
require Game::EvonyTKR::Model::Specialty;
use namespace::clean;

class Game::EvonyTKR::Model::Specialty::Manager :
  isa(Game::EvonyTKR::Shared::Constants) {
  # PODNAME: Game::EvonyTKR::Model::Specialty::Manager
  use Carp;
  use overload
    'bool'     => sub { $_[0]->_isTrue() },
    'fallback' => 0;

  field $specialties = {};

  method getSpecialty ($name) {
    if (exists $specialties->{$name}) {
      return $specialties->{$name};
    }
    $self->logger->WARN("failed to find specialty $name");
    return 0;
  }

  method get_all_specialties {
    my $values;
    @{$values} = values %{$specialties};
    return $values;
  }

  method importAll ($SourceDir) {
    $SourceDir = Path::Tiny::path($SourceDir);
    if (!$SourceDir->is_dir()) {
      $self->dev_guard(
        "Model::Specialty::Manager requires a directory, not $SourceDir");
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
      $self->logger->DEBUG("Specialty::Manager importing $file");
      my $basename = $file->basename('.yaml');
      my $name     = $basename;

      my $data   = $file->slurp_utf8;
      my $object = YAML::PP->new(
        schema       => [qw/ + Perl /],
        yaml_version => ['1.2', '1.1'],
      )->load_string($data);
      if (exists $object->{name}) {
        # some files are lower case, get the proper case sensitive name
        $name = $object->{name};
        if ($object->{name} !~ /$name/i) {
          $self->logger->WARN("$name does not match " . $object->{name});
        }
      }
      unless (exists($object->{name}) && length($object->{name})) {
        $self->logger->WARN(
          "Object must have a name to create a Specialty. Skipping $file");
        next;
      }
      my $s = Game::EvonyTKR::Model::Specialty->from_hash($object);
      $specialties->{$name} = $s if defined($s);
    }
    my $countImported = scalar keys %$specialties;
    $self->logger->INFO(
      "Model::Specialty::Manager imported $countImported specialties");
    return $countImported;
  }
}
1;
