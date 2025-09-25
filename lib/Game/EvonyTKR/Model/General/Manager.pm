use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Encode;
require Data::Printer;
require Path::Tiny;
require Path::Iterator::Rule;
require YAML::PP;
require Game::EvonyTKR::Model::General;
require Game::EvonyTKR::Model::Buff;
require Game::EvonyTKR::Model::Buff::Value;
require Game::EvonyTKR::Model::BasicAttribute;
require Game::EvonyTKR::Model::BasicAttributes;
use namespace::clean;

class Game::EvonyTKR::Model::General::Manager :
  isa(Game::EvonyTKR::Shared::Constants) {
  # PODNAME: Game::EvonyTKR::Model::General::Manager
  use Carp;
  use overload
    'bool'     => sub { $_[0]->_isTrue() },
    'fallback' => 0;

  field $generals = {};

  method add_general($ng) {
    if (Scalar::Util::blessed($ng) eq 'Game::EvonyTKR::Model::General') {
      if (!exists $generals->{ $ng->name }) {
        $generals->{ $ng->name } = $ng;
      }
    }
  }

  method get_all_generals () {
    return \%{$generals};
  }

  method getGeneral ($name) {
    if (exists $generals->{$name}) {
      return $generals->{$name};
    }
    return 0;
  }

  method importAll ($SourceDir) {
    $SourceDir = Path::Tiny::path($SourceDir);
    if (!$SourceDir->is_dir()) {
      $self->logger->logcroak(
"Game::EvonyTKR::Model::General::Manager requires a directory, not $SourceDir"
      );
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
      $self->logger->debug("General::Manager importing $file");
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

      unless (exists $object->{type} and length($object->{type}) > 0) {
        $self->logger->error("$name is missing type!!");
        next;
      }

      $generals->{$name} = Game::EvonyTKR::Model::General->new(
        name            => $name,
        type            => $object->{type},
        ascending       => $object->{ascending},
        stars           => $object->{stars},
        builtInBookName => $object->{book},
        specialtyNames  => $object->{specialties},
      );

      foreach my $baKey (keys %{ $object->{basic_attributes} }) {
        my $ba = Game::EvonyTKR::Model::BasicAttribute->new(
          attribute_name => $baKey,
          base           => $object->{basic_attributes}->{$baKey}->{base},
          increment      => $object->{basic_attributes}->{$baKey}->{increment},
        );
        $generals->{$name}->basicAttributes->setAttribute($baKey, $ba);
      }
      $self->logger->debug(
        sprintf('%s successfully imported', $generals->{$name}->name));
    }
    my $countImported = scalar keys %$generals;
    $self->logger->info(
      "Game::EvonyTKR::Model::General::Manager imported $countImported Generals"
    );
    return $countImported;
  }
}
1;
