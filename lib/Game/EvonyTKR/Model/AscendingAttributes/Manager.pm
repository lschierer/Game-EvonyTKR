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
require Game::EvonyTKR::Model::AscendingAttributes;
use namespace::clean;

class Game::EvonyTKR::Model::AscendingAttributes::Manager :
  isa(Game::EvonyTKR::Shared::Constants) {
  # PODNAME: Game::EvonyTKR::Model::AscendingAttributes::Manager
  use Carp;
  use overload
    'bool'     => sub { $_[0]->_isTrue() },
    'fallback' => 0;

  field $ascendingAttributes = {};

  method getAscendingAttributes ($name) {
    if (exists $ascendingAttributes->{$name}) {
      return $ascendingAttributes->{$name};
    }
    $self->logger->warn("failed to find Ascending Attribute $name");
    return 0;
  }

  method importAll ($SourceDir) {
    $SourceDir = Path::Tiny::path($SourceDir);
    if (!$SourceDir->is_dir()) {
      $self->logger->logcroak(
"Game::EvonyTKR::Model::AscendingAttributes::Manager requires a directory, not $SourceDir"
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
      $self->logger->debug("AscendingAttributes::Manager importing $file");
      my $basename = $file->basename('.yaml');
      my $name     = $basename;

      my $data   = $file->slurp_utf8;
      my $object = YAML::PP->new(
        schema       => [qw/ + Perl /],
        yaml_version => ['1.2', '1.1'],
      )->load_string($data);
      if (exists $object->{general}) {
        if ($object->{general} !~ /$name/i) {
          $self->logger->error(
"filename and internal name do not match for file '$file' with name '$object->{general}'"
          );
        } else {
          $self->logger->info("using object name " . $object->{general})
        }
        $name = $object->{general};
      }

      $ascendingAttributes->{$name} =
        Game::EvonyTKR::Model::AscendingAttributes->new(general => $name,);
      unless (exists $object->{ascending}
        && ref($object->{ascending}) eq 'ARRAY') {
        $self->logger->error("object has unexpected format for '$name': "
            . Data::Printer::np($object));
        next;
      }
      $self->logger->debug("starting import for $name");
      foreach my $oa (@{ $object->{ascending} }) {
        my $level = $oa->{level};
        foreach my $ob (@{ $oa->{buffs} }) {
          my $v = Game::EvonyTKR::Model::Buff::Value->new(
            number => abs($ob->{value}->{number} // 0),
            unit   => $ob->{value}->{unit},
          );
          my $b;
          $b = Game::EvonyTKR::Model::Buff->new(
            value     => $v,
            attribute => $ob->{attribute},
          );

          if (exists $ob->{targetedType}) {
            $b->set_target($ob->{targetedType});
          }

          if (exists $ob->{conditions}) {
            foreach my $c (@{ $ob->{conditions} }) {
              $b->set_condition($c);
            }
          }
          $ascendingAttributes->{$name}->addBuff($level, $b);
          $self->logger->debug(sprintf(
            '%s now has %s buffs at level %s',
            $name,
            scalar(
              $ascendingAttributes->{$name}->ascending->{$level}->{buffs}->@*
            ),
            $level,
          ));
        }
      }
      $self->logger->debug(sprintf('imported %s %s',
        $name,
        exists $ascendingAttributes->{$name}
        ? 'successfully'
        : 'unsuccessfully'));
    }
    my $countImported = scalar keys %$ascendingAttributes;
    $self->logger->info(
"Game::EvonyTKR::Model::AscendingAttributes::Manager imported $countImported ascendingAttributes"
    );
    return $countImported;
  }
}
1;
