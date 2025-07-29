use v5.42.0;
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
require Game::EvonyTKR::Model::Covenant;
use namespace::clean;

class Game::EvonyTKR::Model::Covenant::Manager :
  isa(Game::EvonyTKR::Shared::Constants) {
# PODNAME: Game::EvonyTKR::Model::Covenant::Manager
  use builtin qw(indexed);
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

  field $rootManager : param;

  field $covenants = {};

  method getCovenant ($name) {
    if (exists $covenants->{$name}) {
      return $covenants->{$name};
    }
    $self->logger->info("failed to find covenant $name");
    return 0;
  }

  method get_all_covenants {
    return values %{$covenants};
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
      $self->logger->trace(
        "$object imported, looks like " . Data::Printer::np($object));
      if (exists $object->{name}) {
        if ($object->{name} !~ /$name/i) {
          $self->logger->error(
"filename and internal name do not match for file '$file' with name '$object->{name}'"
          );
        }
        $name = $object->{name};
      }

      my $primary = $rootManager->generalManager->getGeneral($name);
      unless ($primary) {
        $self->logger->error("Cannot find primary general for covenant $name");
        next;
      }
      $self->logger->debug("found primary general for $name, starting import.");
      $self->logger->debug(
        "generals for $name are " . join(", ", @{ $object->{generals} }));

      $covenants->{$name} = Game::EvonyTKR::Model::Covenant->new(
        primary => $primary,
        one     => $object->{generals}->[0],
        two     => $object->{generals}->[1],
        three   => $object->{generals}->[2],
      );

      foreach my $oc (@{ $object->{levels} }) {
        my $category = $oc->{category};

        # Find buffs for this category
        my @buffs;
        if(exists $oc->{buff}){
          @buffs = @{$oc->{buff}};
        }elsif(exists $oc->{buffs}){
          @buffs = @{$oc->{buffs}};
        }
        foreach my $ob (@buffs) {
          my $b = Game::EvonyTKR::Model::Buff->from_hash($ob);
          $covenants->{$name}->addBuff($category, $b);
        }
      }
      $self->logger->debug(
        "import of $file for $name complete.  covenant created: "
          . Data::Printer::np($covenants->{$name}));

    }
    my $countImported = scalar keys %$covenants;
    $self->logger->info(
      "Model::Covenant::Manager imported $countImported covenants");
    return $countImported;
  }
  };
1;
