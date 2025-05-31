use v5.40.0;
use experimental qw(class);
use utf8::all;

require Path::Tiny;
require Path::Iterator::Rule;
require YAML::PP;
require Encode;
require Hash::Util;
require Game::EvonyTKR::Model::General::ConflictGroup;
require Game::EvonyTKR::Model::General::ConflictGroup::Importer;
use namespace::clean;

class Game::EvonyTKR::Model::General::ConflictGroup::Manager :
  isa(Game::EvonyTKR::Model::Data) {
  use Carp;

  field $conflict_groups         = {};
  field $general_to_groups_index = {};

  method get_conflict_groups_for_general($general) {
    return $general_to_groups_index->{$general} // [];
  }

  method are_generals_compatible($general1, $general2) {
    # Efficient compatibility check using indexes
  }

  method get_conflict_group($id) {
    if (exists $conflict_groups->{$id}) {
      return $conflict_groups->{$id};
    }
    return 0;
  }

  method get_conflict_groups () {
    my %copy = %{$conflict_groups};
    Hash::Util::lock_hash(%copy);    # prevents adding/deleting top-level keys
    return \%copy;
  }

  method get_conflict_groups_index () {
    my $list;
    @{$list} = keys %{$conflict_groups};
    $self->logger->debug(
      "conflict_groups_index is " . Data::Printer::np($list));
    return sort $list;
  }

  method importAll($SourceDir) {
    my $importData;
    # Convert string parameter to Path::Tiny for ease of Filesystem operations
    $SourceDir = Path::Tiny::path($SourceDir);
    if (!$SourceDir->is_dir()) {
      $self->logger->logcroak(
"Model::General::ConflictGroup::Manager requires a directory, not $SourceDir"
      );
    }

    my $generals_file =
      $SourceDir->parent->parent->child('EvansFullGeneralInfo.csv');
    my $groups_file =
      $SourceDir->parent->parent->child('EvAnsConflictGroupDefinitions.csv');
    if ($generals_file->is_file() && $groups_file->is_file()) {
      my $importer =
        Game::EvonyTKR::Model::General::ConflictGroup::Importer->new(
        outputDir     => $SourceDir,
        generals_file => $generals_file,
        groups_file   => $groups_file,
        );
      $importData = $importer->importAndMap();
    }
    else {
      $self->logger->error(
        "either '$generals_file' or '$groups_file' does not exist");
    }
    my $allgroups = [];
    foreach my $key (keys %{$importData}) {
      # work around for UTF8 filenames not importing correctly by default.
      $self->logger->debug("General::ConflictGroup::Manager importing $key");

      my $name = $key;

      my $object = $importData->{$key};

# 'name' is a reasonable unique identifier for everything *except* conflict conflict groups
# where the upstream source for the information *regularly* changes the names, and the
# process that takes the upstream source and generates files from it cannot figure out
# which existing file should be mapped to which name.  As a result, I treat names as
# disposable values.   In every other collection type, the filename is based on the unique
# identifier. for this collection type, the name is marginally more human readable than a uuid
# but only marginally.

      #The source data files store the uuid id in a 'id' attribute.
      if (not exists $object->{id} or not defined $object->{id}) {
        $self->logger->error(
          "General::ConflictGroup::Manager cannot import $name without an id");
        next;
      }
      my $id = $object->{id};

      if (not exists $object->{members} or ref $object->{members} ne 'ARRAY') {
        $self->logger->error(
"General::ConflictGroup::Manager cannot import $name if its members attribute is "
            . ref $object->{members}
            . "instead of 'ARRAY'");
        next;
      }

      $conflict_groups->{$id} =
        Game::EvonyTKR::Model::General::ConflictGroup->new(
        id               => $id,
        name             => $name,
        primary_generals => $object->{members},
        linked_groups    => $object->{others} // [],
        );
      push @{$allgroups}, $conflict_groups->{$id};
      foreach my $general (@{ $object->{members} }) {
        push @{ $general_to_groups_index->{$general} }, $id;
      }
    }

    foreach my $key (keys %{$conflict_groups}) {
      $conflict_groups->{$key}->build_expanded_conflicts($conflict_groups);
    }
    my $conflictGroupCount = scalar keys %{$conflict_groups};
    $self->logger->info(
      "General::ConflictGroup::Manager imported $conflictGroupCount.");
    return $conflictGroupCount;
  }

}
