use v5.42.0;
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
  isa(Game::EvonyTKR::Shared::Constants) {
  use Carp;
  use List::AllUtils qw( any none );
  use overload
    'bool'     => sub { my ($self) = @_; $self->_isTrue() },
    'fallback' => 0;

  field $conflict_groups         = {};
  field $general_to_groups_index = {};

  field $totalConflicts = {
    'March Size Increase'  => ['c0dc4005-17cb-5904-b92c-8a2159ea486e',],
    "Mounted Troop Attack" => ['7a1d0650-ea3c-5ef7-9a56-5475979c3b09',],
    'Monster Mounted Attack Increase' =>
      ['7a1d0650-ea3c-5ef7-9a56-5475979c3b09',],
    'March Speed Increase Skill' => [],
  };

  field $partialConflcits = {
    'Increase Double Drop from Monsters' => [],
    'March Size Increase'  => ['b54dd5f0-bd21-5551-9c96-21cf5e8fc345',],
    "Mounted Troop Attack" => ['',],
    'Mounted Troop HP'     => ['e69ef6f5-0a56-5510-b2a7-53fa238e2142',],
  };

  method get_conflict_groups_for_general($general) {
    return $general_to_groups_index->{$general} // [];
  }

  method are_generals_compatible($general1, $general2) {
    # Trivially compatible if the names are identical
    # Return false - A general cannot pair with him/herself.
    return 0 if $general1 eq $general2;

    if (not defined $general1 or length($general1) == 0) {
      $self->error("general1 must be defined!!!");
      return 0;
    }
    if (not defined $general2 or length($general2) == 0) {
      $self->error("general2 must be defined!!!");
      return 0;
    }
    # Try with normalized names
    my $norm1 = $conflict_groups->{ (keys %$conflict_groups)[0] }
      ->normalize_name($general1);
    my $norm2 = $conflict_groups->{ (keys %$conflict_groups)[0] }
      ->normalize_name($general2);

    # Check both original and normalized names
    my @groups1 = @{ $self->get_conflict_groups_for_general($general1) // [] };
    push @groups1, @{ $self->get_conflict_groups_for_general($norm1) // [] }
      if $norm1 ne $general1;

    foreach my $group_id (@groups1) {
      my $group = $self->get_conflict_group($group_id) // next;
      return 0 unless $group->is_compatible($general1, $general2);
    }

    return 1;
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

  method is_book_compatible ($bookName, $generalName, $primary = 1) {

    if (exists $totalConflicts->{$bookName}) {
      $self->logger->debug("bookname $bookName has total conflicts.");
      foreach my $conflictId (@{ $totalConflicts->{$bookName} }) {
        $self->logger->debug(
"inspecting for total confict in group $conflictId for conflicts with $bookName and $generalName"
        );
        my $cg = $self->get_conflict_group($conflictId);
        if ($cg) {
          $self->logger->debug(
"inspecting for total conflict in group $conflictId to see if it contains $generalName in "
              . Data::Printer::np($cg->primary_generals));
          if (any { $cg->normalize_name($generalName) =~ /$_/ }
            @{ $cg->primary_generals }) {
            $self->logger->debug(
              "found conflict in group $conflictId between $bookName and "
                . Data::Printer::np($cg->primary_generals));
            return 0;
          }
        }
        else {
          $self->error("cannot retrieve conflict group $conflictId.");
        }
      }
    }

# if primary is set to true, I am only checking for total conflicts, assuming that
# this general contains the book in question and thus a partial match works.
    if (not $primary) {
      if (exists $partialConflcits->{$bookName}) {
        foreach my $conflictId (@{ $partialConflcits->{$bookName} }) {
          my $cg = $self->get_conflict_group($conflictId);
          if ($cg) {
            $self->logger->debug(
"inspecting for partial conflict conflict in group $conflictId to see if it contains $generalName in "
                . Data::Printer::np($cg->primary_generals));
            if (any { $cg->normalize_name($generalName) =~ /$_/ }
              @{ $cg->primary_generals }) {
              $self->logger->debug(sprintf(
                'found match for book %s between %s and one of %s.',
                $bookName, $generalName,
                Data::Printer::np($cg->primary_generals)
              ));
              return 0;
            }
          }
        }
      }
    }

    return 1;
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
      $SourceDir->parent->parent->child('share/EvansFullGeneralInfo.csv');
    my $groups_file =
      $SourceDir->parent->parent->child(
      'share/EvAnsConflictGroupDefinitions.csv');
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
        my $norm_general = $conflict_groups->{$id}->normalize_name($general);
        push @{ $general_to_groups_index->{$norm_general} }, $id;
        # Also keep the original mapping for backward compatibility
        push @{ $general_to_groups_index->{$general} }, $id
          if $norm_general ne $general;
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
