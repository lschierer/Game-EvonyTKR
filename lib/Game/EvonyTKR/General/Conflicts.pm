use v5.40.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';

class Game::EvonyTKR::General::Conflicts : isa(Game::EvonyTKR::Data) {
# PODNAME: Game::EvonyTKR::General::Conflicts
  use Carp;
  use Clone 'clone';
  use Class::ISA;
  use Types::Common qw( t is_Num is_Str is_Int);
  use Type::Utils "is";
  use Util::Any -all;
  use File::ShareDir ':ALL';
  use File::HomeDir;
  use File::Spec;
  use File::Path qw(make_path);
  use Data::Printer;
  use Hash::Map;
  use YAML::PP::LibYAML;
  use List::MoreUtils;
  use Game::EvonyTKR::SkillBook::Special;
  use Game::EvonyTKR::SkillBook::Standard;
  use Game::EvonyTKR::Data;
  use namespace::autoclean;
# VERSION

  field $name : reader : param;

  field $uuid : reader;

  ADJUST {
    $uuid = uuid5(uuid5($self->UUID5_base(), 'Conflicts'), $name);
  }

  field $primaryMembers : reader = ();

  field $relatedConflicts : reader = ();

  field $books : reader = ();

  method add_primary_general($general) {
    my $classList = blessed $general;
    if (index($classList, 'Game::EvonyTKR::General') != -1) {
      if (none { $_->name() eq $general->name() } @$primaryMembers) {
        push @$primaryMembers, $general;
      }
    }
  }

  method add_related_conflict($conflict) {
    my $classList = blessed $conflict;
    if (index($classList, 'Game::EvonyTKR::General::Conflict') != -1) {
      if (none { $_->name() eq $conflict->name() } @$relatedConflicts) {
        push @$relatedConflicts, $conflict;
      }
    }
  }

  method add_conflicting_book($book) {
    my $classList = blessed $book;
    if (index($classList, 'Game::EvonyTKR::SkillBook') != -1) {
      if (none { $_->name() eq $book->name() } @$books) {
        push @$books, $book;
      }
    }
  }

  method check_conflicting_general($general) {
    my $classList = blessed $general;
    if (index($classList, 'Game::EvonyTKR::General') != -1) {
      my $found = first { $_->name() eq $general->name() } @$primaryMembers;
      if (defined $found) {
        $self->logger()->debug(sprintf(
          'determined that %s is in fact part of conflict group %s.  full list is %s.',
          $general->name(), $name, Data::Printer::np $primaryMembers,
        ));
        return $name;
      }
      $self->logger()->debug(sprintf(
        '%s is not a member of %s, list is %s.',
        $general->name(), $name, Data::Printer::np $primaryMembers,
      ));
      return 0;
    }
    $self->logger()
      ->debug(sprintf(
      'could not validate an object of %s as a member of a conflict group',
      $classList));
    return 0;
  }

  method check_conflicting_book($book) {
    my $classList = blessed $book;
    if (index($classList, 'Game::EvonyTKR::SkillBook') != -1) {
      my $found = first { $_->name() eq $book->name() } @$books;
      if (defined $found) {
        $self->logger()->debug(sprintf(
          'determined that %s is in fact part of conflict group %s.  full list is %s.',
          $book->name(), $name, Data::Printer::np $books,
        ));
        return $name;
      }
      $self->logger()->debug(sprintf(
        '%s is not a member of %s, list is %s.',
        $book->name(), $name, Data::Printer::np $books,
      ));
      return 0;
    }
    $self->logger()
      ->debug(sprintf(
      'could not validate an object of %s as a member of a conflict group',
      $classList));
    return 0;
  }

  method check_conflcting($object) {
    my $classList = blessed $object;
    if (index($classList, 'Game::EvonyTKR::General') != -1) {
      return check_conflicting_general($object);
    }
    if (index($classList, 'Game::EvonyTKR::SkillBook') != -1) {
      return check_conflicting_book($object);
    }
    $self->logger()->error(sprintf(
      '%s is of type %s, which is not %s nor %s',
      Data::Printer::np $object, $classList,
      'Game::EvonyTKR::General', 'Game::EvonyTKR::SkillBook',
    ));
    return 0;
  }

};
1;
__END__

# ABSTRACT: Module for processing information about Evony TKR Conflicts between Game::EvonyTKR::General objects and other Game::EvonyTKR objects.

