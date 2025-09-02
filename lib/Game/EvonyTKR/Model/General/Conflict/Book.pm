use v5.42.0;
use experimental qw(class);
use utf8::all;
require Data::Printer;
require Scalar::Util;
require Game::EvonyTKR::Model::General::Conflict;

class Game::EvonyTKR::Model::General::Conflict::Book :
  isa(Game::EvonyTKR::Model::General::Conflict) {
  use Hash::Util     qw(lock_hash lock_hash_recurse lock_value);
  use List::AllUtils qw( any none uniq all );
  use Carp;

  #root manager instance
  field $rm : param;

  field $booknames = [
    "Level 1 Ground Troop Attack Against Monster",
    "Level 1 Ground Troop Attack",
    "Level 1 Ground Troop Defense Against Monster",
    "Level 1 Ground Troop Defense",
    "Level 1 Ground Troop HP Against Monster",
    "Level 1 Ground Troop HP",
    "Level 1 Luck",
    "Level 1 March Size",
    "Level 1 Mounted Troop Attack Against Monster",
    "Level 1 Mounted Troop Attack",
    "Level 1 Mounted Troop Defense Against Monster",
    "Level 1 Mounted Troop Defense",
    "Level 1 Mounted Troop HP Against Monster",
    "Level 1 Mounted Troop HP",
    "Level 1 Ranged Troop Attack Against Monster",
    "Level 1 Ranged Troop Attack",
    "Level 1 Ranged Troop Defense Against Monster",
    "Level 1 Ranged Troop Defense",
    "Level 1 Ranged Troop HP Against Monster",
    "Level 1 Ranged Troop HP",
    "Level 1 Siege Machine Attack",
    "Level 1 Siege Machine Defense",
    "Level 1 Siege Machine HP",
    "Level 2 Ground Troop Attack Against Monster",
    "Level 2 Ground Troop Attack",
    "Level 2 Ground Troop Defense Against Monster",
    "Level 2 Ground Troop Defense",
    "Level 2 Ground Troop HP Against Monster",
    "Level 2 Ground Troop HP",
    "Level 2 Luck",
    "Level 2 March Size",
    "Level 2 Mounted Troop Attack Against Monster",
    "Level 2 Mounted Troop Attack",
    "Level 2 Mounted Troop Defense Against Monster",
    "Level 2 Mounted Troop Defense",
    "Level 2 Mounted Troop HP Against Monster",
    "Level 2 Mounted Troop HP",
    "Level 2 Ranged Troop Attack Against Monster",
    "Level 2 Ranged Troop Attack",
    "Level 2 Ranged Troop Defense Against Monster",
    "Level 2 Ranged Troop Defense",
    "Level 2 Ranged Troop HP Against Monster",
    "Level 2 Ranged Troop HP",
    "Level 2 Siege Machine Attack",
    "Level 2 Siege Machine Defense",
    "Level 2 Siege Machine HP",
    "Level 3 Ground Troop Attack Against Monster",
    "Level 3 Ground Troop Attack",
    "Level 3 Ground Troop Defense Against Monster",
    "Level 3 Ground Troop Defense",
    "Level 3 Ground Troop HP Against Monster",
    "Level 3 Ground Troop HP",
    "Level 3 Luck",
    "Level 3 March Size",
    "Level 3 Mounted Troop Attack Against Monster",
    "Level 3 Mounted Troop Attack",
    "Level 3 Mounted Troop Defense Against Monster",
    "Level 3 Mounted Troop Defense",
    "Level 3 Mounted Troop HP Against Monster",
    "Level 3 Mounted Troop HP",
    "Level 3 Ranged Troop Attack Against Monster",
    "Level 3 Ranged Troop Attack",
    "Level 3 Ranged Troop Defense Against Monster",
    "Level 3 Ranged Troop Defense",
    "Level 3 Ranged Troop HP Against Monster",
    "Level 3 Ranged Troop HP",
    "Level 3 Siege Machine Attack",
    "Level 3 Siege Machine Defense",
    "Level 3 Siege Machine HP",
    "Level 4 Ground Troop Attack Against Monster",
    "Level 4 Ground Troop Attack",
    "Level 4 Ground Troop Defense Against Monster",
    "Level 4 Ground Troop Defense",
    "Level 4 Ground Troop HP Against Monster",
    "Level 4 Ground Troop HP",
    "Level 4 Luck",
    "Level 4 March Size",
    "Level 4 Mounted Troop Attack Against Monster",
    "Level 4 Mounted Troop Attack",
    "Level 4 Mounted Troop Defense Against Monster",
    "Level 4 Mounted Troop Defense",
    "Level 4 Mounted Troop HP Against Monster",
    "Level 4 Mounted Troop HP",
    "Level 4 Ranged Troop Attack Against Monster",
    "Level 4 Ranged Troop Attack",
    "Level 4 Ranged Troop Defense Against Monster",
    "Level 4 Ranged Troop Defense",
    "Level 4 Ranged Troop HP Against Monster",
    "Level 4 Ranged Troop HP",
    "Level 4 Siege Machine Attack",
    "Level 4 Siege Machine Defense",
    "Level 4 Siege Machine HP",
  ];

  field $books = {};

  field $ProcessedBooks = {};

  field $conflicts_by_book_name = {};

  ADJUST {
    foreach my $name (@$booknames) {
      $books->{$name} = $rm->bookManager->getBook($name);
    }
    lock_hash(%$books);
  }

  my method build_meta_for ($book) {
    $self->logger->debug("building meta for " . $book->name);

    my @buffs = $book->buff->@*;    # already-cloned
    foreach my $buff (@buffs) {
      $self->logger->debug(sprintf(
        'book %s has buff %s', $book->name, Data::Printer::np($buff)));
    }

    my $r = {
      name       => $book->name,
      meta_buffs => [],            # will push entries
    };

    my @prim;
    for my $b (@buffs) {
      my $mp = $self->build_meta_primative($r, $b, 'main') or next;
      push @prim, $mp;
    }

    # ---- Phase A: merge entries with same TROOP set (union attributes) ----
    my %by_troop;    # key: state|val|unit|troops_string -> entry
    for my $e (@prim) {
      # Skip non-troop lines (MS/SP/SC/DD) — keep them as is; no troops_string
      my $troops_s = $e->{targetedTypes_string} // '';
      my $key      = join('|',
        ($e->{state_key}   // ''),
        ($e->{valueNumber} // 0),
        ($e->{valueUnit}   // ''), $troops_s,);
      if (my $acc = $by_troop{$key}) {
        push @{ $acc->{attributes} }, @{ $e->{attributes} };
        $acc->{grouped_attr} = 1;    # now definitely grouped by attr
      }
      else {
        # shallow clone is fine; we'll normalize below
        $by_troop{$key} = { %$e, attributes => [@{ $e->{attributes} }], };
      }
    }

    # normalize attrs and flags
    my @phaseA;
    for my $acc (values %by_troop) {
      my %au;
      $au{$_}++ for @{ $acc->{attributes} // [] };
      $acc->{attributes}        = [sort keys %au];
      $acc->{attributes_string} = join '+', @{ $acc->{attributes} };
      $acc->{grouped_attr}      = (@{ $acc->{attributes} } >= 2) ? 1 : 0;
      push @phaseA, $acc;
    }

    # ---- Phase B: merge entries with same ATTRIBUTE set (union troops) ----
    my %by_attr;    # key: state|val|unit|attributes_string -> entry
    for my $e (@phaseA) {
      # Non-troop lines: no troops_string; merge by attributes as is
      my $attrs_s = $e->{attributes_string} // '';
      my $key     = join('|',
        ($e->{state_key}   // ''),
        ($e->{valueNumber} // 0),
        ($e->{valueUnit}   // ''), $attrs_s,);
      if (my $acc = $by_attr{$key}) {
        push @{ $acc->{targetedTypes} }, @{ $e->{targetedTypes} // [] };
        $acc->{troop_bit} |= ($e->{troop_bit} // 0);
      }
      else {
        $by_attr{$key} = {
          %$e,
          targetedTypes => [@{ $e->{targetedTypes} // [] }],
          troop_bit     => ($e->{troop_bit} // 0),
        };
      }
    }

    # normalize troops and flags -> final @out
    my @out;
    for my $acc (values %by_attr) {
      my %tu;
      $tu{$_}++ for @{ $acc->{targetedTypes} // [] };
      $acc->{targetedTypes}        = [sort keys %tu];
      $acc->{targetedTypes_string} = join '+', @{ $acc->{targetedTypes} };
      $acc->{grouped_troop}        = (@{ $acc->{targetedTypes} } >= 2) ? 1 : 0;

      push @out, $acc;
    }

    # compute prim_mask (OR of non-passive troop bits)
    my $mask = 0;
    $mask |= ($_->{troop_bit} // 0) for @out;

    $r->{meta_buffs} = \@out;
    $r->{prim_mask}  = $mask;
    lock_hash_recurse(%$r);
    $ProcessedBooks->{ $book->name } = $r;
    return $r;
  }

  method is_general_and_book_compatible($general, $book) {
    unless (exists $self->ProcessedGenerals->{ $general->name }) {
      $self->logger->error("General has not been processed for conflicts yet.");
      return 0;
    }
    my $m1 = $self->ProcessedGenerals->{ $general->name };
    my $m2 = $ProcessedBooks->{ $book->name }
      // ($ProcessedBooks->{ $book->name } = $self->build_meta_for($book));

    # Quick reject: if book is troop-scoped and there’s no overlap at all
    if ($m2->{prim_mask} && !($m1->{prim_mask} & $m2->{prim_mask})) {
      return 1;    # compatible
    }

    # Compare each entry; book entries are singletons (no “packages”)
    for my $e1 (@{ $m1->{meta_buffs} // [] }) {
      for my $eb (@{ $m2->{meta_buffs} // [] }) {
        # 1) attribute must match
        my $book_attr = $eb->{attributes}[0] // '';
        next
          unless List::AllUtils::any { $_ eq $book_attr }
        @{ $e1->{attributes} // [] };

        # 2) condless (MS/SP/SC/DD) -> conflict on attribute match alone
        if ($eb->{is_condless}) {
          $self->logger->debug(
            "Book <$book_attr> (condless) conflicts with $general->{name}");
          return 0;
        }

        # 3) troop overlap (for troop-scoped books)
        next
          unless $self->_intersect($e1->{targetedTypes}, $eb->{targetedTypes});

      # 4) state interplay
      # Your meta already strips 'leading the army', so books are usually blank.
        my $S1 = $e1->{state_key} // '';
        my $S2 = $eb->{state_key} // '';

        my $blank1 = (length($S1) == 0);
        my $blank2 = (length($S2) == 0);

        # Same rules for triad and non-triad here (books are singletons):
        # - both blank -> conflict
        # - same non-blank -> conflict
        # - blank vs non-blank -> conflict
        # - different non-blank -> allow
        return 0 if ($blank1  && $blank2);
        return 0 if (!$blank1 && !$blank2 && $S1 eq $S2);
        return 0 if ($blank1 ^ $blank2);

        # else: different non-blank states → compatible for this pair
      }
    }

    return 1;    # no collisions found

  }

}
1;
__END__
