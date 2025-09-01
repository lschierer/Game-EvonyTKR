use v5.42.0;
use experimental qw(class);
use utf8::all;
require Data::Printer;

class Game::EvonyTKR::Model::General::Conflict :
  isa(Game::EvonyTKR::Shared::Constants) {
  use Carp;
  use Readonly;
  use Hash::Util qw(lock_hash lock_hash_recurse lock_value);
  use Data::Compare;
  use Scalar::Util   qw( refaddr );
  use List::AllUtils qw( any none uniq all );

  ### input fields
  field $rootManager : param;
  field $build_index : param //= 0;
  field $assume_g1_is_main : param : reader : writer //= 1;

  # Presence toggles (UI can flip via writers later)
  field $main_has_dragon : param : writer //= 1;
  field $main_has_spirit : param : writer //= 1;
  field $asst_has_dragon : param : writer //= 0;
  field $asst_has_spirit : param : writer //= 0;
  # Add scenario toggles (defaults: marching context)
  field $allow_city_buffs : param : writer //= 0;    # In City / Mayor
  field $allow_wall_buffs : param : writer //= 0;    # Main City Defense
  field $officer_eval     : param : writer //= 0;

  field $generals;
  ADJUST {
    $generals = $rootManager->generalManager->get_all_generals;
  }

  state %PASSIVE = ('you own the General' => 1,);

  field $ProcessedGenerals = {};

  ### output fields

  # { code => [ generals... ] }
  field $groups_by_code : reader = {};

  # { general => { other_general => 1, ... } }
  field $by_general : reader = {};

  field %PBIT = (GR => 1, RA => 2, MT => 4, SG => 8);
  ADJUST {
    lock_hash_recurse(%PBIT);
  }

  field %CONDLESS = map { $_ => 1 }
    ('March Size', 'Marching Speed', 'Stamina cost', 'Double Items Drop Rate');

  ADJUST {
    lock_hash_recurse(%CONDLESS);
  }

  field %STACKABLE_SAME_SIDE = (
    MS                       => 1,    # March Size
    'MARCHING SPEED'         => 1,    # if you store this exact token
    'STAMINA COST'           => 1,
    'DOUBLE ITEMS DROP RATE' => 1,
  );
  field $STACK_MONSTER_SAME_SIDE : reader;

  ADJUST {
    lock_hash_recurse(%STACKABLE_SAME_SIDE);

    # flip to 1 when you feel confident; until then it’s off
    Readonly my $flag => 0;
    $STACK_MONSTER_SAME_SIDE = $flag;
  }

  field %IGNORE_STATE = map { $_ => 1 } (
    'leading the army',
    'brings a dragon',
    'brings a sacred dragon',
    'brings a spiritual beast',
    'you own the General',
  );

  # ---------- Public ----------
  method conflicts_by_general() {$by_general}
  method groups()               {$groups_by_code}

  # ---------- Helpers ----------

  my method _intersect ($a_aref, $b_aref) {
    my %a = map { $_ => 1 } @{ $a_aref // [] };
    for my $x (@{ $b_aref // [] }) { return 1 if $a{$x} }
    0;
  }
  my method _is_blank ($s) { !defined($s) || $s eq '' }

  my %TO_BIT = (
    'Ground Troops'  => 1,
    'Ranged Troops'  => 2,
    'Mounted Troops' => 4,
    'Siege Machines' => 8,
  );

  my method troop_bit ($target) { $TO_BIT{ $target // '' } // 0 }

  # full names now that you’ve moved off 2-letter codes:

  my method is_triad ($attr) {
    $attr eq 'Attack' || $attr eq 'Defense' || $attr eq 'HP';
  }

  # Does this entry's side have ANY grouped entry in the same operative state
  # with overlapping troop scope (besides this exact entry)?
  my method _has_any_group_entry_in_context {
    my ($entry, $side_entries) = @_;
    my $S  = $entry->{state_key} // '';
    my %T  = map { $_ => 1 } @{ $entry->{targetedTypes} // [] };

    for my $x (@{ $side_entries // [] }) {
      next if Scalar::Util::refaddr($x) == Scalar::Util::refaddr($entry);
      next unless ($x->{grouped_attr} || $x->{grouped_troop});

      # same operative state?
      next unless ($x->{state_key} // '') eq $S;

      # troop overlap?
      for my $t (@{ $x->{targetedTypes} // [] }) {
        return 1 if $T{$t};
      }
    }
    return 0;
  }

  my method _cond_active_for_role ($cond, $role) {
    # role: 'main' or 'assistant'
    return 1
      if $cond eq 'leading the army';    # treat as operative/non-passive flag

    if ($cond eq 'brings a dragon') {
      return $role eq 'main' ? $main_has_dragon : $asst_has_dragon;
    }
    if ($cond eq 'brings a spiritual beast') {
      return $role eq 'main' ? $main_has_spirit : $asst_has_spirit;
    }

    # all other conditions are always “environmentally possible”
    return 1;
  }

  # If your enum never produces "leading the army", you can skip this entirely.
  # If it does (as an operative/non-passive flag), keep the drop here.
  my method _strip_nonoperative ($conds_aref) {
    my @keep = grep { !$IGNORE_STATE{$_} } @{ $conds_aref // [] };
    return [] unless @keep;
    @keep = sort @keep;
    return \@keep;
  }

  # Build a stable key from an enum arrayref
  my method _state_key_from_enum_aref ($conds_aref) {
    my $clean = $self->&_strip_nonoperative($conds_aref);
    return '' unless @$clean;     # unconditional
    my @sorted = sort @$clean;    # enum tokens sort stably
    join("\t", @sorted);
  }

  # Role-aware gate (uses your existing toggles), then key
  my method _effective_state_key_from_enum_aref ($conds_aref, $role) {
    my @kept =
      grep { $self->&_cond_active_for_role($_, $role) } @{ $conds_aref // [] };
    $self->&_state_key_from_enum_aref(\@kept);
  }

  my method _state_key_from_conditions ($conds_aref) {
    my @c      = @{ $conds_aref // [] };
    my $joined = join "\t", sort @c;
    return $joined;
  }

  my method build_meta_primative ($r, $buff, $role) {
    return undef if $buff->passive;    # only passives are dropped

    my $attr = $buff->attribute // '';
    my $tgt  = $buff->targetedType;           # may be undef (global)
    my $bit  = $TO_BIT{ $tgt // '' } // 0;    # 0 for global/unscoped

    # state key: force blank for condless attrs, otherwise compute normally
    my $state_key =
      exists $CONDLESS{$attr}
      ? ''
      : $self->&_effective_state_key_from_enum_aref($buff->conditions, $role);

    return {
      attributes    => [$attr],
      targetedTypes => (defined $tgt ? [$tgt] : [])
      ,                                          # keep empty list for global
      valueNumber   => $buff->value->number,
      valueUnit     => $buff->value->unit,
      state_key     => $state_key,
      troop_bit     => $bit,                   # contributes to prim_mask if >0
      passive       => 0,
      grouped_attr  => 0,                      # set later during merge pass
      grouped_troop => 0,                      # set later during merge pass
      is_condless   => exists $CONDLESS{$attr} ? 1 : 0, # helpful for MS/SP/SC/DD logic
    };
  }

  my method _same_set ($aref, $bref) {
    my %a = map { $_ => 1 } @{ $aref // [] };
    my %b = map { $_ => 1 } @{ $bref // [] };
    return 0 unless keys(%a) == keys(%b);
    for my $k (keys %a) { return 0 unless $b{$k} }
    return 1;
  }

  my method compare_meta_primatives ($A, $B) {
    # state/unit/value must match to even consider merging
    return 0 if $A->{valueNumber} != $B->{valueNumber};
    return 0 if ($A->{valueUnit} // '') ne ($B->{valueUnit} // '');
    return 0 if ($A->{state_key} // '') ne ($B->{state_key} // '');

    my $same_troops = $self->&_same_set($A->{targetedTypes}, $B->{targetedTypes});
    my $same_attrs  = $self->&_same_set($A->{attributes},    $B->{attributes});

    # Case 1: same troop set → merge attributes
    return 1 if $same_troops;

    # Case 2: same attribute set → merge troop targets
    return 2 if $same_attrs;

    return 0;
  }

  my method audit_buffs_for_missing_state {
    my ($self) = @_;
    my $gens = $self->{generals};   # or $self->generalManager->get_all_generals

    my @likely_omission;
    my @suspicious_unconditional;

    for my $g (sort { $a->name cmp $b->name } values %$gens) {
      my $m     = $self->_meta_for_general($g);  # uses your propagation already
      my @troop = grep {
        my $A = $_->{A} // '';
        # troop-scoped attributes you care about
        $A eq 'AT' || $A eq 'DF' || $A eq 'HP'
      } @{ $m->{buffs} || [] };

      next unless @troop;                        # nothing troop-scoped => skip

      my %states     = map { (($_->{STATE} // '') => 1) } @troop;
      my $has_blank  = exists $states{''};
      my $num_states = scalar grep { length($_) } keys %states;

      # Mixed: some blank, some non-blank
      # (after propagation this shouldn't happen much)
      if ($has_blank && $num_states >= 1) {
        push @likely_omission, $g->name;
        next;
      }

      # All active troop buffs but no state at all
      my $any_passive = grep { $_->{PASSIVE} } @troop;
      if (!$any_passive && $num_states == 0) {
        push @suspicious_unconditional, $g->name;
      }
    }

    if (@likely_omission) {
      $self->logger->warn(
        "Likely missing STATE in: " . join(', ', @likely_omission));
    }
    if (@suspicious_unconditional) {
      $self->logger->warn(
        "Suspicious unconditional troop buffs (check for 'leading the army'): "
          . join(', ', @suspicious_unconditional));
    }
  }

  # Warm the meta cache (and run audit) for all generals
  my method _warm_cache_and_audit {
  }

  # Optional: build the per-general conflicts hash using are_generals_compatible
  my method build_conflicts_index {
  }

  my method build_meta_for ($general, $role) {
    $self->logger->debug("building meta for " . $general->name);
    my $book = $rootManager->bookManager->getBook($general->builtInBookName)
      or $self->logger->logcroak("Book not found for " . $general->name);

    $general->set_builtInBook($book) unless defined($general->builtInBook);

    my @buffs = $general->builtInBook->buff->@*;    # already-cloned
    foreach my $buff (@buffs){
      $self->logger->debug(sprintf('book %s has buff %s',
      $book->name, Data::Printer::np($buff) ));
    }

    my $r = {
      name     => $general->name,
      types    => $general->type // [],
      is_mayor => List::AllUtils::any { $_ eq 'mayor' }
      @{ $general->type // [] },
      is_wall => List::AllUtils::any { $_ eq 'wall' } @{ $general->type // [] },
      is_officer => List::AllUtils::any { $_ eq 'officer' }
      @{ $general->type // [] },
      meta_buffs => [],    # will push entries
    };

    my @prim;
    for my $b (@buffs) {
      my $mp = $self->&build_meta_primative($r, $b, $role) or next;
      push @prim, $mp;
    }

    # ---- Phase A: merge entries with same TROOP set (union attributes) ----
    my %by_troop; # key: state|val|unit|troops_string -> entry
    for my $e (@prim) {
      # Skip non-troop lines (MS/SP/SC/DD) — keep them as is; no troops_string
      my $troops_s = $e->{targetedTypes_string} // '';
      my $key = join('|',
        ($e->{state_key} // ''),
        ($e->{valueNumber} // 0),
        ($e->{valueUnit} // ''),
        $troops_s,
      );
      if (my $acc = $by_troop{$key}) {
        push @{ $acc->{attributes} }, @{ $e->{attributes} };
        $acc->{grouped_attr} = 1; # now definitely grouped by attr
      } else {
        # shallow clone is fine; we'll normalize below
        $by_troop{$key} = {
          %$e,
          attributes => [ @{ $e->{attributes} } ],
        };
      }
    }

    # normalize attrs and flags
    my @phaseA;
    for my $acc (values %by_troop) {
      my %au; $au{$_}++ for @{ $acc->{attributes} // [] };
      $acc->{attributes} = [ sort keys %au ];
      $acc->{attributes_string} = join '+', @{ $acc->{attributes} };
      $acc->{grouped_attr} = (@{ $acc->{attributes} } >= 2) ? 1 : 0;
      push @phaseA, $acc;
    }

    # ---- Phase B: merge entries with same ATTRIBUTE set (union troops) ----
    my %by_attr; # key: state|val|unit|attributes_string -> entry
    for my $e (@phaseA) {
      # Non-troop lines: no troops_string; merge by attributes as is
      my $attrs_s = $e->{attributes_string} // '';
      my $key = join('|',
        ($e->{state_key} // ''),
        ($e->{valueNumber} // 0),
        ($e->{valueUnit} // ''),
        $attrs_s,
      );
      if (my $acc = $by_attr{$key}) {
        push @{ $acc->{targetedTypes} }, @{ $e->{targetedTypes} // [] };
        $acc->{troop_bit} |= ($e->{troop_bit} // 0);
      } else {
        $by_attr{$key} = {
          %$e,
          targetedTypes => [ @{ $e->{targetedTypes} // [] } ],
          troop_bit     => ($e->{troop_bit} // 0),
        };
      }
    }

    # normalize troops and flags -> final @out
    my @out;
    for my $acc (values %by_attr) {
      my %tu; $tu{$_}++ for @{ $acc->{targetedTypes} // [] };
      $acc->{targetedTypes}        = [ sort keys %tu ];
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
    $ProcessedGenerals->{ $general->name } = $r;
    return $r;
  }

  method are_generals_compatible ($g1, $g2) {
    $self->logger->debug(sprintf(
      'testing %s and %s for compatibility', $g1->name, $g2->name));
    my $role1 = $assume_g1_is_main ? 'main'      : 'assistant';
    my $role2 = $assume_g1_is_main ? 'assistant' : 'main';

    my $m1 = $self->&build_meta_for($g1, $role1);
    my $m2 = $self->&build_meta_for($g2, $role2);
    # no overlapping troop classes

    return 1 unless ($m1->{prim_mask} & $m2->{prim_mask});
    $self->logger->debug(sprintf(
      'm1 for %s is %s; m2 for %s is %s',
      $g1->name, Data::Printer::np($m1, multiline => 1),
      $g2->name, Data::Printer::np($m2, multiline => 1),
    ));

    # attributes that conflict regardless of state
    my %STATE_AGNOSTIC = map { $_ => 1 } qw(MS);

    # attributes whose 'leading the army' state should NOT cause a conflict
    my %LEAD_NONCONFLICT = map { $_ => 1 } qw(AT DF HP);

    for my $e1 (@{ $m1->{meta_buffs} // [] }) {
      for my $e2 (@{ $m2->{meta_buffs} // [] }) {

        # quick reject: troop targets don’t overlap
        next
          unless $self->&_intersect($e1->{targetedTypes}, $e2->{targetedTypes});

        my $S1 = $e1->{state_key} // '';
        my $S2 = $e2->{state_key} // '';

        # Now go attribute-by-attribute to keep logic precise
        for my $a1 (@{ $e1->{attributes} // [] }) {
          for my $a2 (@{ $e2->{attributes} // [] }) {
            $self->logger->debug(sprintf(
              'comparing %s for %s with %s for %s',
              $a1, $g1->name, $a2, $g2->name
            ));
            # Only consider same-attribute collisions
            next unless $a1 eq $a2;

            # 1) condless: conflict on troop overlap regardless of state
            if (exists $CONDLESS{$a1}) {
              $self->logger->debug("$a1 conflict (condless)");
              return 0;
            }

            # 2) triad has special behavior
            if ($self->&is_triad($a1)) {
              # different states -> usually compatible,
              # except when one side is unconditional singleton (Sun Ce gotcha)
              if ($S1 ne $S2) {
                my $x_blank =
                     $self->&_is_blank($S1)
                  && !$e1->{grouped_attr}
                  && !$e1->{grouped_troop};
                my $y_blank =
                     $self->&_is_blank($S2)
                  && !$e2->{grouped_attr}
                  && !$e2->{grouped_troop};

                # optional carve-out:
                # if the blank side truly has no other triad lines
                # for this troop scope, allow it;
                # otherwise, conflict:
                #if ($x_blank || $y_blank) {
                #  $self->logger->debug(
                #    'triad: unconditional vs scoped -> conflict');
                #  return 0;
                #}
                next;    # states differ and not singleton unconditional → fine
              }

              # same operative state
              my $G1 = $e1->{grouped_attr} || $e1->{grouped_troop};
              my $G2 = $e2->{grouped_attr} || $e2->{grouped_troop};

              if ($G1 && $G2) {
                #they both *have* groups. *are they the same??*
                if($e1->{attributes_string} eq $e2->{attributes_string}) {
                  $self->logger->debug(sprintf('%s for %s and %s for %s are the same attribute group',
                  Data::Printer::np($e1->{attributes}), $g1->name,
                  Data::Printer::np($e2->{attributes}), $g2->name, ));
                  if($e1->{targetedTypes_string} eq $e2->{targetedTypes_string}){
                    $self->logger->debug(sprintf('%s for %s and %s for %s are the same targetedTypes group',
                    Data::Printer::np($e1->{targetedTypes}), $g1->name,
                    Data::Printer::np($e2->{targetedTypes}), $g2->name,
                    ));
                    return 0;
                  }
                }
                next;
              }
              elsif ($G1 ^ $G2) {
                # who is grouped vs single?
                my ($grp, $sng, $grp_side, $sng_side) =
                  $G1 ? ($e1, $e2, $m1->{meta_buffs}, $m2->{meta_buffs})
                      : ($e2, $e1, $m2->{meta_buffs}, $m1->{meta_buffs});

                # require attribute AND troop overlap, as you already do
                my %ga = map { $_ => 1 } @{ $grp->{attributes}    // [] };
                my %gt = map { $_ => 1 } @{ $grp->{targetedTypes} // [] };

                my $attr_overlap  = List::AllUtils::any { $ga{$_} } @{ $sng->{attributes}    // [] };
                my $troop_overlap = List::AllUtils::any { $gt{$_} } @{ $sng->{targetedTypes} // [] };
                next unless $attr_overlap && $troop_overlap;

                my $Sg = $grp->{state_key} // '';
                my $Ss = $sng->{state_key} // '';

                # same state → normally conflict, EXCEPT if the singleton side ALSO
                # participates in some grouped clause in this same context.
                if ($Sg eq $Ss) {
                  if ($self->&_has_any_group_entry_in_context($sng, $sng_side)) {
                    $self->logger->debug('single side also has a grouped entry in-context -> allow');
                    next;
                  }
                  $self->logger->debug('grouped vs single, same state -> conflict');
                  return 0;
                }

                # blank vs scoped → your existing “pure single AT” carve-out still applies;
                # otherwise conflict, same as you had.
                if ((length($Sg) == 0) ^ (length($Ss) == 0)) {
                  my $blank_is_grp = (length($Sg) == 0);
                  my $blank        = $blank_is_grp ? $grp : $sng;

                  my $blank_is_pure_single_at =
                       (@{ $blank->{attributes} // [] } == 1
                    &&  $blank->{attributes}[0] eq 'Attack'
                    && !$self->&_has_any_group_entry_in_context($blank, $blank_is_grp ? $grp_side : $sng_side));

                  if ($blank_is_pure_single_at) {
                    $self->logger->debug('pure single AT (Sun Ce style) -> allow');
                    next;
                  }
                  $self->logger->debug('blank vs scoped -> conflict');
                  return 0;
                }
                $self->logger->debug('different non-blank states → compatible');
                # different non-blank states → compatible
                next;
              }
              else {
                # standalone vs standalone in the same state → conflict
                $self->logger->debug(
                  'triad: standalone vs standalone (same state) -> conflict');
                return 0;
              }
            }

            # 3) non-triad normal attributes:
            my $blank1 = $self->&_is_blank($S1);
            my $blank2 = $self->&_is_blank($S2);

            # blank vs non-blank -> compatible
            next if $blank1 ^ $blank2;

            # both non-blank and equal -> conflict
            if (!$blank1 && $S1 eq $S2) {
              $self->logger->debug('non-triad: same state -> conflict');
              return 0;
            }

            # both blank -> conflict (always-on, same troop + attribute)
            if ($blank1 && $blank2) {
              $self->logger->debug('non-triad: both blank -> conflict');
              return 0;
            }

            # else (different non-blank) compatible
          }
        }
      }
    }

    return 1;  # compatible (no conflicting active/active identical-state pairs)
  }

  # Call from ADJUST (or expose explicit methods you call in tests)
  ADJUST {
    #$self->&_warm_cache_and_audit();
    #$self->&build_conflicts_index() if $build_index;
  }

}
1;
__END__
