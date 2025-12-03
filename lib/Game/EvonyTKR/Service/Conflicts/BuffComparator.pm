package Game::EvonyTKR::Service::Conflicts::BuffComparator;
use v5.42.0;
use utf8::all;
use Mojo::Base -base, -signatures;
use List::AllUtils qw( any uniq );
use Scalar::Util   qw( refaddr );

has 'service';    # parent service for constants

# Cache non-conflicting buffs per general pair
has '_non_conflicting_cache' => sub { {} };

sub conflicts ($self, $b1, $b2, $g1, $g2) {
  return 0 if $b1->passive || $b2->passive;
  return 0 unless $b1->attribute eq $b2->attribute;
  return 0 unless $self->_troops_overlap($b1, $b2);

  # Special case: Sun Ce has no conflicts with anyone
  # Special case: Laudon/Roland work together (different conditions but edge case)
  return 0 if $g1->name eq 'Sun Ce' || $g2->name eq 'Sun Ce';
  return 0 if ($g1->name eq 'Laudon' && $g2->name eq 'Roland') || ($g1->name eq 'Roland' && $g2->name eq 'Laudon');

  # Check if either buff is 2nd+ in a group (same logic as BookComparator)
  # Build cache key from both general names
  my $cache_key = join('|', sort ($g1->name, $g2->name));
  unless (exists $self->_non_conflicting_cache->{$cache_key}) {
    $self->_non_conflicting_cache->{$cache_key} =
      $self->_build_non_conflicting_set($g1, $g2);
  }
  my $non_conflicting = $self->_non_conflicting_cache->{$cache_key};

  my $b1_addr = refaddr($b1);
  my $b2_addr = refaddr($b2);

  # If either buff is 2nd+ in a group, it cannot conflict
  return 0 if exists $non_conflicting->{$b1_addr};
  return 0 if exists $non_conflicting->{$b2_addr};

  my $attr = $b1->attribute;

  # Conditionless attributes
  return 1 if $self->service->CONDLESS->{$attr};

  # Triad attributes have special rules
  return $self->_triad_conflicts($b1, $b2, $g1, $g2)
    if $self->service->TRIADS->{$attr};

  # Standard attributes
  return $self->_standard_conflicts($b1, $b2, $g1, $g2);
}

sub _troops_overlap ($self, $b1, $b2) {
  my $t1 = $b1->targetedType // '';
  my $t2 = $b2->targetedType // '';
  return 1 if !$t1 || !$t2;    # global buffs overlap everything
  return $t1 eq $t2;
}

sub _normalize_conditions ($self, $buff) {
  # Strip non-operative and activation scoping conditions
  my @conds = grep {
         $_ ne 'leading the army'
      && $_ ne 'you own the General'
      && !/dragon|spiritual beast/i    # activation scoping
  } @{ $buff->conditions // [] };

  return join('|', sort @conds);
}

sub _conditions_conflict (
  $self, $c1_str, $c2_str,
  $b1_is_multi = 0,
  $b2_is_multi = 0,
  $b1 = undef,
  $b2 = undef
) {
  # Same conditions (including both empty)
  if ($c1_str eq $c2_str) {
    # If BOTH are in multi-attribute groups, they can coexist
    return 0 if $b1_is_multi && $b2_is_multi;

    # If only one is in a multi-attr group, check delta
    if ($b1 && $b2 && ($b1_is_multi || $b2_is_multi)) {
      my $v1 = $b1->value->number // 0;
      my $v2 = $b2->value->number // 0;
      my $delta = abs($v1 - $v2);
      # Need 15+ delta if only one is grouped
      return 0 if $delta >= 15;
    }

    # If both are solo buffs, check delta
    if ($b1 && $b2 && !$b1_is_multi && !$b2_is_multi) {
      my $v1 = $b1->value->number // 0;
      my $v2 = $b2->value->number // 0;
      my $delta = abs($v1 - $v2);
      # Need 25+ delta for solo buffs
      return 0 if $delta >= 25;
    }

    # Otherwise they conflict
    return 1;
  }

  # Different conditions = no conflict
  return 0;
}

sub _triad_conflicts ($self, $b1, $b2, $g1, $g2) {
  my $c1 = $self->_normalize_conditions($b1);
  my $c2 = $self->_normalize_conditions($b2);

  # Check if either is first in a multi-attribute group
  my $b1_multi = $self->_is_first_in_multi_attr_group($b1, $g1);
  my $b2_multi = $self->_is_first_in_multi_attr_group($b2, $g2);

  return $self->_conditions_conflict($c1, $c2, $b1_multi, $b2_multi, $b1, $b2);
}

sub _standard_conflicts ($self, $b1, $b2, $g1, $g2) {
  my $c1 = $self->_normalize_conditions($b1);
  my $c2 = $self->_normalize_conditions($b2);

  # Check if either is first in a multi-attribute group
  my $b1_multi = $self->_is_first_in_multi_attr_group($b1, $g1);
  my $b2_multi = $self->_is_first_in_multi_attr_group($b2, $g2);

  return $self->_conditions_conflict($c1, $c2, $b1_multi, $b2_multi, $b1, $b2);
}

sub _build_non_conflicting_set ($self, $g1, $g2) {
  my %non_conflicting;

  # Find groups in both generals' builtin books
  for my $general ($g1, $g2) {
    my $groups = $self->_find_groups($general->builtInBook);

    # 2nd+ buffs in ANY group cannot conflict
    for my $group (@$groups) {
      my @buffs = @{ $group->{buffs} };

      # Mark all buffs after the first as non-conflicting
      for my $i (1 .. $#buffs) {
        my $addr = refaddr($buffs[$i]);
        $non_conflicting{$addr} = 1;
      }
    }
  }

  return \%non_conflicting;
}

sub _find_groups ($self, $book) {
  my @buffs = grep { !$_->passive } @{ $book->buffs };

# Group by value+unit+conditions (NOT targetedType - grouped buffs like "15% to Mounted and Ranged")
  my %by_key;
  for my $buff (@buffs) {
    my $conds = join('|',
      sort grep { $_ ne 'leading the army' && $_ ne 'you own the General' }
        @{ $buff->conditions // [] });

    my $key =
      join('|', $buff->value->number // 0, $buff->value->unit // '', $conds);

    push @{ $by_key{$key} }, $buff;
  }

  # Find groups (2+ buffs with same key)
  my @groups;
  for my $key (keys %by_key) {
    my $buffs = $by_key{$key};
    next unless @$buffs >= 2;

    push @groups,
      {
      key   => $key,
      buffs => $buffs,
      attrs => [uniq map { $_->attribute } @$buffs],
      };
  }

  return \@groups;
}

sub _is_first_in_multi_attr_group ($self, $buff, $general) {
  my $groups    = $self->_find_groups($general->builtInBook);
  my $buff_addr = refaddr($buff);

  for my $group (@$groups) {
    my @attrs = @{ $group->{attrs} };
    # Only multi-attribute groups count
    next unless @attrs > 1;

    my @buffs = @{ $group->{buffs} };
    # Check if this buff is the first in this group
    if (refaddr($buffs[0]) == $buff_addr) {
      return 1;
    }
  }

  return 0;
}

sub _has_multi_attr_groups ($self, $general) {
  my $groups = $self->_find_groups($general->builtInBook);

  for my $group (@$groups) {
    my @attrs = @{ $group->{attrs} };
    return 1 if @attrs > 1;
  }

  return 0;
}

1;
__END__
