package Game::EvonyTKR::Service::Conflicts::BookComparator;
use v5.42.0;
use utf8::all;
use Mojo::Base -base, -signatures;
use List::AllUtils qw( any uniq );
use Scalar::Util   qw( refaddr );

has 'service';    # parent service for constants

# Compare generic book against general's builtin book
# Returns: 0 = compatible, 1 = partial conflict, 2 = full conflict
sub conflicts ($self, $general, $generic_book, $opts = {}) {
  my $same_side = $opts->{same_side} // 0;

  my $builtin = $general->builtInBook;

  # Find grouped buffs in builtin book
  my $groups = $self->_find_groups($builtin);

  # Debug logging
  if ($self->service->can('logger')) {
    $self->service->logger->debug(sprintf(
      'Found %d groups in builtin book for %s',
      scalar @$groups,
      $general->name
    ));
  }

  # Only the FIRST buff in each group can conflict
  # Subsequent buffs in the same sentence cannot conflict with books
  my %non_conflicting_buffs;
  for my $group (@$groups) {
    my @buffs = @{ $group->{buffs} };
    # Debug logging
    if ($self->service->can('logger')) {
      $self->service->logger->debug(sprintf(
        'Group with %d buffs: %s',
        scalar @buffs,
        join(', ', map { $_->attribute } @buffs)
      ));
    }
    # Skip first buff, mark rest as non-conflicting
    for my $i (1 .. $#buffs) {
      my $addr = refaddr($buffs[$i]);
      $non_conflicting_buffs{$addr} = 1;
      # Debug logging
      if ($self->service->can('logger')) {
        $self->service->logger->debug(sprintf(
          'Marking as non-conflicting: %s (index %d, addr=%s)',
          $buffs[$i]->attribute,
          $i, $addr
        ));
      }
    }
  }

  # Compare each buff in generic book against builtin buffs
  my $worst = 0;    # track worst conflict level

  for my $gen_buff (@{ $generic_book->buffs }) {
    next if $gen_buff->passive;

    if ($self->service->can('logger')) {
      $self->service->logger->debug(sprintf(
        'Generic buff: %s %s',
        $gen_buff->attribute, $gen_buff->targetedType // 'global'
      ));
    }

    for my $bi_buff (@{ $builtin->buffs }) {
      next if $bi_buff->passive;

      my $bi_addr = refaddr($bi_buff);

      # Skip buffs that are 2nd+ in a group (only first can conflict)
      if (exists $non_conflicting_buffs{$bi_addr}) {
        if ($self->service->can('logger')) {
          $self->service->logger->debug(sprintf(
            'Skipping non-conflicting buff: %s (addr=%s)',
            $bi_buff->attribute, $bi_addr
          ));
        }
        next;
      }

      # Debug: show which buff we're checking
      if ($self->service->can('logger')) {
        $self->service->logger->debug(sprintf(
          'Checking buff: %s (addr=%s, in hash=%d)',
          $bi_buff->attribute, $bi_addr,
          exists($non_conflicting_buffs{$bi_addr}) ? 1 : 0
        ));
      }

      my $conflict = $self->_buff_conflict($gen_buff, $bi_buff);
      $worst = $conflict if $conflict > $worst;
      return 2 if $worst == 2;    # short-circuit on full conflict
    }
  }

  # Partial conflicts are OK on same side (both buffs work)
  return 0 if $worst == 1 && $same_side;

  return $worst;
}

sub _buff_conflict ($self, $generic_buff, $builtin_buff) {
  # Must match: attribute, troop type, conditions
  return 0 unless $generic_buff->attribute eq $builtin_buff->attribute;

  my $troops_match = $self->_troops_match($generic_buff, $builtin_buff);
  if (!$troops_match && $self->service->can('logger')) {
    $self->service->logger->debug(sprintf(
      'Troops dont match: %s vs %s',
      $generic_buff->targetedType // 'global',
      $builtin_buff->targetedType // 'global'
    ));
  }
  return 0 unless $troops_match;

  my $conds_match = $self->_conditions_match($generic_buff, $builtin_buff);
  if (!$conds_match && $self->service->can('logger')) {
    $self->service->logger->debug(sprintf(
      'Conditions dont match: [%s] vs [%s]',
      join(',', @{ $generic_buff->conditions // [] }),
      join(',', @{ $builtin_buff->conditions // [] })
    ));
  }
  return 0 unless $conds_match;

  # Compare values
  my $gen_val = $generic_buff->value->number // 0;
  my $bi_val  = $builtin_buff->value->number // 0;

  # Debug
  if ($self->service->can('logger')) {
    $self->service->logger->debug(sprintf(
      'Book buff match: %s %s vs %s (gen=%s, bi=%s)',
      $generic_buff->attribute,
      $generic_buff->targetedType // 'global',
      $builtin_buff->targetedType // 'global',
      $gen_val,
      $bi_val
    ));
  }

  # Full conflict: builtin >= generic
  return 2 if $bi_val >= $gen_val;

  # Partial conflict: generic > builtin
  return 1 if $gen_val > $bi_val;

  return 0;
}

sub _troops_match ($self, $b1, $b2) {
  my $t1 = $b1->targetedType // '';
  my $t2 = $b2->targetedType // '';

  # Both empty (March Size) or both same
  return 1 if $t1 eq $t2;

  return 0;
}

sub _conditions_match ($self, $b1, $b2) {
  my $c1 = $self->_normalize_conditions($b1);
  my $c2 = $self->_normalize_conditions($b2);

  return $c1 eq $c2;
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

sub _find_groups ($self, $book) {
  my @buffs = grep { !$_->passive } @{ $book->buffs };

  # Group by value+unit+conditions+targetedType
  my %by_key;
  for my $buff (@buffs) {
    my $conds = join('|',
      sort grep { $_ ne 'leading the army' && $_ ne 'you own the General' }
        @{ $buff->conditions // [] });

    my $key = join('|',
      $buff->value->number // 0,
      $buff->value->unit   // '',
      $buff->targetedType  // '', $conds);

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

1;
__END__
