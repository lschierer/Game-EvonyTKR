package Game::EvonyTKR::Service::Conflicts::BookComparator;
use v5.42.0;
use utf8::all;
use Mojo::Base -base, -signatures;
use List::AllUtils qw( any );

has 'service';  # parent service for constants

# Compare generic book against general's builtin book
# Returns: 0 = compatible, 1 = partial conflict, 2 = full conflict
sub conflicts ($self, $general, $generic_book, $opts = {}) {
  my $same_side = $opts->{same_side} // 0;
  
  my $builtin = $general->builtInBook;
  
  # Compare each buff in generic book against builtin buffs
  my $worst = 0;  # track worst conflict level
  
  for my $gen_buff (@{$generic_book->buffs}) {
    next if $gen_buff->passive;
    
    for my $bi_buff (@{$builtin->buffs}) {
      next if $bi_buff->passive;
      
      my $conflict = $self->_buff_conflict($gen_buff, $bi_buff);
      $worst = $conflict if $conflict > $worst;
      return 2 if $worst == 2;  # short-circuit on full conflict
    }
  }
  
  # Partial conflicts are OK on same side (both buffs work)
  return 0 if $worst == 1 && $same_side;
  
  return $worst;
}

sub _buff_conflict ($self, $generic_buff, $builtin_buff) {
  # Must match: attribute, troop type, conditions
  return 0 unless $generic_buff->attribute eq $builtin_buff->attribute;
  return 0 unless $self->_troops_match($generic_buff, $builtin_buff);
  return 0 unless $self->_conditions_match($generic_buff, $builtin_buff);
  
  # Compare values
  my $gen_val = $generic_buff->value->number // 0;
  my $bi_val = $builtin_buff->value->number // 0;
  
  # Debug
  if ($self->service->can('logger')) {
    $self->service->logger->debug(sprintf(
      'Book buff match: %s %s vs %s (gen=%s, bi=%s)',
      $generic_buff->attribute,
      $generic_buff->targetedType // 'global',
      $builtin_buff->targetedType // 'global',
      $gen_val, $bi_val
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
    && !/dragon|spiritual beast/i  # activation scoping
  } @{$buff->conditions // []};
  
  return join('|', sort @conds);
}

1;
__END__
