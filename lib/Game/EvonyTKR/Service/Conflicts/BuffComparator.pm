package Game::EvonyTKR::Service::Conflicts::BuffComparator;
use v5.42.0;
use utf8::all;
use Mojo::Base -base, -signatures;
use List::AllUtils qw( any );

has 'service';    # parent service for constants

sub conflicts ($self, $b1, $b2, $g1, $g2) {
  return 0 if $b1->passive || $b2->passive;
  return 0 unless $b1->attribute eq $b2->attribute;
  return 0 unless $self->_troops_overlap($b1, $b2);

  # Special case: Louis XIV has stackable buffs
  # (TODO: detect via book text when available)
  # Book test is available in about 5% of generals, including Louis XIV
  # but using it not implemented here yet
  return 0 if $g1->name eq 'Louis XIV' || $g2->name eq 'Louis XIV';

  my $attr = $b1->attribute;

  # Check if buffs have activation scoping (dragon/spirit conditions)
  my $c1      = $b1->conditions // [];
  my $c2      = $b2->conditions // [];
  my $scoped1 = any {/dragon|spiritual beast/i} @$c1;
  my $scoped2 = any {/dragon|spiritual beast/i} @$c2;

# If either has activation scoping, they don't conflict
# (scoped buff only exists if condition met, so can't conflict with always-on buff)
  return 0 if $scoped1 || $scoped2;

  # Conditionless attributes
  return 1 if $self->service->CONDLESS->{$attr};

  # Triad attributes have special rules
  return $self->_triad_conflicts($b1, $b2) if $self->service->TRIADS->{$attr};

  # Standard attributes
  return $self->_standard_conflicts($b1, $b2);
}

sub _troops_overlap ($self, $b1, $b2) {
  my $t1 = $b1->targetedType // '';
  my $t2 = $b2->targetedType // '';
  return 1 if !$t1 || !$t2;    # global buffs overlap everything
  return $t1 eq $t2;
}

sub _normalize_conditions ($self, $buff) {
  # Strip non-operative conditions
  my @conds = grep { $_ ne 'leading the army' && $_ ne 'you own the General' }
    @{ $buff->conditions // [] };

  return join('|', sort @conds);
}

sub _conditions_conflict ($self, $c1_str, $c2_str) {
  # Both empty = compatible (both are "leading the army" only, different roles)
  return 0 if !$c1_str && !$c2_str;

  # Same conditions = conflict
  return 1 if $c1_str eq $c2_str;

  return 0;
}

sub _triad_conflicts ($self, $b1, $b2) {
  my $c1 = $self->_normalize_conditions($b1);
  my $c2 = $self->_normalize_conditions($b2);

  return $self->_conditions_conflict($c1, $c2);
}

sub _standard_conflicts ($self, $b1, $b2) {
  my $c1 = $self->_normalize_conditions($b1);
  my $c2 = $self->_normalize_conditions($b2);

  return $self->_conditions_conflict($c1, $c2);
}

1;
__END__
