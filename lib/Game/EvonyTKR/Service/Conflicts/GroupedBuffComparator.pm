package Game::EvonyTKR::Service::Conflicts::GroupedBuffComparator;
use v5.42.0;
use utf8::all;
use Mojo::Base -base, -signatures;
use List::AllUtils qw( any uniq );

has 'service';  # parent service for constants

# Detect conflicts between generals with grouped buffs
sub conflicts ($self, $g1, $g2) {
  # Special case: Louis XIV has stackable buffs
  return undef if $g1->name eq 'Louis XIV' || $g2->name eq 'Louis XIV';
  
  my $groups1 = $self->_find_groups($g1);
  my $groups2 = $self->_find_groups($g2);
  
  # If neither has groups, can't determine via this method
  return undef unless @$groups1 && @$groups2;
  
  # Check if any groups conflict
  for my $g1 (@$groups1) {
    for my $g2 (@$groups2) {
      return 1 if $self->_groups_conflict($g1, $g2);
    }
  }
  
  # Groups exist but don't conflict - let BuffComparator handle individual buffs
  return undef;
}

sub _find_groups ($self, $general) {
  my @buffs = grep { !$_->passive } @{$general->builtInBook->buffs};
  
  # Group by value+unit+conditions
  my %by_key;
  for my $buff (@buffs) {
    my $conds = join('|', sort grep { 
      $_ ne 'leading the army' && $_ ne 'you own the General' 
    } @{$buff->conditions // []});
    
    my $key = join('|', 
      $buff->value->number // 0,
      $buff->value->unit // '',
      $conds
    );
    
    push @{$by_key{$key}}, $buff;
  }
  
  # Find groups (2+ buffs with same key)
  my @groups;
  for my $key (keys %by_key) {
    my $buffs = $by_key{$key};
    next unless @$buffs >= 2;
    
    push @groups, {
      key   => $key,
      buffs => $buffs,
      attrs => [uniq map { $_->attribute } @$buffs],
    };
  }
  
  return \@groups;
}

sub _groups_conflict ($self, $g1, $g2) {
  # Same key = same value/unit/conditions
  return $g1->{key} eq $g2->{key};
}

1;
__END__
