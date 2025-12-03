package Game::EvonyTKR::Service::Conflicts::GroupedBuffComparator;
use v5.42.0;
use utf8::all;
use Mojo::Base -base, -signatures;
use List::AllUtils qw( any uniq );

has 'service';    # parent service for constants

# Detect conflicts between generals with grouped buffs
sub conflicts ($self, $g1, $g2) {
  # Special case: Sun Ce has no conflicts with anyone
  return undef if $g1->name eq 'Sun Ce' || $g2->name eq 'Sun Ce';

  my $groups1 = $self->_find_groups($g1);
  my $groups2 = $self->_find_groups($g2);

  # If neither has groups, can't determine via this method
  return undef unless @$groups1 && @$groups2;

  # Check if any groups conflict
  for my $gr1 (@$groups1) {
    for my $gr2 (@$groups2) {
      # Groups conflict if same value, same troop type, AND same attribute set
      next unless $gr1->{value} == $gr2->{value};
      next unless $gr1->{type} eq $gr2->{type};

      # Check if attribute sets are identical
      my @attrs1 = sort @{$gr1->{attrs}};
      my @attrs2 = sort @{$gr2->{attrs}};
      next unless @attrs1 == @attrs2;

      my $attrs_match = 1;
      for my $i (0 .. $#attrs1) {
        if ($attrs1[$i] ne $attrs2[$i]) {
          $attrs_match = 0;
          last;
        }
      }

      return 1 if $attrs_match;
    }
  }

  # Groups exist but don't conflict - let BuffComparator handle individual buffs
  return undef;
}

sub _find_groups ($self, $general) {
  my @buffs = grep { !$_->passive } @{ $general->builtInBook->buffs };

  # Group by value+unit+targetedType+conditions (match BookComparator logic)
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
      value => $buffs->[0]->value->number // 0,
      type  => $buffs->[0]->targetedType // '',
      };
  }

  return \@groups;
}

1;
__END__
