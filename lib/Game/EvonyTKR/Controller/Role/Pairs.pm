use v5.42.0;
use utf8::all;
use File::FindLib 'lib';
require Game::EvonyTKR::Service::Cache;
require Game::EvonyTKR::Model::Factory;

package Game::EvonyTKR::Controller::Role::Pairs {
  use Mojo::Base -role, -signatures;
  use List::AllUtils qw(uniq);
  use List::UtilsBy;
  use Carp;

  has 'pair_cache' => sub ($job) {
    return Game::EvonyTKR::Service::Cache->new(namespace => 'pairs__');
  };

  has 'conflict_cache' => sub ($job) {
    return Game::EvonyTKR::Service::Cache->new(namespace => 'conflicts__');
  };

  sub setup_pairs_by_type ($self) {
    my $pairs = {};
    foreach my $key ($self->GeneralKeys->@*) {
      $pairs->{$key} = [];
    }
    my $success = $self->pair_cache->add('pairs_by_type', $pairs);
    my $verify  = $self->pair_cache->get('pairs_by_type');
    $self->logger->debug(sprintf(
      'After add with return value "%s", stored value: %s',
      defined($success) ? $success : 'undef return',
      Data::Printer::np($verify)
    ));
    return (defined($success) && length($success) && $success ne '0');
  }

  sub add_wire_pair ($self, $wire_pair) {
    my $key = $self->wire_pair_to_key($wire_pair);
    my $cas_val;

    $cas_val = $self->pair_cache->gets('pair_list');
    if (defined($cas_val) && length($$cas_val[1]) > 0) {
      my $longstring = $$cas_val[1];
      my $pair_list;
      @$pair_list  = split ';', $longstring;
      $longstring  = join ';', List::AllUtils::uniq($key, $pair_list->@*);
      $$cas_val[1] = $longstring;
    }
    elsif (defined($cas_val) && length($$cas_val[1]) == 0) {
      $$cas_val[1] = $key;
    }
    elsif (defined($cas_val) && $$cas_val[1] == 0) {
      $$cas_val[1] = $key;
    }
    else {
      my $result = $self->pair_cache->add('pair_list', $key);
      if (not defined($result) || $result == 0) {
        $self->logger->error('could neither retrieve nor add pair_list');
      }
      return $result;
    }
    $self->pair_cache->cas('pair_list', @$cas_val);
    $cas_val = undef;

    $cas_val = $self->pair_cache->gets('pairs_by_type');

    if (defined($cas_val) && ref($$cas_val[1]) eq 'HASH') {
      $$cas_val[1] = $self->merge_into_pairs_by_type($$cas_val[1], $wire_pair);
      $self->pair_cache->cas('pairs_by_type', @$cas_val);
    }
    elsif (defined($cas_val) && not defined($cas_val->[1])) {
      $cas_val->[1] = $self->merge_into_pairs_by_type({}, $wire_pair);
    }
    elsif (defined($cas_val) && $cas_val->[1] eq "0") {
      my $result = $self->setup_pairs_by_type();
      if ($result) {
        return $self->add_wire_pair($wire_pair);
      }
      else {
        $self->logger->error(sprintf('error setting up pairs_by_type: %s.',
          defined($result) ? $result : 'undef result'));
        return $result;
      }
    }
    else {
      $self->logger->error(
        sprintf('cannot get pairs_by_type %s.', Data::Printer::np($cas_val)));
      return undef;
    }

    return $self->pair_cache->set($key, $wire_pair);
  }

  sub merge_into_pairs_by_type($self, $pairs_by_type, @new_pairs) {
    foreach my $np (@new_pairs) {
      unless (List::AllUtils::any { $_ eq $np->{type} } $self->GeneralKeys->@*)
      {
        $self->logger->error(sprintf(
          'pair type must be one of %s, not %s',
          join(', ', $self->GeneralKeys->@*),
          $np->{type}
        ));
        next;
      }

      $pairs_by_type->{ $np->{type} } //= [];
      $pairs_by_type->{ $np->{type} } =
        [$np, $pairs_by_type->{ $np->{type} }->@*];
    }

    foreach my $type (keys $pairs_by_type->%*){
      my %hash = map { $self->wire_pair_to_key($_) => $_ } $pairs_by_type->{$type}->@*;

      $pairs_by_type->{$type} =  [
        sort { $self->wire_pair_to_key($a) cmp $self->wire_pair_to_key($b) } values %hash
      ];

      $self->logger->debug(sprintf('after merge, there are %s %s type pairs',
      scalar($pairs_by_type->{$type}->@*), $type));
    }
    return $pairs_by_type;
  }

  sub get_pairs_by_type ($self) {
    return $self->pair_cache->get('pairs_by_type');
  }

  sub get_all_pairs ($self) {
    my $key_list = $self->pair_cache->get('pair_list') // '';
    my $keys     = [split ';', $key_list];
    my $pairs    = [];
    foreach my $pair_key ($keys->@*) {
      my $pair = $self->pair_cache->get($pair_key);
      unless ($pair) {
        $self->logger->error("failed to get $pair_key");
        next;
      }
      push @{$pairs}, $pair;
    }
    return $pairs;
  }

  sub wire_pair_to_key($self, $wire_pair) {
    my $key = sprintf('%s/%s/%s',
      $wire_pair->{type},
      $self->normalize($wire_pair->{primary}),
      $self->normalize($wire_pair->{secondary}),
    );
    $key = lc($key);
    $key =~ s/ /_/g;
    return $key;
  }

}
1;
__END__
