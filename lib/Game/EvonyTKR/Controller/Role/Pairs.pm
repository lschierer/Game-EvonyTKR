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

  sub setup_pairs_by_type ($self) {
    my $pairs = {};
    foreach my $key ($self->GeneralKeys->@*) {
      $pairs->{$key} = [];
    }
    return $self->pair_cache->add('pairs_by_type', $pairs);
  }

  sub add_wire_pair ($self, $wire_pair) {
    my $key = sprintf('%s/%s/%s',
      lc($self->normalize($wire_pair->{type})),
      lc($self->normalize($wire_pair->{primary})),
      lc($self->normalize($wire_pair->{secondary})),
    );
    $key =~ s/ /_/g;
    my $cas_val = $self->pair_cache->gets('pairs_by_type');
    if (defined($cas_val)) {
      $$cas_val[1] = $self->merge_into_pairs_by_type($$cas_val[1], $wire_pair);
      $self->pair_cache->cas('pairs_by_type', @$cas_val);
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
      $pairs_by_type->{ $np->{type} } = [
        List::UtilsBy::uniq_by {
          sprintf('%s/%s/%s', $_->{type}, $_->{primary}, $_->{secondary})
        }
        ($np, $pairs_by_type->{ $np->{type} }->@*)
      ];
    }
    return $pairs_by_type;
  }

  sub get_pairs_by_type ($self) {
    return $self->pair_cache->get('pairs_by_type');
  }

}
1;
__END__
