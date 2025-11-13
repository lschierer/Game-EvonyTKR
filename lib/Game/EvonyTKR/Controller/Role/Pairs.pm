use v5.42.0;
use utf8::all;
use File::FindLib 'lib';
require Game::EvonyTKR::Service::Cache;
require Game::EvonyTKR::Model::Factory;
require Game::EvonyTKR::Model::General::Conflict;

package Game::EvonyTKR::Controller::Role::Pairs {
  use Mojo::Base -role, -signatures;
  use List::AllUtils qw(uniq);
  use List::UtilsBy;
  use Carp;

  has 'pair_cache' => sub ($job) {
    return Game::EvonyTKR::Service::Cache->new(namespace => 'pairs__');
  };

  sub pairs_by_type ($self, $new_pbt = undef) {
    state $pairs_by_type = {};
    if(defined($new_pbt) && ref($new_pbt) eq 'HASH'){
      $pairs_by_type = $new_pbt;
    }
    return $pairs_by_type;
  }

  has 'conflict_cache' => sub ($job) {
    return Game::EvonyTKR::Service::Cache->new(namespace => 'conflicts__');
  };

  sub get_conflict_detector ($self) {
    state $cd;
    unless ($cd) {
      $cd = $self->initialize_conflict_detector();
    }
    return $cd;
  }

  sub setup_pairs_by_type ($self) {
    my $pairs = $self->pairs_by_type();
    foreach my $key ($self->GeneralKeys->@*) {
      $pairs->{$key} = [] unless (ref($pairs) eq 'HASH' && exists $pairs->{$key});
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

    my $add_result = 0;

    my $pbt_cas_val = $self->pair_cache->gets('pairs_by_type');
    if(defined($pbt_cas_val) && ref($pbt_cas_val) && ref($pbt_cas_val) eq 'ARRAY'){
      my $npbt = $$pbt_cas_val[1];
      my %hash = map { $self->wire_pair_to_key($_) => $_ } ( $wire_pair, $npbt->{ $wire_pair->{type} }->@* );
      $npbt->{ $wire_pair->{type} } =  [
        sort { $self->wire_pair_to_key($a) cmp $self->wire_pair_to_key($b) } values %hash
      ];
      $$pbt_cas_val[1] = $npbt;
      my $pbt_result = $self->pair_cache->cas('pairs_by_type', @$pbt_cas_val);
      if($pbt_result){
        my $wp_result = $self->pair_cache->add($self->wire_pair_to_key($wire_pair), $wire_pair);
        if($wp_result){
          $self->pairs_by_type($npbt);
          $add_result = 1;
        }
      }
    }

    return $add_result;
  }

  sub get_pair ($self, $key){
    my $wire_pair = $self->pair_cache->get($key);
    unless($wire_pair){
      $self->logger->warn(sprintf('cannot find pair for key %s', $key));
      return;
    }
    my $pair = Game::EvonyTKR::Model::General::Pair->from_wire_hash($wire_pair);
    unless($pair){
      $self->logger->error(sprintf('cannot create pair from wire_pair %s/%s/%s; key %s',
      $wire_pair->{type}, $wire_pair->{primary}, $wire_pair->{secondary}, $key));
      return;
    }
    return $pair;
  }

  sub get_pairs_by_type ($self) {
    # Get pairs from cache and inflate them into objects
    my $pairs_by_type = $self->pair_cache->get('pairs_by_type') // {};
    my $inflated_pairs = {};

    foreach my $type (keys %$pairs_by_type) {
      $inflated_pairs->{$type} = [];
      foreach my $wire_pair (@{$pairs_by_type->{$type}}) {
        # Inflate wire pair into proper pair object
        if (my $pair_obj = Game::EvonyTKR::Model::General::Pair->from_wire_hash($wire_pair)) {
          push @{$inflated_pairs->{$type}}, $pair_obj;
        }
      }
    }

    return $inflated_pairs;
  }

  sub get_pair_list ($self, $requested_type = undef){
    my $list = [];
    my $pairs_by_type = $self->pair_cache->get('pairs_by_type') // {};

    foreach my $type (keys( $pairs_by_type->%* )){
      if(defined $requested_type && $type ne $requested_type){
        $self->logger->debug(sprintf('skipping type %s as it does not match requested type %s', $type, $requested_type));
        next;
      }
      $list = [ List::AllUtils::uniq ( $list->@*, map { $self->wire_pair_to_key($_) } $pairs_by_type->{$type}->@* ) ];
    }
    $list = [ sort $list->@*];
    return $list;
  }

  sub get_all_pairs ($self) {
    my $pairs_by_type =$self->get_pairs_by_type() // {};
    my $pairs = [];

    # Flatten all pairs from all types
    foreach my $type (keys %$pairs_by_type) {
      push @$pairs, @{$pairs_by_type->{$type}};
    }

    return $pairs;
  }

  sub initialize_conflict_detector($self, $conflict_detector = undef) {
    # Create or use provided conflict detector
    $conflict_detector //= Game::EvonyTKR::Model::General::Conflict::Book->new(
      build_index      => 1,
      asst_has_dragon  => 1,
      asst_has_spirit  => 1,
      allow_wall_buffs => 1,
    );

    # Load existing conflict data from cache
    my $cached_conflicts = $self->conflict_cache->get('merged_conflicts');

    if ($cached_conflicts) {
      $self->logger->debug('Loading existing conflict data from cache');

      $conflict_detector->preseed($cached_conflicts->{by_general}, $cached_conflicts->{groups_by_conflict_type});
    }

    return $conflict_detector;
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
