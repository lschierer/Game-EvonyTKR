package Game::EvonyTKR::Role::Persistence::Generals;
use v5.42.0;
use utf8::all;
use Mojo::Base -role,                                     -signatures;
use Mojo::Base 'Game::EvonyTKR::Role::Persistence::Core', -role;

sub store_buff_cache ($self, $key, $buffValues) {
  $key = lc($self->normalize($key));
  return $self->persistence->store_data('general_buff_cache', $key,
    $buffValues);
}

sub get_buff_cache ($self, $key) {
  $key = lc($self->normalize($key));
  return $self->persistence->get_data('general_buff_cache', $key);
}

sub add_general ($self, $general) {
  my $name = lc($self->normalize($general->name));
  # Normalize the name before storing to ensure consistent lookups
  my $normalized_name = $self->normalize($name);
  return $self->persistence->store_general($normalized_name,
    $general->to_wire_hash());
}

sub get_general ($self, $name, $opts = {}) {
  require Game::EvonyTKR::Model::Factory;

  state $generals = {};

  return if (not length($name));

  $self->log_debug("get_general called for: $name");

  my $normalized_name = lc($self->normalize($name)) // '';
  return unless (length($normalized_name));

  # Only use cache if using default options (full population)
  my $use_cache = !%$opts;

  if ($use_cache && exists $generals->{$normalized_name}) {
    $self->log_debug("Returning general $name from state cache");
    return $generals->{$normalized_name};
  }

  # Load directly from SQLite - use normalized name for lookup
  my $wire_data = $self->persistence->get_general($normalized_name);

  unless (defined($wire_data)) {
    $self->log_warn("No data found for: $name");
    return;
  }

  my $general =
    Game::EvonyTKR::Model::Factory->build_from_wire('General', $wire_data,
    $opts);

  unless (defined($general)) {
    $self->log_error("Factory failed to build general from wire_data");
    return;
  }

  $self->log_debug("Successfully built general: " . $general->name);

  # Only cache if using default options
  if ($use_cache) {
    $generals->{$normalized_name} = $general;
  }

  return $general;
}

sub get_generals ($self) {
  my $result = [];
  foreach my $name ($self->list_generals->@*) {
    my $g = $self->get_general($name);
    if ( $g
      && ref($g)
      && blessed($g)
      && $g->isa('Game::EvonyTKR::Model::General')) {
      push @{$result}, $g;
    }
  }

  if (scalar(@{$result}) != $self->persistence->count_generals()) {
    $self->log_error('unable to fetch all generals.');
  }
  return $result;
}

sub list_generals ($self) {
  require Mojo::File;
  require Mojo::Home;
  require List::UtilsBy;

  my $mh =
    Mojo::File->new(Mojo::Home->new->detect('Game::EvonyTKR')->to_string());
  my $collectionDir = $mh->child('share/collections/data');
  my $generalDir    = $collectionDir->child('generals');
  my @suffixlist    = ('.yaml', '.yml');
  my @files = $generalDir->list->grep(sub { $_ =~ /\.ya?ml$/ && -f -r $_ })
    ->sort->map(sub { return $_->basename(@suffixlist) })->each;
  my @returnlist = List::UtilsBy::uniq_by { lc($self->normalize($_)) } @files;
  return \@returnlist;
}

sub generate_buff_cache_key(
  $self,           $general,        $isPrimary,     $targetType,
  $activationType, $ascendingLevel, $covenantLevel, $specialty1,
  $specialty2,     $specialty3,     $specialty4
) {
  my $gn;
  if (defined($general) && ref($general) && $general->can('name')) {
    $gn = $general->name;
  }
  elsif (defined($general) && !ref($general) && length($general)) {
    $gn = $general;
  }
  else {
    $self->log_error('general name is required to generate a buff cache key');
    return '';
  }

  return sprintf(
    '%s:%s:%s:%s:%s:%s:%s:%s:%s:%s',
    $gn,             $isPrimary,     $targetType, $activationType,
    $ascendingLevel, $covenantLevel, $specialty1, $specialty2,
    $specialty3,     $specialty4
  );
}

1;
__END__

=head1 NAME

Game::EvonyTKR::Role::Persistence::Generals - General persistence operations

=cut
