package Game::EvonyTKR::Role::Persistence::AscendingAttributes;
use v5.42.0;
use utf8::all;
use Mojo::Base -role, -signatures;
with 'Game::EvonyTKR::Role::Persistence::Core';

sub add_ascending_attribute ($self, $ascendingAttribute) {
  my $name = lc($self->normalize($ascendingAttribute->general));
  return $self->persistence->store_ascending_attribute($name,
    $ascendingAttribute->to_wire_hash());
}

sub get_ascending_attributes ($self, $name) {
  require Game::EvonyTKR::Model::Factory;

  state $AscendingAttributes = {};

  $self->logger->debug("get_ascending_attribute called for: $name");

  my $normalized_name = lc($self->normalize($name));

  if (exists $AscendingAttributes->{$normalized_name}) {
    $self->logger->debug(
      "Returning Ascending Attributes $name from state cache");
    return $AscendingAttributes->{$normalized_name};
  }

  # Load directly from Redis
  my $wire_data = $self->persistence->get_ascending_attribute($normalized_name);

  return unless defined($wire_data);

  $self->logger->debug(
    "Found wire_data, attempting to build AscendingAttributes");
  my $ascendingAttribute =
    Game::EvonyTKR::Model::Factory->build_from_wire('AscendingAttributes',
    $wire_data);

  unless (defined($ascendingAttribute)) {
    $self->logger->error(
      "Factory failed to build AscendingAttributes from wire_data");
    return;
  }

  $self->logger->debug(
    "Successfully built ascendingAttributes: " . $ascendingAttribute->general);
  $AscendingAttributes->{$normalized_name} = $ascendingAttribute;
  return $ascendingAttribute;
}

# Alias for singular form
sub get_ascending_attribute ($self, $name) {
  return $self->get_ascending_attributes($name);
}

sub list_ascending_attributes ($self) {
  require Mojo::File;
  require Mojo::Home;
  require List::UtilsBy;

  my $mh =
    Mojo::File->new(Mojo::Home->new->detect('Game::EvonyTKR')->to_string());
  my $collectionDir          = $mh->child('share/collections/data');
  my $AscendingAttributesDir = $collectionDir->child('ascending attributes');
  my @suffixlist             = ('.yaml', '.yml');
  my @files =
    $AscendingAttributesDir->list->grep(sub { $_ =~ /\.ya?ml$/ && -f -r $_ })
    ->sort->map(sub { return $_->basename(@suffixlist) })->each;
  return [List::UtilsBy::uniq_by { lc($self->normalize($_)) } @files];
}

1;
__END__

=head1 NAME

Game::EvonyTKR::Role::Persistence::AscendingAttributes - Ascending attribute persistence operations

=cut
