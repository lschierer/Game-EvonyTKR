package Game::EvonyTKR::Role::Persistence::Covenants;
use v5.42.0;
use utf8::all;
use Mojo::Base -role,                                     -signatures;
use Mojo::Base 'Game::EvonyTKR::Role::Persistence::Core', -role;

sub add_covenant ($self, $covenant) {
  my $name = lc($self->normalize($covenant->primary->name));
  return $self->persistence->store_covenant($name, $covenant->to_wire_hash());
}

sub get_covenant ($self, $name) {
  require Game::EvonyTKR::Model::Factory;

  state $covenants = {};

  my $normalized_name = lc($self->normalize($name));

  if (exists $covenants->{$normalized_name}) {
    $self->log_debug("Returning covenant $name from state cache");
    return $covenants->{$normalized_name};
  }

  # Load directly from SQLite
  my $wire_data = $self->persistence->get_covenant($normalized_name);

  return unless defined($wire_data);

  my $covenant =
    Game::EvonyTKR::Model::Factory->build_from_wire('Covenant', $wire_data);
  $covenants->{$normalized_name} = $covenant if defined($covenant);
  return $covenant;
}

sub list_covenants ($self) {
  require Mojo::File;
  require Mojo::Home;
  require List::UtilsBy;

  my $mh =
    Mojo::File->new(Mojo::Home->new->detect('Game::EvonyTKR')->to_string());
  my $collectionDir = $mh->child('share/collections/data');
  my $cdir          = $collectionDir->child('covenants');
  my @suffixlist    = ('.yaml', '.yml');
  my @files         = $cdir->list->grep(sub { $_ =~ /\.ya?ml$/ && -f -r $_ })
    ->sort->map(sub { return $_->basename(@suffixlist) })->each;
  return [List::UtilsBy::uniq_by { lc($self->normalize($_)) } @files];
}

1;
__END__

=head1 NAME

Game::EvonyTKR::Role::Persistence::Covenants - Covenant persistence operations

=cut
