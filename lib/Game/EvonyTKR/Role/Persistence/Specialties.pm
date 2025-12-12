package Game::EvonyTKR::Role::Persistence::Specialties;
use v5.42.0;
use utf8::all;
use Mojo::Base -role,                                     -signatures;
use Mojo::Base 'Game::EvonyTKR::Role::Persistence::Core', -role;

our $roleLogger = Game::EvonyTKR::Role::Logging::get_logger(__PACKAGE__);

sub add_specialty ($self, $specialty) {
  my $normalized_name = lc($self->normalize($specialty->name));
  $self->persistence->store_specialty($normalized_name,
    $specialty->to_wire_hash());
  return 1;
}

sub get_specialty ($self, $name) {
  require Game::EvonyTKR::Model::Factory;

  my $normalized_name = lc($self->normalize($name));

  # Load directly from SQLite
  my $wire_data = $self->persistence->get_specialty($normalized_name);
  $roleLogger->debug(sprintf(
'%s get_specialty call attempted persistence retrieval of "%s" with key "%s" %s',
    __PACKAGE__,      $name,
    $normalized_name, defined($wire_data) ? 'successfully' : 'unsucessfully',
  ));
  return unless defined($wire_data);

  my $specialty =
    Game::EvonyTKR::Model::Factory->build_from_wire('Specialty', $wire_data);
  unless ($specialty) {
    $roleLogger->error(
      sprintf('failed to hydrate "%s" using factory', $wire_data->{name}));
  }
  return $specialty;
}

sub list_specialties ($self) {
  require Mojo::File;
  require Mojo::Home;
  require List::UtilsBy;

  my $mh =
    Mojo::File->new(Mojo::Home->new->detect('Game::EvonyTKR')->to_string());
  my $collectionDir = $mh->child('share/collections/data');
  my $sdir          = $collectionDir->child('specialties');
  my @suffixlist    = ('.yaml', '.yml');
  my @files         = $sdir->list->grep(sub { $_ =~ /\.ya?ml$/ && -f -r $_ })
    ->sort->map(sub { return $_->basename(@suffixlist) })->each;
  return [List::UtilsBy::uniq_by { lc($self->normalize($_)) } @files];
}

1;
__END__

=head1 NAME

Game::EvonyTKR::Role::Persistence::Specialties - Specialty persistence operations

=cut
