package Game::EvonyTKR::Role::Persistence::Glossary;
use v5.42.0;
use utf8::all;
use Mojo::Base -role,                                     -signatures;
with 'Game::EvonyTKR::Role::Persistence::Core';

sub add_glossary_term ($self, $term) {
  my $name = $term->term;
  return $self->persistence->store_glossary_term($name, $term->to_wire_hash());
}

sub get_glossary_term ($self, $term_name) {
  require Game::EvonyTKR::Model::Glossary;

  state $glossary_terms = {};

  my $normalized_name = lc($self->normalize($term_name));

  if (exists $glossary_terms->{$normalized_name}) {
    $self->logger->debug("Returning glossary term $term_name from state cache");
    return $glossary_terms->{$normalized_name};
  }

  # Load directly from SQLite
  my $wire_data = $self->persistence->get_glossary_term($term_name);

  return unless defined($wire_data);

  my $term = Game::EvonyTKR::Model::Glossary->from_wire_hash($wire_data);
  $glossary_terms->{$normalized_name} = $term if defined($term);
  return $term;
}

sub list_glossary_terms ($self) {
  return [values $self->persistence->get_all_glossary_terms()->%*];
}

1;
__END__

=head1 NAME

Game::EvonyTKR::Role::Persistence::Glossary - Glossary term persistence operations

=head1 DESCRIPTION

Handles loading and caching of glossary terms.

=cut
