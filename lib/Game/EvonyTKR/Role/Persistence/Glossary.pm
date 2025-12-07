package Game::EvonyTKR::Role::Persistence::Glossary;
use v5.42.0;
use utf8::all;
use Mojo::Base -role,                                     -signatures;
use Mojo::Base 'Game::EvonyTKR::Role::Persistence::Core', -role;

sub add_glossary_term ($self, $term) {
  my $name = $term->term;
  $self->persistence->store_glossary_term($name, $term->to_wire_hash());
  return 1;
}

sub get_glossary_term ($self, $term_name) {
  require Game::EvonyTKR::Model::Glossary;

  state $glossary_terms = {};

  my $normalized_name = lc($self->normalize($term_name));
  $normalized_name =~ s/ /_/g;

  if (exists $glossary_terms->{$normalized_name}) {
    $self->log_debug("Returning glossary term $term_name from state cache");
    return $glossary_terms->{$normalized_name};
  }

  # Load directly from SQLite
  my $wire_data = $self->persistence->get_glossary_term($term_name);

  return unless defined($wire_data);

  my $term = Game::EvonyTKR::Model::Glossary->from_wire_hash($wire_data);
  $glossary_terms->{$normalized_name} = $term if defined($term);
  return $term;
}

1;
__END__

=head1 NAME

Game::EvonyTKR::Role::Persistence::Glossary - Glossary term persistence operations

=head1 DESCRIPTION

Handles loading and caching of glossary terms.

=cut
