use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Game::EvonyTKR::Shared::Constants;
require Data::Printer;
require Path::Tiny;
require YAML::PP;
use namespace::clean;

class Game::EvonyTKR::Model::Glossary : isa(Game::EvonyTKR::Shared::Constants) {
  use Carp;

  field $term          : param : reader;
  field $definition    : param : reader;
  field $synonyms      : param : reader //= [];
  field $related_terms : param : reader //= [];
  field $examples      : param : reader //= [];
  field $owner         : param : reader //= '';
  field $status        : param : reader //= 'approved';

  # Check if this term matches a search (term or synonym)
  method matches_search($search_term) {
    return 1 if lc($term) eq lc($search_term);

    foreach my $synonym (@$synonyms) {
      return 1 if lc($synonym) eq lc($search_term);
    }

    return 0;
  }

  # Get the first letter for grouping
  method first_letter() {
    return uc(substr($term, 0, 1));
  }

  # Convert to hash for JSON serialization
  method to_hash() {
    return {
      term          => $term,
      definition    => $definition,
      synonyms      => $synonyms,
      related_terms => $related_terms,
      examples      => $examples,
      owner         => $owner,
      status        => $status,
    };
  }

  # JSON serialization method
  method TO_JSON() {
    return $self->to_hash();
  }
}

1;
