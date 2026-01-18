package Game::EvonyTKR::Model::Glossary;
use v5.42.0;
use utf8::all;
use Moo;
extends 'Game::EvonyTKR::Model::Base';
use overload
  '""'       => \&as_string,
  '<=>'      => \&_comparison,
  'cmp'      => \&_comparison,
  'bool'     => sub { my $self = shift; $self->_isTrue },
  "fallback" => 1;

has 'term'  => ( is => 'lazy' );
has 'definition'  => ( is => 'lazy' );
has 'rendered_def'  => ( is => 'lazy' );
has 'synonyms'      => (is => 'lazy', default =>  sub { [] });
has 'related_terms' => (is => 'lazy', default =>  sub { [] });
has 'examples'      => (is => 'lazy', default =>  sub { [] });
has 'owner'  => ( is => 'lazy', default => '' );
has 'status'        => ( is => 'lazy', default => 'approved' );

# Check if this term matches a search (term or synonym)
sub matches_search ($self, $search_term) {
  return 1 if lc($self->term) eq lc($search_term);

  foreach my $synonym (@{ $self->synonyms }) {
    return 1 if lc($synonym) eq lc($search_term);
  }

  return 0;
}

# Get the first letter for grouping
sub first_letter ($self) {
  return uc(substr($self->term, 0, 1));
}

# Convert to wire format for storage
sub to_wire_hash ($self) {
  return {
    term          => $self->term,
    definition    => $self->definition,
    synonyms      => $self->synonyms,
    related_terms => $self->related_terms,
    examples      => $self->examples,
    owner         => $self->owner,
    status        => $self->status,
  };
}

# Create from wire format
sub from_wire_hash ($class, $wire) {
  return $class->new(
    term          => $wire->{term},
    definition    => $wire->{definition},
    synonyms      => $wire->{synonyms}      // [],
    related_terms => $wire->{related_terms} // [],
    examples      => $wire->{examples}      // [],
    owner         => $wire->{owner}         // '',
    status        => $wire->{status}        // 'approved',
  );
}

sub _comparison ($self, $other, $swap = 0) {
  die "Cannot compare BasicAttributes with " . ref($other)
    unless blessed($other) && $other->isa(__PACKAGE__);

  return $swap ? $other->term cmp $self->term : $self->term cmp $other->term;
}

1;
__END__
