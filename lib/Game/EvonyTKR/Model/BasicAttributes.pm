use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require JSON::PP;
require Game::EvonyTKR::Shared::Constants::BuffConstants;
use namespace::autoclean;

package Game::EvonyTKR::Model::BasicAttributes {
  use Mojo::Base -base, -signatures;
  use Mojo::Base 'Game::EvonyTKR::Shared::Constants::BuffConstants', -role;
# VERSION
  use Carp;
  use List::AllUtils qw( any none first );
  use Data::Printer;
  use File::FindLib 'lib';
  use overload
    '<=>'      => \&_comparison,
    '=='       => \&_equality,
    '""'       => \&as_string,
    'fallback' => 1;

  my $logger = Game::EvonyTKR::Log::Config->logger();

  # Store as a hash for easy access
  has '_attributes' => sub { {} };

  # Add/set an attribute (fluent interface)
  sub set_attribute ($self, $basic_attr) {
    my $name = $basic_attr->attribute_name;
    $logger->logcroak("Invalid attribute name: $name")
      unless grep { $_ eq $name } $self->BasicAttributeTypes->@*;

    $self->_attributes->{$name} = $basic_attr;
    return $self;    # fluent
  }

  # Get specific attribute
  sub get_attribute ($self, $name) {
    return $self->_attributes->{$name};
  }

  # Validate we have all 4
  sub validate ($self) {
    my $attrs = $self->_attributes;
    for my $required ($self->BasicAttributeTypes->@*) {
      $logger->logcroak("Missing required attribute: $required")
        unless exists $attrs->{$required};
    }
    return 1;
  }

  sub total($self, $level = 1, $stars = 'none', $name = "GeneralName") {
    my $total = 0;
    foreach my $stat (
      Game::EvonyTKR::Shared::Constants::BuffConstants->BasicAttributeTypes->@*)
    {
      $total += $self->get_attribute($stat)->total($level, $stars, $name);
    }
    return $total;
  }

  sub _comparison ($self, $other, $swap = 0) {
    die "Cannot compare BasicAttributes with " . ref($other)
      unless blessed($other) && $other->isa(__PACKAGE__);

    my $my_total    = $self->total();
    my $other_total = $other->total();

    return $swap ? $other_total <=> $my_total : $my_total <=> $other_total;
  }

  sub _equality ($self, $other, $swap = 0) {
    return 0 unless blessed($other) && $other->isa(__PACKAGE__);

    # Check each attribute for equality
    for my $stat ($self->BasicAttributeTypes->@*) {
      my $mine   = $self->get_attribute($stat);
      my $theirs = $other->get_attribute($stat);
      return 0 unless $mine == $theirs;    # Uses BasicAttribute's equality
    }
    return 1;
  }

  sub to_hash ($self) {
    return {
      attack     => $self->_attribute->{attack},
      defense    => $self->_attribute->{defense},
      leadership => $self->_attribute->{leadership},
      politics   => $self->_attribute->{politics},
    };
  }

  # Method for JSON serialization
  sub TO_JSON ($self) {
    return $self->to_hash();
  }

  # Stringification method using JSON
  sub as_string ($self) {
    my $json =
      JSON::PP->new->utf8->canonical(1)
      ->allow_blessed(1)
      ->convert_blessed(1)
      ->encode($self->to_hash());
    return $json;
  }

}
1;
__END__
# ABSTRACT: Stores the collection of what Evony refers to as the Basic Attributes for a Game::EvonyTKR::Model::General
