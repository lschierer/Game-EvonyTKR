use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require JSON::PP;
use namespace::autoclean;

package Game::EvonyTKR::Model::BasicAttributes {
  use Mojo::Base -base,                                            -signatures;
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::BuffConstants', -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::GeneralConstants', -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Logger',                      -role;
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

  has 'attack' => sub {
    Game::EvonyTKR::Model::BasicAttribute->new(attribute_name => 'attack');
  };
  has 'leadership' => sub {
    Game::EvonyTKR::Model::BasicAttribute->new(attribute_name => 'leadership');
  };
  has 'defense' => sub {
    Game::EvonyTKR::Model::BasicAttribute->new(attribute_name => 'defense');
  };
  has 'politics' => sub {
    Game::EvonyTKR::Model::BasicAttribute->new(attribute_name => 'politics');
  };

  # Get specific attribute
  sub get_attribute ($self, $name) {
    return $self->$name;
  }

  sub total($self, $level = 1, $stars = 'none', $name = "GeneralName") {
    my $total = 0;
    foreach my $stat (
      Game::EvonyTKR::Role::Constants::BuffConstants->BasicAttributeTypes->@*) {
      $total += $self->$stat->total($level, $stars, $name);
    }
    return $total;
  }

  sub setAttribute($self, $attributeName, $newAttribute) {
    if (none { $_ =~ $attributeName } $self->basic_types->@*) {
      $self->logger->error(sprintf(
        'attributeName must be one of %s, not %s',
        Data::Printer::np($self->AttributeValues),
        $attributeName,
      ));
      return;
    }

    unless (ref($newAttribute)
      && $newAttribute->isa('Game::EvonyTKR::Model::BasicAttribute')) {
      $self->logger->error(sprintf(
        'newAttribute must be a %s not a %s',
        'Game::EvonyTKR::Model::BasicAttribute',
        blessed $newAttribute
      ));
      return;
    }
    if ($self->can($attributeName)) {
      $self->$attributeName($newAttribute);
    }
    else {
      $self->logger->error(sprintf(
        'cannot find method "%s" in "%s"',
        $attributeName, blessed($self)
      ));
    }
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
      my $mine   = $self->$stat;
      my $theirs = $other->$stat;
      return 0 unless $mine == $theirs;    # Uses BasicAttribute's equality
    }
    return 1;
  }

  sub from_hash ($class, $hashObject) {
    my $logger = Game::EvonyTKR::Log::Config->logger();
    unless (ref($hashObject) eq 'HASH') {
      $logger->error(sprintf(
        '%s from_hash requires a hash as a parameter, not %s',
        __PACKAGE__, Data::Printer::np($hashObject)
      ));
      return;
    }
    my $return = $class->new();

    foreach my $baKey (keys $hashObject->%*) {
      unless ($return->can($baKey)) {
        $logger->error(sprintf('hash has an invalid key %s', $baKey));
        return;
      }
      my $ba = Game::EvonyTKR::Model::BasicAttribute->new(
        attribute_name => $baKey,
        base           => $hashObject->{$baKey}->{base},
        increment      => $hashObject->{$baKey}->{increment},
      );
      $return->setAttribute($baKey, $ba);
    }
    return $return;
  }

  sub to_hash ($self) {
    return {
      attack     => $self->attack,
      defense    => $self->defense,
      leadership => $self->leadership,
      politics   => $self->politics,
    };
  }

  # Method for JSON serialization
  sub TO_JSON ($self) {
    return $self->to_hash();
  }

  # Stringification method using JSON
  sub as_string {
    my $self = shift;
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
