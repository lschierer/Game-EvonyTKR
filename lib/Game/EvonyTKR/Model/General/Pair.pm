use v5.42.0;
use experimental qw(class);
use utf8::all;

require Scalar::Util;

require Game::EvonyTKR::Model::General;

package Game::EvonyTKR::Model::General::Pair {
  use Moo;
  extends 'Game::EvonyTKR::Model::Base';
  with 'Game::EvonyTKR::Role::Constants::BuffConstants';
  with 'Game::EvonyTKR::Role::Constants::GeneralConstants';
  with 'Game::EvonyTKR::Role::Constants::AscendingAttributes';
  use UUID           qw(uuid5);
  use List::AllUtils qw( any none );
  use File::FindLib 'lib';
  use Carp;
  use overload
    '""'       => \&to_string,
    '<=>'      => \&compare,
    'cmp'      => \&compare,
    'bool'     => \&_isTrue,
    "fallback" => 1;

  has [qw(primary secondary type)] => (is => 'rw');

# Precomputed generic book buff values by activation type and level
# Structure: { Attacking => { level3 => {march_size => 12, ...}, level6 => {...}, ... }, ... }
# For pairs, level3 = single general's 3 books, level6 = both generals' 6 books combined
  has 'genericBookBuffs' => (is => 'rw', default => sub { {} });

  sub to_hash ($self) {
    my $h = {
      primary   => $self->primary->to_hash(),
      secondary => $self->secondary->to_hash(),
      type      => $self->type,
    };
    return $h;
  }

  sub to_string {
    my $self = shift;
    my $json =
      JSON::PP->new->utf8(0)->pretty->canonical(1)
      ->allow_blessed(1)
      ->convert_blessed(1)
      ->encode($self->to_hash());
    return $json;
  }

  sub to_wire_hash ($self) {
    my $h = {
      primary   => { name => $self->primary->name },
      secondary => { name => $self->secondary->name },
      type      => $self->type,
    };
    return $h;
  }

  sub from_wire_hash ($class, $h, %opts) {
    my $logger = Log::Handler->get_logger(__PACKAGE__);

    my $primary_name =
      ref($h->{primary}) eq 'HASH'
      ? $h->{primary}->{name}
      : $h->{primary};
    my $secondary_name =
      ref($h->{secondary}) eq 'HASH'
      ? $h->{secondary}->{name}
      : $h->{secondary};

    unless (defined($primary_name) && length($primary_name)) {
      $logger->error('hash object must contain a primary with a name in it.');
      return;
    }

    state $general_helper;
    unless ($general_helper) {
      eval { $general_helper = Game::EvonyTKR::Model::Base->new(); } or do {
        $logger->error(
          sprintf('eval failed; cannot define general helper: "%s"', $@));
        return;
      };
    }

    # Pass options down to get_general (e.g., populateGenericBooks => 0)
    my $primary = $general_helper->get_general($primary_name, \%opts);
    unless ($primary) {
      $logger->error(
        sprintf('cannot retrieve general for %s when creating a pair.',
          $primary_name)
      );
      return;
    }
    my $secondary = $general_helper->get_general($secondary_name, \%opts);
    unless ($secondary) {
      $logger->error(
        sprintf('cannot retrieve general for %s when creating a pair.',
          $secondary_name)
      );
      return;
    }

    unless ($h
      && ref($h)
      && ref($h) eq 'HASH'
      && exists $h->{type}
      && length($h->{type})) {
      $logger->error('hash object must contain a type.');
      return;
    }

    my $pair = $class->new(
      primary   => $primary,
      secondary => $secondary,
      type      => $h->{type},
    );

    # Populate generic books for the pair
    $pair->populateGenericBooks();

    return $pair;
  }

  sub populateGenericBooks ($self) {
    $self->logger->debug(sprintf(
      'populateGenericBooks called for pair: %s + %s',
      $self->primary->name, $self->secondary->name
    ));

    return;
  }

  sub compare ($self, $other, $swapped = undef) {
    my ($a, $b) = $swapped ? ($other, $self) : ($self, $other);
    if ($a
      && Scalar::Util::blessed($a) =~ /Game::EvonyTKR::Model::General::Pair/) {
      if ($b
        && Scalar::Util::blessed($b) =~ /Game::EvonyTKR::Model::General::Pair/)
      {
        return $a->primary->name cmp $b->primary->name
          || $a->secondary->name cmp $b->secondary->name;
      }
      else {
        return $a->primary->name cmp "$b"
          || $a->secondary->name cmp "$b";
      }
    }
    elsif ($b
      && Scalar::Util::blessed($b) =~ /Game::EvonyTKR::Model::General::Pair/) {
      return "$a" cmp $b->primary->name
        || "$a" cmp $b->secondary->name;
    }
    else {
      return "$a" cmp "$b";
    }
  }

  sub _isTrue ($self, $other = undef, $swap = undef) {
    return
         defined($self)
      && ref($self)
      && blessed($self)
      && $self->isa(__PACKAGE__);
  }
}
1;
__END__
