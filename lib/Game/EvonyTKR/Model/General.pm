use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require UUID;

require Game::EvonyTKR::Model::BasicAttributes;
require Game::EvonyTKR::Model::BasicAttribute;

package Game::EvonyTKR::Model::General {
  use Moo;
  extends 'Game::EvonyTKR::Model::Base';
  with 'Game::EvonyTKR::Role::Constants::BuffConstants';
  with 'Game::EvonyTKR::Role::Constants::GeneralConstants';
  with 'Game::EvonyTKR::Role::Constants::AscendingAttributes';
  use JSON::PP;
  use UUID           qw(uuid5);
  use List::AllUtils qw( any none all );
  use File::FindLib 'lib';
  use Carp;
  use namespace::autoclean;
  use overload
    '""'       => \&as_string,
    'eq'       => \&equality,
    'bool'     => \&_isTrue,
    "fallback" => 1;

  our $VERSION = 'v0.40.0';

  has id => (
    is      => 'ro',
    lazy    => 1,
    default => sub {
      my ($self) = @_;
      my $general_type =
        ref($self->type) eq 'ARRAY' ? $self->type->[0] : $self->type;
      if (exists $self->UUID5_Generals->{$general_type}) {
        return uuid5($self->UUID5_Generals->{$general_type}, $self->name);
      }
      return $self->name;
    }
  );

  has name => (is => 'rw');

  has type => (is => 'rw');

  has ascendingAttributes => (is => 'rw');

  has builtInBookName => (is => 'rw');

  has builtInBook => (is => 'rw');

  has specialtyNames => (
    is      => 'rw',
    default => sub { [] }
  );

  has specialties => (
    is      => 'rw',
    default => sub { [] }
  );

  has ascending => (
    is      => 'rw',
    default => sub {0}
  );

  has stars => (
    is      => 'rw',
    default => sub {'none'}
  );

  has basicAttributes => (
    is      => 'rw',
    lazy    => 1,
    default => sub {
      return Game::EvonyTKR::Model::BasicAttributes->new();
    }
  );

# Precomputed generic book buff values by activation type and level
# Structure: { Attacking => { level1 => {march_size => 3, ...}, level2 => {...}, ... }, ... }
  has genericBookBuffs => (
    is      => 'rw',
    default => sub { {} }
  );

  sub set_general_id($self) {
    if (ref $self->type) {
      my @ts;
      push @ts, $self->type->@*;
      my $ut = $ts[0];
      $self->logger->debug("using type $ut");
      my $uuid5base = $self->UUID5_Generals->{$ut};
      $self->id = uuid5($uuid5base, $self->name);
    }
    else {
      my $uuid5base = $self->UUID5_Generals->{ $self->type };
      $self->id = uuid5($uuid5base, $self->normalize($self->name));
    }
  }

  sub validate($self) {
    my @errors;
    if (not defined $self->GeneralKeys) {
      $self->logger->logcroak('GeneralKeys is not defined in a General');
    }
    if (not defined $self->type) {
      push @errors,
        sprintf('type must be one of %s', join(', ', @{ $self->GeneralKeys }));
    }
    elsif (not $self->ValidateGeneralType($self->type)) {
      push @errors, 'General Type Failed Validation';
    }

    my @valv;
    map { push @valv, $_ } $self->AscendingAttributeLevelValues();
    map { push @valv, $_ } $self->AscendingAttributeLevelValues(0);
    if (none { $self->stars =~ /$_/ } @valv) {
      push @errors,
        sprintf(
        'stars must be one of %s, not "%s"',
        join(',', @valv),
        $self->stars
        );
    }

    if (@errors) {
      $self->logger->logcroak(join ', ', @errors);
      return;
    }
    return 1;
  }

  sub populateAscendingAttributes ($self,) {
    return;
  }

  sub populateBuiltinBook ($self) {
    return;
  }

  sub populateSpecialties ($self,) {
    return;
  }

  sub populateGenericBooks ($self) {
    return;
  }

  # Helper method to map a buff to a column name (same logic as PDL Compiler)
  sub _map_buff_to_column ($self, $buff) {
    my $attribute     = lc($buff->{attribute} || '');
    my $targeted_type = $buff->{targetedType} || '';
    my $conditions    = $buff->{conditions}   || [];

    # Normalize attribute names
    $attribute =~ s/\s+/_/g;

    # Check if it's a debuff
    my $is_debuff = grep { $_ eq 'Enemy' } @$conditions;

    # Simple attributes that don't need troop type
    return 'march_size'       if $attribute eq 'march_size';
    return 'death_to_wounded' if $attribute eq 'death_to_wounded';
    return 'marching_speed'   if $attribute eq 'marching_speed';

    # Combat attributes need troop type suffix
    if ($attribute =~ /^(attack|defense|hp)$/) {
      my $buff_type = $attribute;

      # Determine troop type suffix
      my $suffix = 'all';    # Default to 'all' if no specific type

      if ($targeted_type =~ /ground/i) {
        $suffix = 'ground';
      }
      elsif ($targeted_type =~ /mounted/i) {
        $suffix = 'mounted';
      }
      elsif ($targeted_type =~ /ranged/i) {
        $suffix = 'ranged';
      }
      elsif ($targeted_type =~ /siege/i) {
        $suffix = 'siege';
      }

      # Add enemy prefix for debuffs
      my $prefix = $is_debuff ? 'enemy_' : '';
      return "${prefix}${buff_type}_${suffix}";
    }

    # Unknown attribute
    return undef;
  }

  sub from_hash ($class, $hashObject) {
    my $logger = Log::Handler->get_logger(__PACKAGE__);

    if (!exists $hashObject->{name}) {
      $logger->error('hash object must contain a name attribute.');
      return undef;
    }

    my $g = Game::EvonyTKR::Model::General->new(
      name            => $hashObject->{name},
      type            => $hashObject->{type},
      ascending       => $hashObject->{ascending},
      stars           => $hashObject->{stars},
      builtInBookName => $hashObject->{book},
      specialtyNames  => $hashObject->{specialties},
    );
    unless ($g->validate()) {
      $logger->error('Invalid Hash Object.');
      return;
    }

    foreach my $baKey (keys %{ $hashObject->{basic_attributes} }) {
      my $ba = Game::EvonyTKR::Model::BasicAttribute->new(
        attribute_name => $baKey,
        base           => $hashObject->{basic_attributes}->{$baKey}->{base},
        increment => $hashObject->{basic_attributes}->{$baKey}->{increment},
      );
      $g->basicAttributes->setAttribute($baKey, $ba);
    }

    return $g;
  }

  sub to_hash ($self) {
    return {
      __CLASS__       => __PACKAGE__,
      id              => $self->id,
      name            => $self->name,
      type            => $self->type,
      basicAttributes => $self->basicAttributes,
      ascending       => $self->ascending,
      builtInBookName => $self->builtInBookName,
      specialtyNames  => $self->specialtyNames,
    };
  }

  sub to_wire_hash ($self) {
    my $hash = {
      _v                  => 1,
      id                  => $self->id,
      name                => $self->name,
      type                => $self->type,
      ascending           => $self->ascending,
      builtInBookName     => $self->builtInBookName,
      specialtyNames      => $self->specialtyNames,
      stars               => $self->stars,
      ascendingAttributes => $self->ascendingAttributes,
    };

    # Handle basicAttributes if it exists
    if ($self->basicAttributes) {
      $hash->{basicAttributes} = $self->basicAttributes->to_hash();
    }

    return $hash;
  }

  sub from_wire_hash ($class, $w, $opts = {}) {
    my $logger = Log::Handler->get_logger(__PACKAGE__);
    unless (($w->{_v} // 1) == 1) {
      $logger->error('unknown wire version');
      die "unknown wire version";
    }

    my $general = $class->new(
      name            => $w->{name},
      type            => $w->{type},
      ascending       => $w->{ascending} // 0,
      builtInBookName => $w->{builtInBookName},
      specialtyNames  => $w->{specialtyNames} // [],
      stars           => $w->{stars}          // 'none',
    );
    $general->populateBuiltinBook()
      unless (exists $opts->{populateBuiltinBook}
      && $opts->{populateBuiltinBook} == 0);
    $general->populateAscendingAttributes()
      unless (exists $opts->{populateAscendingAttributes}
      && $opts->{populateAscendingAttributes} == 0);
    $general->populateSpecialties()
      unless (exists $opts->{populateSpecialties}
      && $opts->{populateSpecialties} == 0);
    $general->populateGenericBooks()
      unless (exists $opts->{populateGenericBooks}
      && $opts->{populateGenericBooks} == 0);

    # Handle basicAttributes if it exists
    if ($w->{basicAttributes}) {
      $general->basicAttributes(
        Game::EvonyTKR::Model::BasicAttributes->from_hash(
          $w->{basicAttributes}
        )
      );
    }

    return $general;
  }

  sub TO_JSON {
    my $self = shift;
    return $self->to_wire_hash();
  }

  sub as_string {
    my $self = shift;
    my $json =
      JSON::PP->new->utf8(0)->pretty->canonical(1)
      ->allow_blessed(1)
      ->convert_blessed(1)
      ->encode($self->to_hash());
    return $json;
  }

  sub equality($self, $other, $swap = 0) {
    my $one = $swap ? $other : $self;
    my $two = $swap ? $self  : $other;
    my $on  = '';
    my $tn  = '';
    if (ref($one) && $one->isa('Game::EvonyTKR::Model::General')) {
      $on = $one->name;
    }
    else {
      $on = "$one";
    }
    if (ref($two) && $two->isa('Game::EvonyTKR::Model::General')) {
      $tn = $two->name;
    }
    else {
      $tn = "$two";
    }
    return $on eq $tn;
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
