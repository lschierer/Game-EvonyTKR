use v5.42.0;
use utf8::all;
use File::FindLib 'lib';
require JSON::PP;
use namespace::autoclean;

package Game::EvonyTKR::Model::Book {
  use Mojo::Base -base,                               -signatures;
  use Mojo::Base 'Game::EvonyTKR::Role::Logger',      -role;
  use Mojo::Base 'Game::EvonyTKR::Model::Role::Book', -role;
  use Carp;
  use overload
    '""'       => \&as_string,
    '.'        => \&concat,
    'bool'     => \&_isTrue,
    'fallback' => 0;

  has ['name', 'text'] => '';
  has 'buffs'          => sub { [] };

  # for backwards compatibility
  sub buff ($self) {
    return [$self->buffs->@*];
  }

  sub to_hash ($self) {
    my $hash = {
      __CLASS__ => __PACKAGE__,
      name      => $self->name,
      buffs     => $self->buffs,
    };
    if (length($self->text)) {
      $hash->{text} = $self->text;
    }
    if ($self->can('validate_level')) {
      $hash->{level} = $self->level;
    }
    $hash->{_roles} = [
      $self->can('is_builtin')
        && $self->is_builtin == 1
      ? 'Game::EvonyTKR::Model::Role::Book::Builtin'
      : (),
      $self->can('validate_level')
      ? 'Game::EvonyTKR::Model::Role::Book::SkillBook'
      : (),
    ];
    return $hash;
  }

  sub to_wire_hash ($self) {
    my $hash = {
      _v   => 1,
      name => $self->name,
      text => $self->text // '',
    };

    foreach my $b ($self->buffs->@*) {
      push @{ $hash->{buffs} }, $b->to_wire_hash();
    }

    if ($self->can('validate_level')) {
      $hash->{level} = $self->level;
    }
    $hash->{_roles} = [
      $self->can('is_builtin')
        && $self->is_builtin == 1
      ? 'Game::EvonyTKR::Model::Role::Book::Builtin'
      : (),
      $self->can('validate_level')
      ? 'Game::EvonyTKR::Model::Role::Book::SkillBook'
      : (),
    ];
    return $hash;
  }

  sub from_wire_hash ($class, $w) {
    die "unknown wire version" unless ($w->{_v} // 1) == 1;

    my $b = $class->new(name => $w->{name});

    # Apply roles based on _roles array
    if ($w->{_roles} && @{ $w->{_roles} }) {
      $b = $b->with_roles(@{ $w->{_roles} });
    }

    # Set level if it exists (for SkillBook role)
    if (exists $w->{level}) {
      $b->level($w->{level});
    }

    # Set text
    if (exists $w->{text} && length($w->{text})) {
      $b->text($w->{text});
    }

    # Add buffs
    if ($w->{buffs} && @{ $w->{buffs} }) {
      foreach my $buff_data (@{ $w->{buffs} }) {
        my $buff = Game::EvonyTKR::Model::Buff->from_wire_hash($buff_data);
        push @{ $b->buffs }, $buff;
      }
    }

    return $b;
  }

  sub TO_JSON {
    my $self = shift;
    return $self->to_hash();
  }

  sub as_string ($self, @args) {
    if ($self->can('validate_level')) {
      return sprintf('"%s %s: %s"', $self->level, $self->name, $self->text);
    }
    return sprintf('"%s: %s"', $self->name, $self->text);
  }

  sub concat ($self, $other, $swap = 0) {
    my $one = $swap ? $other : $self;
    my $two = $swap ? $self  : $other;
    return "$one" . "$two";
  }

  sub _isTrue ($self, $other = undef, $swap = undef) {
    return
         defined($self)
      && ref($self)
      && blessed($self)
      && $self->isa('Game::EvonyTKR::Model::Book');
  }
}
1;
__END__
