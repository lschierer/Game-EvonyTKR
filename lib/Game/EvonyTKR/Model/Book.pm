use v5.42.0;
use utf8::all;
use File::FindLib 'lib';
require JSON::PP;
use namespace::autoclean;

package Game::EvonyTKR::Model::Book {
  use Mojo::Base "Game::EvonyTKR::Model::Base";
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::BuffConstants',    -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::GeneralConstants', -role;
  use List::AllUtils qw( any none );
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

  sub get_buffs (
    $self, $attribute, $matching_type,
    $targetedType     = '',
    $conditions       = [],
    $debuffConditions = [],
  ) {
    $self->logger->debug(sprintf(
      'Calculating buffs for "%s", attribute: "%s"',
      $self->name, $attribute
    ));

    my $total = 0;

    # For buff matching, don't pass debuff conditions
    # For debuff matching, don't pass buff conditions
    my ($match_buff_conditions, $match_debuff_conditions);
    if ($matching_type eq 'buff') {
      $match_buff_conditions   = $conditions;
      $match_debuff_conditions = [];
    }
    else {
      $match_buff_conditions   = $conditions;
      $match_debuff_conditions = $debuffConditions;
    }

    foreach my $b ($self->buffs->@*) {
      my $matcher = Game::EvonyTKR::Model::Buff::Matcher->new(toTest => $b);
      my $logID   = int(rand(9e12)) + 1e12;
      if ($matcher->match(
        $attribute,             $targetedType,
        $match_buff_conditions, $match_debuff_conditions,
        $logID
      )) {
        my $val = $b->value->number;
        $self->logger->debug("  ➤ Match found. Adding $val to total.");
        $total += $val;
      }
      else {
        $self->logger->debug("  ✗ No match found.");
      }
    }

    $self->logger->debug(sprintf(
      '"%s": total for attribute "%s": "%s"',
      $self->name, $attribute, $total
    ));
    return $total;
  }

  sub addBuff ($self, $newBuff) {
    $self->logger->debug(sprintf('addBuff called for book "%s"', $self->name));

    if (!defined $newBuff) {
      $self->logger->warn("addBuff: newBuff is undefined");
      return;
    }

    my $reftype = Scalar::Util::reftype($newBuff);
    my $blessed = Scalar::Util::blessed($newBuff);

    $self->logger->debug(sprintf(
      'addBuff: newBuff reftype="%s", blessed="%s"',
      $reftype, ($blessed // 'undef')
    ));
    unless ($blessed && $newBuff->isa('Game::EvonyTKR::Model::Buff')) {
      $self->logger->logcroak(sprintf(
        'not adding totally bogus buff: reftype="%s", blessed="%s"',
        $reftype, ($blessed // 'undef')
      ));
      return;
    }

    my $classList = $blessed;
    $self->logger->debug(sprintf(
      'Adding buff of class "%s" to book "%s"',
      $classList, $self->name
    ));

    my @classStack = split(/::/, $classList);
    $self->logger->debug("Class stack: " . join(", ", @classStack));

    if (scalar @classStack > 3) {
      if ($classStack[3] eq 'Buff') {
        $self->logger->debug(sprintf('adding %s to %s', $newBuff, $self->name));

        push @{ $self->buffs }, $newBuff;
        $self->logger->debug(sprintf(
          'Book "%s" now has "%s" buffs',
          $self->name, scalar @{ $self->buffs }
        ));
      }
      else {
        $self->logger->warn(sprintf(
          'Not adding buff: class stack position 2 is "%s" not "Buff"',
          $classStack[2]));
      }
    }
    else {
      $self->logger->warn(
        "Not adding buff: class stack has fewer than 3 elements");
    }

  }

  sub validate($self) {
    my @errors;
    if (scalar @{ $self->buffs }) {
      for my $b (@{ $self->buffs }) {
        my $bc  = blessed $b;
        my @bcl = split(/::/, $bc);
        if (not($bcl[1] eq 'EvonyTKR' and $bcl[2] eq 'Buff')) {
          push @errors,
            sprintf(
            '$buff must contain type Game::EvonyTKR::Model::Buff not %s',
            $bc);
        }
      }
    }
    unless (not Scalar::Util::looks_like_number($self->name)) {
      push @errors =>
        sprintf('$name must contain a string, not %s', $self->name);
    }
    unless (not Scalar::Util::looks_like_number($self->text)) {
      push @errors =>
        sprintf('$text must contain a string, not %s', $self->text);
    }
    if (@errors) {
      $self->logger->logcroak(join ', ', @errors);
      return;
    }
  }

  sub from_hash($class, $object) {
    my $logger = Game::EvonyTKR::Role::Logging::get_logger(__PACKAGE__);
    my $b;
    if ($object->{name} =~ m/Level [1-4]/i) {
      $logger->debug(
        sprintf('detected that %s is a Generic book', $object->{name}));
      my $name = $object->{name} =~ s/Level [1-4]\s+//ir;
      my $level;
      if ($object->{name} =~ m/Level ([1-4])/i) {
        $level = $1;
      }

      $b = Game::EvonyTKR::Model::Book->new(name => $name,)
        ->with_roles('Game::EvonyTKR::Model::Role::Book::SkillBook');
      $b->level($level);
    }
    else {
      $logger->debug(
        sprintf('detected that %s is a builtin book', $object->{name}));
      $b = Game::EvonyTKR::Model::Book->new(name => $object->{name},)
        ->with_roles('Game::EvonyTKR::Model::Role::Book::Builtin');
    }
    if (exists $object->{text}) {
      $b->text($object->{text});
    }
    my $oba;
    $oba = $object->{buffs} if exists $object->{buffs};
    $oba = $object->{buff}  if ((not defined($oba)) and exists $object->{buff});
    $oba = []               if (not defined $oba);
    foreach my $ob (values $oba->@*) {
      my $nb = Game::EvonyTKR::Model::Buff->from_hash($ob);
      $b->addBuff($nb);
    }
    return $b;
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
      && $self->isa(__PACKAGE__);
  }
}
1;
__END__
