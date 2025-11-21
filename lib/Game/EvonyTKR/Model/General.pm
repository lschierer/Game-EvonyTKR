use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require UUID;

require Game::EvonyTKR::Model::BasicAttributes;
require Game::EvonyTKR::Model::BasicAttribute;

package Game::EvonyTKR::Model::General {
  use Mojo::Base "Game::EvonyTKR::Model::Base";
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::BuffConstants', -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::GeneralConstants',    -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::AscendingAttributes', -role;
  use JSON::PP;
  use UUID           qw(uuid5);
  use List::AllUtils qw( any none );
  use File::FindLib 'lib';
  use Carp;
  use overload
    '""'       => \&as_string,
    'eq'       => \&equality,
    'bool'     => \&_isTrue,
    "fallback" => 1;

  our $VERSION = 'v0.40.0';

  has 'id' => sub ($self) {
    my $general_type =
      ref($self->type) eq 'ARRAY' ? $self->type->[0] : $self->type;
    if (exists $self->UUID5_Generals->{$general_type}) {
      return uuid5($self->UUID5_Generals->{$general_type}, $self->name);
    }
    return $self->name;
  };

  has ['name', 'type', 'ascendingAttributes', 'builtInBookName',
    'builtInBook'] => undef;

  has ['specialtyNames', 'specialties'] => sub { [] };
  has 'ascending'                       => 0;
  has 'stars'                           => 'none';
  has 'basicAttributes' =>
    sub { return Game::EvonyTKR::Model::BasicAttributes->new() };

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
    return unless $self->ascending;

    state $ascending_helper //= do {
      my $helper = eval {
        Mojo::Base->new->with_roles(
          'Game::EvonyTKR::Role::Logger',
          'Game::EvonyTKR::Role::Common',
          'Game::EvonyTKR::Controller::Role::AscendingAttributes'
        );
      };
      if ($@) {
        $self->logger->error("Cannot create ascending attributes helper: $@");
        return;
      }
      $helper;
    };

    return unless $ascending_helper;

    my $key = lc($self->normalize($self->name));
    $key =~ s/ /_/g;

    my $aa = $ascending_helper->get_ascending_attributes($key);
    if ($aa) {
      $self->ascendingAttributes($aa);
    }
    else {
      $self->logger->warn(sprintf(
        'failed to find expected ascending attributes '
          . 'for %s. expected keys are %s',
        $self->name,
        join ', ',
        map { sprintf('"%s"', %_ // 'undef file') }
          sort $ascending_helper->list_ascending_attributes
      ));
      return;
    }
    return 1;
  }

  sub populateBuiltinBook ($self) {
    state $books_helper //= do {
      my $helper = eval {
        Mojo::Base->new->with_roles(
          'Game::EvonyTKR::Role::Logger',
          'Game::EvonyTKR::Role::Common',
          'Game::EvonyTKR::Controller::Role::Books'
        );
      };
      if ($@) {
        $self->logger->error("Cannot create books helper: $@");
        return;
      }
      $helper;
    };

    return unless $books_helper;

    my $book;

    eval { $book = $books_helper->get_builtin_book($self->builtInBookName); }
      or do {
      $self->logger->error(
        sprintf('eval failed; cannot get book from helper: %s', $@));
      my $ab = $books_helper->list_builtin_books;
      $self->logger->debug(sprintf(
        'available books: %s',
        scalar(@{$ab})
        ? join ', ',
          map { sprintf('"%s"', $_) } $ab->@*
        : 'no books available'
      ));
      return;
      };

    if (defined($book)) {
      $self->logger->debug(sprintf(
        'fetch returned book "%s" with name "%s" for builtInBookName "%s"',
        blessed($book), $book->can('name') ? $book->name : 'no name method',
        $self->builtInBookName
      ));
    }
    else {
      $self->logger->error(
        sprintf('failed to fetch book for builtin book "%s" from cache',
          $self->builtInBookName)
      );
    }
    $self->builtInBook($book);
    return $self;
  }

  sub populateSpecialties ($self,) {
    state $specialty_helper //= do {
      my $helper = eval {
        Mojo::Base->new->with_roles(
          'Game::EvonyTKR::Role::Logger',
          'Game::EvonyTKR::Role::Common',
          'Game::EvonyTKR::Controller::Role::Specialties'
        );
      };
      if ($@) {
        $self->logger->error("Cannot create specialty helper: $@");
        return;
      }
      $helper;
    };

    return unless $specialty_helper;

    foreach my $sn_index (0 .. scalar($#{ $self->specialtyNames })) {
      my $sn = $self->specialtyNames->[$sn_index];
      if (!defined($sn) || !length($sn)) {
        $self->logger->error(sprintf(
          'invalid undef specialty in general %s at index %s',
          $self->name, $sn_index
        ));
        next;
      }
      $self->logger->debug(sprintf(
        'populating speciality at index %s, name %s',
        $sn_index, defined($sn) && length($sn) ? $sn : 'undefined'
      ));
      my $specialty = $specialty_helper->get_specialty($sn);
      if ($specialty) {
        $self->specialties->[$sn_index] = $specialty;
      }
      else {
        $self->logger->error(sprintf(
          'Missing specialty at index %s for general "%s": "%s" ',
          $sn_index, $self->name, $sn
        ));
      }
    }
  }

  sub from_hash ($self, $hashObject) {
    my $logger = Game::EvonyTKR::Log::Config->logger();

    if (!exists $hashObject->{name}) {
      $self->logger->error('hash object must contain a name attribute.');
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
      $self->logger->error('Invalid Hash Object.');
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
    my $logger = Game::EvonyTKR::Log::Config->logger();
    unless (($w->{_v} // 1) == 1 ) {
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
    $general->populateBuiltinBook() unless (exists $opts->{populateBuiltinBook} && $opts->{populateBuiltinBook} == 0);
    $general->populateAscendingAttributes() unless (exists $opts->{populateAscendingAttributes} && $opts->{populateAscendingAttributes} == 0);
    $general->populateSpecialties() unless (exists $opts->{populateSpecialties} && $opts->{populateSpecialties} == 0);

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

  sub TO_JSON ($self) {
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
