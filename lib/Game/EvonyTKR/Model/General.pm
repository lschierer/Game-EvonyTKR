use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require Game::EvonyTKR::Model::BasicAttributes;
require JSON::PP;
require Mojo::JSON;
require Log::Log4perl;

class Game::EvonyTKR::Model::General : isa(Game::EvonyTKR::Shared::Constants) {
# PODNAME: Game::EvonyTKR::Model::General
  use List::AllUtils qw( any none );
  use Types::Common  qw( t is_Num is_Str);
  use UUID           qw(uuid5);
  use namespace::autoclean;
  use Carp;
  use File::FindLib 'lib';
  use overload
    '""'       => \&as_string,
    'bool'     => sub { $_[0]->_isTrue() },
    "fallback" => 1;

  our $VERSION = 'v0.30.0';
  my $debug = 1;

  field $id : reader;

  field $name : reader : param;

  field $type : reader : param;

  field $ascending : reader : param //= 0;
  field $stars     : reader : param //= 'none';

  field $basicAttributes : reader =
    Game::EvonyTKR::Model::BasicAttributes->new();

  field $builtInBookName : reader : param;

  field $builtInBook : reader : writer = undef;

  field $specialtyNames : reader : param;

  field $specialties : reader //= [];

  ADJUST {
    my @errors;
    unless ($self->can('_isTrue') && $self->_isTrue()) {
      Log::Log4perl->logcroak("unexpected value: " . blessed($self));
    }
    if (not defined $type) {
      push @errors,
        sprintf('type must be one of %s', join(', ', @{ $self->GeneralKeys }));
    }
    elsif (ref $type) {
      foreach my $t1 (@{$type}) {
        if (none { $t1 =~ /$_/i } @{ $self->GeneralKeys }) {
          push @errors,
            sprintf('type must be one of %s, not %s',
            Data::Printer::np($self->GeneralKeys()->values()), $t1);
        }
      }
    }
    elsif (none { $type =~ /$_/i } @{ $self->GeneralKeys }) {
      push @errors,
        sprintf('type must be one of %s, not "%s"',
        Data::Printer::np($self->GeneralKeys()), $type);
    }
    my @valv;
    map { push @valv, $_ } $self->AscendingAttributeLevelValues();
    map { push @valv, $_ } $self->AscendingAttributeLevelValues(0);
    if (none { $stars =~ /$_/ } @valv) {
      push @errors,
        sprintf('stars must be one of %s, not "%s"', join(',', @valv), $stars);
    }
    if (@errors) {
      $self->logger()->logcroak(join ', ' => @errors);
    }

    if (ref $type) {
      my @ts = @{$type};
      my $ut = $ts[0];
      $self->logger->debug("using type $ut");
      my $uuid5base = $self->UUID5_Generals()->{$ut};
      $id = uuid5($uuid5base, $name);
    }
    else {
      my $uuid5base = $self->UUID5_Generals()->{$type};
      $id = uuid5($uuid5base, $name);
    }

  }

  method populateBuiltInBook($bookManager) {
    my $bb = $bookManager->getBook($builtInBookName);
    if ($bb) {
      $builtInBook = $bb;
    }
  }

  method populateSpecialties ($specialtyManager) {
    foreach my $sn_index (0 .. scalar(@{$specialtyNames})) {
      my $sn = $specialtyNames->[$sn_index];
      $self->logger->debug("populating $sn");
      my $specialty = $specialtyManager->getSpecialty($sn);
      if ($specialty) {
        $specialties->[$sn_index] = $specialty;
      }
    }
  }

  method can_afford_ascending_level($requestedLevel) {
    my @valv;
    map { push @valv, $_ } $self->AscendingAttributeLevelValues();
    map { push @valv, $_ } $self->AscendingAttributeLevelValues(0);
    if (any { $requestedLevel eq $_ } @valv) {
      my %ranks = (
        none    => 0,
        purple1 => 1,
        purple2 => 2,
        purple3 => 3,
        purple4 => 4,
        purple5 => 5,
        red1    => 6,
        red2    => 7,
        red3    => 8,
        red4    => 9,
        red5    => 10
      );
      my $mr = $ranks{$stars};
      my $rr = $ranks{$requestedLevel};
      return $rr <= $mr;
    }
    return 0;
  }

  method to_hash {
    return {
      id              => $id,
      name            => $name,
      type            => $type,
      basicAttributes => $basicAttributes,
      ascending       => $ascending,
      builtInBookName => $builtInBookName,
      specialtyNames  => $specialtyNames,
    };
  }

  method TO_JSON {
    return JSON::PP->new->utf8(1)->pretty->canonical(1)
      ->allow_blessed(1)
      ->convert_blessed(1)
      ->encode($self->to_hash());
  }

  method as_string {
    my $json =
      JSON::PP->new->utf8(0)->pretty->canonical(1)
      ->allow_blessed(1)
      ->convert_blessed(1)
      ->encode($self->to_hash());
    return $json;
  }

  sub from_hash ($self, $ho, $logger = undef) {
    if (!exists $ho->{name}) {
      if (defined($logger)) {
        $logger->logcroak('hash object must contain a name attribute.');
        return undef;
      }
      else {
        croak('hash object must contain a name attribute.');
        return undef;
      }
    }
    my $o = Game::EvonyTKR::Model::General->new(
      name            => $ho->{name},
      type            => $ho->{type},
      ascending       => $ho->{ascending},
      stars           => $ho->{stars},
      builtInBookName => $ho->{book},
      specialtyNames  => $ho->{specialties},
    );

    foreach my $baKey (keys %{ $ho->{basic_attributes} }) {
      my $ba = Game::EvonyTKR::Model::BasicAttribute->new(
        attribute_name => $baKey,
        base           => $ho->{basic_attributes}->{$baKey}->{base},
        increment      => $ho->{basic_attributes}->{$baKey}->{increment},
      );
      $o->basicAttributes->setAttribute($baKey, $ba);
    }

    return $o;
  }

}
1;

__END__
#ABSTRACT: how to store a General in memory

=pod

=head1 DESCRIPTION

I am not doing true MVC because I am using yaml files as the persistence layer instead of a database.  This class stores a General in memory so that the Controller need not read
in and parse the YAML every time.

=cut

=method new($name)

Create an instance of a Model::General with name $name.

=method name()

returns the general's name.

=cut

=method type()

returns the general's type, which must be one of the values from Game::EvonyTKR::Model::Data->GeneralKeys()

=cut
1;

__END__
#ABSTRACT: how to store a General in memory

=pod

=head1 DESCRIPTION

I am not doing true MVC because I am using yaml files as the persistence layer instead of a database.  This class stores a General in memory so that the Controller need not read
in and parse the YAML every time.

=cut

=method new($name)

Create an instance of a Model::General with name $name.

=method name()

returns the general's name.

=cut

=method type()

returns the general's type, which must be one of the values from Game::EvonyTKR::Model::Data->GeneralKeys()

=cut
