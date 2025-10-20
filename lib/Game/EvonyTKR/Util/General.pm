use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require Game::EvonyTKR::Model::BasicAttributes;
require JSON::PP;
require Mojo::JSON;
require Game::EvonyTKR::Shared::Constants;

package Game::EvonyTKR::Util::General {
  use Mojo::Base 'Game::EvonyTKR::Util::Common', -role, -signatures;
  use Mojo::Base 'Game::EvonyTKR::Shared::Constants::BuffConstants', -role;
  use Mojo::Base 'Game::EvonyTKR::Shared::Constants::AscendingAttributes',
    -role;
  use List::AllUtils qw( any none );
  use Types::Common  qw( t is_Num is_Str);
  use UUID           qw(uuid5);
  use Log::Any       qw($log);
  use namespace::autoclean;
  use Carp;
  use File::FindLib 'lib';

  our $VERSION = 'v0.40.0';
  my $debug = 1;

  my $logger = Game::EvonyTKR::Log::Config->logger();

  sub validate($self) {
    my @errors;
    unless (exists($self->type)) {
      push @errors,
        sprintf('type must be one of %s', join(', ', @{ $self->GeneralKeys }));
    }
    if (ref($self->type)) {
      foreach my $t1 ($self->type->@*) {
        if (none { $t1 =~ /$_/i } @{ $self->GeneralKeys }) {
          push @errors,
            sprintf('type must be one of %s, not %s',
            join(', ', @{ $self->GeneralKeys }), $t1);
        }
      }
    }
    elsif (none { $self->type =~ /$_/i } @{ $self->GeneralKeys }) {
      push @errors,
        sprintf('type must be one of %s, not "%s"',
        join(', ', @{ $self->GeneralKeys }), $type);
    }

    my @valv;
    map { push @valv, $_ } $self->AscendingAttributeLevelValues();
    map { push @valv, $_ } $self->AscendingAttributeLevelValues(0);
    if (none { $general->stars =~ /$_/ } @valv) {
      push @errors,
        sprintf(
        'stars must be one of %s, not "%s"',
        join(',', @valv),
        $general->stars
        );
    }

    if (@errors) {
      $self->logger->logcroak(join ', ', @errors);
      return;
    }
    return 1;
  }

  sub set_general_id($self) {
    if (ref $self->type) {
      my @ts;
      push @ts, $self->type->@*;
      my $ut = $ts[0];
      $logger->debug("using type $ut");
      my $uuid5base = $self->UUID5_Generals->{$ut};
      $general->{id} = uuid5($uuid5base, $name);
    }
    else {
      my $uuid5base = $self->UUID5_Generals->{ $self->type };
      $id = uuid5($uuid5base, $self->normalize($self->name));
    }
  }

  sub populateSpecialties ($self, $allSpecialties) {
    my @specialtyNames;
    push @specialtyNames, $self->specialtyNames->@*;
    foreach my $sn_index (0 .. scalar(@specialtyNames)) {
      my $sn = $specialtyNames[$sn_index];
      $logger->debug("populating $sn");
      my $specialty = $allSpecialties->{$sn};
      if ($specialty) {
        $specialties->[$sn_index] = $specialty;
      }
      else {
        $logger->error(sprintf(
          'Missing specialty number %s for general "%s": "%s" ',
          $sn_index, $general->{name}, $sn
        ));
      }
    }
  }

  sub can_afford_ascending_level($self, $requestedLevel) {
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
      my $mr = $ranks{ $self->stars };
      my $rr = $ranks{$requestedLevel};
      return $rr <= $mr;
    }
    return 0;
  }

  sub from_hash ($self, $hashObject) {
    my $logger = Game::EvonyTKR::Log::Config->logger();

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
