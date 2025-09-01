use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require Game::EvonyTKR::Model::BasicAttributes;
require JSON::PP;
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
    return $self->to_hash();
  }

  method as_string {
    my $json =
      JSON::PP->new->utf8->pretty->canonical(1)
      ->allow_blessed(1)
      ->convert_blessed(1)
      ->encode($self->to_hash());
    return $json;
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
