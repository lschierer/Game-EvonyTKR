use v5.40.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require Game::EvonyTKR::BasicAttributes;
require JSON::PP;

class Game::EvonyTKR::General : isa(Game::EvonyTKR::Data) {
# PODNAME: Game::EvonyTKR::Model::General
  use List::AllUtils qw( any none );
  use Types::Common  qw( t is_Num is_Str);
  use UUID           qw(uuid5);
  use namespace::autoclean;
  use Carp;
  use File::FindLib 'lib';
  use overload
    '""'       => \&TO_JSON,
    "fallback" => 1;

  our $VERSION = 'v0.30.0';
  my $debug = 1;

  field $id : reader;

  field $name : reader : param;

  field $type : reader : param;

  field $ascending : reader : param //= false;

  field $basicAttributes : reader = Game::EvonyTKR::BasicAttributes->new();

  field $builtInBookName : reader : param;

  field $builtInBook : reader;

  field $specialityNames : reader : param;

  field $specialities : reader //= [];

  ADJUST {
    my @errors;

    my $re = $self->GeneralKeys->as_regexp();
    if (ref $type) {
      foreach my $t1 (@{$type}) {
        if ($t1 !~ /$re/i) {
          push @errors,
            sprintf('type must be one of %s, not %s',
            Data::Printer::np($self->GeneralKeys()->values()), $t1);
        }
      }
    }
    elsif ($type !~ /$re/i) {
      push @errors,
        sprintf('type must be one of %s, not %s',
        Data::Printer::np($self->GeneralKeys()->values()), $type);
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

  method toHashRef {
    return {
      id              => $id,
      name            => $name,
      type            => $type,
      basicAttributes => $basicAttributes,
      ascending       => $ascending,
      builtInBookName => $builtInBookName,
      specialityNames => $specialityNames,
    };
  }

  method TO_JSON {
    return $self->toHashRef();
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

returns the general's type, which must be one of the values from Game::EvonyTKR::Data->GeneralKeys()

=cut
