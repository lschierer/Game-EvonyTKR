use v5.40.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require Game::EvonyTKR::Model::BasicAttributes;

class Game::EvonyTKR::Model::General : isa(Game::EvonyTKR::Data) {
# PODNAME: Game::EvonyTKR::Model::General
  use List::AllUtils qw( any none );
  use UUID           qw(uuid5);
  use Mojo::JSON     qw (encode_json);
  use namespace::autoclean;
  use Carp;
  use File::FindLib 'lib';
  use overload
    '""' => \&to_String;

  our $VERSION = 'v0.30.0';
  my $debug = 1;

  field $id : reader;

  field $name : reader : param;

  field $type : reader : param;

  field $ascending : reader : param //= false;

  field $basicAttributes : reader =
    Game::EvonyTKR::Model::BasicAttributes->new();

  field $builtInBookName : reader : param;

  field $builtInBook : reader;

  field $specialityNames : reader : param;

  field $specialities : reader //= [];

  ADJUST {
    my @errors;

    if (none { $_ =~ /$type/i } $self->GeneralKeys()) {
      my @gk       = $self->GeneralKeys();
      my $possible = Data::Printer::np(@gk);
      push @errors, sprintf('type must be one of %s, not %s', $possible, $type);
    }
    if (@errors) {
      $self->logger()->logcroak(join ', ' => @errors);
    }

    my $uuid5base = $self->UUID5_Generals()->{$type};
    $id = uuid5($uuid5base, $name);
  }

  method TO_JSON() {
    $self->logger()->trace(sprintf('Model::General->TO_JSON'));
    my $bb = defined $builtInBook  ? $builtInBook  : $builtInBookName;
    my $s  = scalar @$specialities ? $specialities : $specialityNames;
    return {
      id              => $self->id(),
      name            => $self->name(),
      type            => $self->type(),
      ascending       => $self->ascending(),
      basicAttributes => $self->basicAttributes(),
      builtInBook     => $bb,
      specialities    => $s,
    };
  }

  method to_String($meth = "toString", $a = undef, $b = undef) {
    $self->logger()->trace(sprintf('Model::General->to_String started'));
    my $returnable;
    if (defined $b) {
      $returnable = "[$meth $a $b]";
    }
    else {
      if (defined $a) {
        if (blessed $a) {
          my $aClass     = blessed $a;
          my @aClassList = split(/::/, $aClass);
          if ($aClassList[2] eq 'Model' and $aClassList[3] eq 'General') {
            $returnable = a->TO_JSON();
          }
          else {
            if (defined $meth) {
              $returnable = "blessed [$meth $a ]";
            }
            else {
              $returnable = "blessed $a";
            }
          }
        }
        else {
          if (defined $meth) {
            $returnable = "unblessed [$meth $a ]";
          }
          else {
            $returnable = $self->TO_JSON();
          }
        }
      }
      else {
        $returnable = $self->TO_JSON();
      }
    }
    $self->logger()
      ->trace(sprintf('Model::General->to_String returning %s', $returnable));
    return $returnable;
  }

  method _data_printer ($ddp) {
    return $self->TO_JSON();
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
