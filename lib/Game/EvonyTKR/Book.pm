use v5.40.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require Game::EvonyTKR::Model::Buff::Value;

class Game::EvonyTKR::Model::Book : isa(Game::EvonyTKR::Data) {
# PODNAME: Game::EvonyTKR::Model::Book
  use List::AllUtils qw( any none );
  use namespace::autoclean;
  use Carp;
  use File::FindLib 'lib';
  use overload
    '""' => \&to_String;

  our $VERSION = 'v0.30.0';
  my $debug = 1;

  field $name : reader : param;

  field $text : reader : param //= "Original text not available.";

  field $buff : reader : param //= [];

  method validate() {
    my @errors;
    if (scalar @{$buff}) {
      for my $b (@{$buff}) {
        my $bc  = blessed $b;
        my @bcl = split(/::/, $bc);
        if (not($bcl[2] eq 'Model' and $bcl[3] eq 'Buff')) {
          push @errors,
            sprintf(
            '$buff must contain type Game::EvonyTKR::Model::Buff not %s',
            $bc);
        }
      }
    }
    my $type = t('Str');
    $type->check($name)
      or push @errors => sprintf('$name must contain a string, not %s', $name);
    $type->check($text)
      or push @errors => sprintf('$text must contain a string, not %s', $text);
    if (@errors) {
      $self->logger()->logcroak(join(', ' => @errors));
    }
  }

  ADJUST {
    $self->validate();
  }

  method TO_JSON($verbose = false) {
    my $returnable = {
      name => $name,
      text => $text,
    };
    if ($verbose) {
      if (scalar @{$buff}) {
        for my $b (@{$buff}) {
          push @{ $returnable->{'buff'} }, $b->TO_JSON();
        }
      }
    }
    return $returnable;
  }

  method to_String($verbose = false) {
    $self->TO_JSON($verbose);
  }

}
1;

__END__

#ABSTRACT: base class for builtin BuiltIn and Standard Skill Books.

=pod

=head1 DESCRIPTION

Books are one of the fundamental ways in which the game adds Buffs and Debuffs to Generals.

=cut

=cut
