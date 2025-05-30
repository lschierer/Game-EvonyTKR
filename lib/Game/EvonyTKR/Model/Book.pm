use v5.40.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require Game::EvonyTKR::Model::Buff;
require Game::EvonyTKR::Model::Buff::Value;
use namespace::clean;

class Game::EvonyTKR::Model::Book : isa(Game::EvonyTKR::Model::Data) {
  # PODNAME: Game::EvonyTKR::Model::Book
  use Carp;
  use overload
    '""'       => \&TO_JSON,
    'bool'     => \&_isTrue,
    'fallback' => 0;

  field $name : reader : param;
  field $buff : reader;
  field $text : reader : param //= '';

  method addBuff ($newBuff) {
    if (Scalar::Util::reftype($newBuff) eq 'OBJECT') {
      my $classList  = blessed $newBuff;
      my @classStack = split(/::/, Scalar::Util::reftype($newBuff));
      if (scalar @classStack >= 3) {
        if ($classStack[2] eq 'Buff') {
          $self->logger->info("adding $newBuff to $name");
          push @{$buff}, $newBuff;
        }
      }
    }
  }

  method toHashRef {
    return {
      name => $name,
      text => $text,
      buff => $buff,
    };
  }

  method TO_JSON {
    return $self->toHashRef();
  }

  method _isTrue {
    return
         defined($self)
      && ref($self)
      && blessed($self)
      && $self->isa('Game::EvonyTKR::Model::Book');
  }

  method validate() {
    my @errors;
    if (scalar @{$buff}) {
      for my $b (@{$buff}) {
        my $bc  = blessed $b;
        my @bcl = split(/::/, $bc);
        if (not($bcl[1] eq 'EvonyTKR' and $bcl[2] eq 'Buff')) {
          push @errors,
            sprintf('$buff must contain type Game::EvonyTKR::Model::Buff not %s', $bc);
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

}
1;

__END__

#ABSTRACT: base class for builtin BuiltIn and Standard Skill Books.

=pod

=head1 DESCRIPTION

Books are one of the fundamental ways in which the game adds Buffs and Debuffs to Generals.

=cut

=cut
