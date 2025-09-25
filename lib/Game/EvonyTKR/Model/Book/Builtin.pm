use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require Game::EvonyTKR::Model::Buff;
require Game::EvonyTKR::Model::Book;

class Game::EvonyTKR::Model::Book::Builtin : isa(Game::EvonyTKR::Model::Book) {
# PODNAME: Game::EvonyTKR::Model::Book::Builtin
  use List::AllUtils qw( any none );
  use namespace::autoclean;
  use Carp;
  use File::FindLib 'lib';
  use overload
    '""'       => \&TO_JSON,
    'fallback' => 0;

  our $VERSION = 'v0.30.0';
  my $debug = 1;

  method toHashRef {
    return {
      name => $self->name,
      text => $self->text,
      buff => $self->buff,
    };
  }

  method TO_JSON {
    return $self->toHashRef();
  }

  sub from_hash ($self, $object, $logger = undef) {
    unless (defined($logger)) {
      $logger = Log::Log4perl->get_logger(Scalar::Util::blessed($self));
    }
    my $bb = Game::EvonyTKR::Model::Book::Builtin->new(
      name => $object->{name},
      text => $object->{text} // '',
    );
    my $buffCount = 0;

    my @buffs;
    if (exists $object->{buff}) {
      @buffs = @{ $object->{buff} };
    }
    elsif (exists $object->{buffs}) {
      @buffs = @{ $object->{buffs} };
    }
    $logger->debug(
      sprintf('Book %s has %s buffs in YAML', $object->{name}, scalar @buffs));

    foreach my $ob (@buffs) {
      my $b = Game::EvonyTKR::Model::Buff->from_hash($ob, $logger);
      $bb->addBuff($b);
    }

    $logger->debug(sprintf(
      'Finished importing book "%s" with %s buffs: %s',
      $object->{name},
      scalar @{ $bb->buff },
      Data::Printer::np($bb, multiline => 0)
    ));
    return $bb;
  }

}
1;

__END__

#ABSTRACT: Model of the Books that come built-in with Generals

=pod

=head1 DESCRIPTION

Generals have books built in.  This describes/models the books.

=cut

=cut
