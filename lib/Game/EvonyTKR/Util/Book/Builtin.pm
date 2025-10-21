use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require Game::EvonyTKR::Model::Buff;
require Game::EvonyTKR::Model::Book;
use namespace::autoclean;

package Game::EvonyTKR::Model::Book::Builtin {
  use Mojo::Base 'Game::EvonyTKR::Util::Book', -base;
  use List::AllUtils qw( any none );
  use Carp;
  use File::FindLib 'lib';
  use Log::Any qw($log);

  our $VERSION = 'v0.30.0';
  my $debug  = 1;
  my $logger = $log;

  sub from_hash ($class, $object,) {
    my $logger = $log;
    my $bb     = Game::EvonyTKR::Model::Book::Builtin->new(
      name => $object->{name},
      text => $object->{text} // '',
    );
    my $buffCount = 0;

    my @buffs;
    if (exists $object->{buff}) {
      push @buffs, $object->{buff}->@*;
    }
    elsif (exists $object->{buffs}) {
      push @buffs, $object->{buffs}->@*;
    }
    $logger->debug(
      sprintf('Book %s has %s buffs in YAML', $object->{name}, scalar @buffs));

    foreach my $ob (@buffs) {
      my $b = Game::EvonyTKR::Model::Buff->from_hash($ob);
      $bb->addBuff($b);
    }

    $logger->debug(sprintf(
      'Finished importing book "%s" with %s buffs: %s',
      $object->{name},
      scalar @{ $bb->buffs },
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
