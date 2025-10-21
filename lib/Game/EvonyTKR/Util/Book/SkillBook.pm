use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require Game::EvonyTKR::Model::Buff;
use namespace::autoclean;

package Game::EvonyTKR::Model::Book::SkillBook {
  use Mojo::Base 'Game::EvonyTKR::Model::Book', -base;
  use List::AllUtils qw( any none );
  use Carp;
  use File::FindLib 'lib';
  use Log::Any qw($log);

  our $VERSION = 'v0.30.0';
  my $debug  = 1;
  my $logger = $log;

  sub validate ($self) {
    # Validate level is between 1 and 5
    if ($self->level < 1 || $self->level > 5) {
      $logger->logcroak(
        sprintf(
          'Skillbook level must be between 1 and 5, got %s', $self->level
        )
      );
      return;
    }
  }

  sub from_hash ($class, $object,) {
    my $logger  = $log;
    my ($level) = $object->{name} =~ /Level (\d+)/;
    my $bb      = Game::EvonyTKR::Model::Book::SkillBook->new(
      name  => $object->{name},
      level => $level,
      text  => $object->{text} // '',
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
