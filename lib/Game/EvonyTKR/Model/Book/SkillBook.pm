use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require Game::EvonyTKR::Model::Buff;

class Game::EvonyTKR::Model::Book::SkillBook : isa(Game::EvonyTKR::Model::Book)
{
# PODNAME: Game::EvonyTKR::Model::Book::SkillBook
  use List::AllUtils qw( any none );
  use namespace::autoclean;
  use Carp;
  use File::FindLib 'lib';
  use overload
    '""'       => \&TO_JSON,
    'fallback' => 0;

  our $VERSION = 'v0.30.0';
  my $debug = 1;

  field $level : reader : param;

  ADJUST {
    # Validate level is between 1 and 5
    if ($level < 1 || $level > 5) {
      $self->logger->ERR(
        sprintf('Skillbook level must be between 1 and 5, got %s', $level));
      croak(sprintf('Skillbook level must be between 1 and 5, got %s', $level));
      return;
    }
  }

  method to_hash {
    my $hashRef = $self->SUPER::to_hash;
    $hashRef->{level} = $level;
    return $hashRef;
  }

  method TO_JSON {
    return $self->to_hash();
  }

  sub from_hash ($self, $object, $logger = undef) {
    unless (defined($logger)) {
      $logger = Game::EvonyTKR::Shared::Logger::get_logger(__PACKAGE__);
    }
    my ($level) = $object->{name} =~ /Level (\d+)/;
    my $bb = Game::EvonyTKR::Model::Book::SkillBook->new(
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
    $logger->DEBUG(
      sprintf('Book %s has %s buffs in YAML', $object->{name}, scalar @buffs));

    foreach my $ob (@buffs) {
      my $b = Game::EvonyTKR::Model::Buff->from_hash($ob, $logger);
      $bb->addBuff($b);
    }

    $logger->DEBUG(sprintf(
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

#ABSTRACT: Model for non-builtin Skill Books that can be equipped to Generals

=pod

=head1 DESCRIPTION

SkillBooks are books that can be equipped to Generals. Unlike Builtins,
these have levels and can be upgraded.

=cut

=cut
