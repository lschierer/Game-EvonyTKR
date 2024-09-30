use v5.40.0;
use experimental qw(class);
use File::FindLib 'lib';

class Game::EvonyTKR::SkillBook : isa(Game::EvonyTKR::Logger) {
# PODNAME: Game::EvonyTKR::SkillBook
# VERSION
  use builtin         qw(indexed);
  use Types::Standard qw(is_Int Int Num is_Num Str is_Str);
  use Types::Common   qw( t);
  use Type::Utils "is";
  use Carp;
  use Data::Dumper;
  use Data::Printer;
  use File::ShareDir ':ALL';
  use File::Spec;
  use Game::EvonyTKR::Buff;
  use Game::EvonyTKR::Buff::Value;
  use List::MoreUtils;
  use JSON::MaybeXS;
  use Util::Any -all;
  use YAML::XS qw{LoadFile Load};
  use namespace::autoclean;
  use overload
    '""'       => \&_toString,
    "fallback" => 1;

# from Type::Registry, this will save me from some of the struggles I have had with some types having blessed references and others not.
  ADJUST {
    if (!(t->simple_lookup("Num"))) {
      t->add_types(-Common);
    }
  }

  field $name : reader : param;

  ADJUST {
    my @errors;
    is_Str($name) or push @errors => "name must be a string, not $name";
    if (@errors) {
      die join ', ' => @errors;
    }
  }

  field $text : reader;

  field @buffs : reader;

  method getEvAnsScore($name, $resultRef, $BuffMultipliers, $GeneralBias) {
    for my ($i, $b) (indexed(@buffs)) {
      my $result = $b->getEvAnsScore($name, $BuffMultipliers, $GeneralBias,);
      $self->logger()->debug("recieved $result from getEvAnsScore for buff $i");
      my $category = $BuffMultipliers->EvAnsCategory($b);
      if (not defined $category) {
        $self->logger()->warn("no category returned for " . np $b);
        $category = 'Unused';
      }
      $resultRef->{'SBS'}->{$category} += $result;
      $resultRef->{$category}->{'SBS'} += $result;
    }
  }

  method set_text($nt = '') {
    is_Str($nt) or $self->logger()->logcroak("text must be a string, not $nt");

    $text = $nt;
  }

  method add_buff($nb) {
    if (blessed $nb ne 'Game::EvonyTKR::Buff') {
      $self->logger()
        ->warn(
        "refusing to add something that is not a 'Game::EvonyTKR::Buff' to $name"
        );
      return 0;
    }
    elsif (scalar @buffs >= 1) {
      my $found_match = 0;
      foreach (@buffs) {
        if ($_ == $nb) {
          $self->logger()
            ->warn("refusing to add duplicate buff to skillbook $name");
          return 0;
        }
      }
    }
    push @buffs, $nb;
  }

  method toHashRef($verbose = 0) {
    $self->logger()->trace("starting toHashRef for $name");
    my $returnRef = {};
    $returnRef->{'name'} = $name;
    $returnRef->{'text'} = $text;
    if ($verbose) {
      for my $tb (@buffs) {
        push @{ $returnRef->{'buffs'} }, $tb->toHashRef();
      }
    }
    return $returnRef;
  }

  method _toString {
    my $json = JSON::MaybeXS->new(utf8 => 1);
    return $json->encode($self->toHashRef());
  }

}

1;

__END__

# ABSTRACT: Module for processing information about Evony TKR SkillBooks.

=head1 DESCRIPTION

=for SkillBooks one of several ways that a General can provide Buffs for Troops.

This is the base class, providing common methods for all SkillBooks.  You should probably be using either the SkillBook::Standard or the SkilllBook::Special subclass instead.

=cut

=method name

each SkillBook has a name
=cut

=method text

returns the human text that the game actually provides, if recorded here.
=cut

=method buffs

each SkillBook has one or more Game::EvonyTKR:Buffs
=cut

=method add_buff($nb)

This method takes a Game::EvonyTKR::Buff as its sole parameter and adds it as one of the buffs this SkillBook provides.

One of the interesting properties of the buffs being an optional field in the class is that when seeking to record conflicting books, I do not care what buffs the books provide, simply enough to I<identify> them.  This facilitates that.
=cut

=method getEvAnsScore($name, $resultRef, $BuffMultipliers, $GeneralBias) 

$name is simply a string used to help identify records in logs.

$resultRef is a fairly complicated HashRef of HashRefs. 
This method will write to $resultRef->{'SBS'}

the key for that hash comes from L<https://www.evonyanswers.com/post/evony-answers-attribute-methodology-explanation>
where Game::EvonyTKR::SkillBook scores end up in what EvAns calls the BSS, 4SB and the SKS categories.  I've combined them in my implementation. 

$BuffMultipliers must contain an instance of a I<child class> of Game::EvonyTKR::Buff::EvaluationData

$GeneralBias must contain a valid instane of a class from Game::EvonyTKR::Buff::Data

There is no return value, the result is writen directly into the resultref parameter as noted above.
=cut
