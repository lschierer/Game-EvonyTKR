use v5.40.0;
use experimental qw(class);

class Game::EvonyTKR::SkillBook :isa(Game::EvonyTKR::Logger) {
# PODNAME: Game::EvonyTKR::SkillBook

  use Types::Standard qw(is_Int Int Num is_Num Str is_Str);
  use Types::Common qw( t);
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
    '""'        => \&_toString,
    "fallback"  => 1;

  # from Type::Registry, this will save me from some of the struggles I have had with some types having blessed references and others not.
  ADJUST {
    if(!(t->simple_lookup("Num"))) {
      t->add_types(
      -Common
      );
    }
  }

 field $name :reader :param;

  ADJUST {
    my @errors;
    is_Str($name) or push @errors => "name must be a string, not $name";
    if (@errors) {
      die join ', ' => @errors;
    }
  }

  field @buffs :reader;

  method add_buff($nb) {
    if(blessed $nb ne 'Game::EvonyTKR::Buff'){
      return 0;
    } elsif (scalar @buffs >= 1){
      my $found_match = 0;
      foreach(@buffs) {
        if(not $nb->compare($_)) {
          $found_match = 1;
        }
      }
      if($found_match) {
        return 0;
      }
    }
    push @buffs, $nb;
  }

  method toHashRef() {
    $self->logger()->trace("starting toHashRef for $name");
    my $returnRef = {};
    $returnRef->{'name'} = $name;
    for my $tb (@buffs) {
      push @{ $returnRef->{'buffs'} }, $tb->toHashRef();
    }
    return $returnRef;
  }

  method _toString {
    my $json = JSON::MaybeXS->new(utf8 => 1);
    return $json->encode($self->toHashRef());
  }

  method readFromFile() {
    my $SkillBookFileName = $name . '.yaml';
    my $SkillBookShare = File::Spec->catfile(dist_dir('Game-EvonyTKR'), 'skillbooks');
    my $FileWithPath = File::Spec->catfile($SkillBookShare, $SkillBookFileName);
    if( -T -s -r $FileWithPath ) {
      $self->logger()->debug("$SkillBookFileName exists as expected");
      my $data = LoadFile($FileWithPath);
      my @dataBuffs = @{ $data->{'buff'}};
      $self->logger()->debug("$name has " . scalar @buffs . " buffs");
      for my $sbb (@dataBuffs) {
        my $v;
        my $b;
        my @sbKeys = keys %{$sbb};
        
        if(any {$_ eq 'value'} @sbKeys) {
          $self->logger()->debug("SkillBook $name has a buff with a value");
          $v = Game::EvonyTKR::Buff::Value->new(
              number  => $sbb->{'value'}->{'number'},
              unit    => $sbb->{'value'}->{'unit'},
            );
          if(any {$_ eq 'class'} @sbKeys) {
            $b = Game::EvonyTKR::Buff->new(
              attribute     => $sbb->{'attribute'},
              value        => $v,
              buffClass    => $sbb->{'class'},
            );
          } else {
            $b = Game::EvonyTKR::Buff->new(
              attribute     => $sbb->{'attribute'},
              value        => $v,
            );
          }
        } else {
          $self->logger()->warn("SkillBook $name has a buff with no value.");
        }
        if(defined $b) {
          if(any {$_ eq 'condition'} @sbKeys) {
            my @conditions = @{ $sbb->{condition}};
            foreach my $sbc (@conditions) {
              $b->set_condition($sbc);
            }
          }
          $self->logger()->info("Adding buff from SkillBook $name");
          push @buffs, $b;
        } else {
          $self->logger()->warn("No buff defined in readFromFile for $name");
        }
      }
    }
  }
}

1;

__END__

# ABSTRACT: Module for processing information about Evony TKR SkillBooks.

=head1 DESCRIPTION

=for SkillBooks one of several ways that a General can provide Buffs for Troops.

This is the base class, providing common methods for all SkillBooks.  You should probably be using either the SkillBook::Standard or the SkilllBook::Special subclass instead.

=cut

=attr name

each SkillBook has a name
=cut

=attr buffs

each SkillBook has one or more Game::EvonyTKR:Buffs
=cut

=method add_buff($nb)

This method takes a Game::EvonyTKR::Buff as its sole parameter and adds it as one of the buffs this SkillBook provides.

One of the interesting properties of the buffs being an optional field in the class is that when seeking to record conflicting books, I do not care what buffs the books provide, simply enough to I<identify> them.  This facilitates that.
=cut

