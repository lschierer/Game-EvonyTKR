use v5.40.0;
use experimental qw(class);
use FindBin;
use lib "$FindBin::Bin/../../../../lib";

class Game::EvonyTKR::SkillBook::Special : isa(Game::EvonyTKR::SkillBook) {
# PODNAME: Game::EvonyTKR::SkillBook::Special
  use Data::Dumper;
  use Data::Printer;
  use File::ShareDir ':ALL';
  use File::Spec;
  use Types::Standard qw(is_Int Int Num is_Num Str is_Str);
  use Types::Common   qw( t);
  use Type::Utils "is";
  use List::MoreUtils;
  use Util::Any -all;
  use YAML::XS qw{LoadFile Load};
  use Game::EvonyTKR::Buff;
  use Carp;
  use namespace::autoclean;

# from Type::Registry, this will save me from some of the struggles I have had with some types having blessed references and others not.
  ADJUST {
    if (!(t->simple_lookup("Num"))) {
      t->add_types(-Common);
    }
  }

  field $builtIn : reader : param //= 1;

  method is_built_in() {
    return 1;
  }

  method toHashRef($verbose = 0) {
    my $name = $self->name();
    $self->logger()->trace("starting toHashRef for $name");
    my $returnRef = {};
    $returnRef->{'name'} = $name;
    $returnRef->{'text'} = $self->text();
    if($verbose) {  
      for my $tb ($self->buffs()) {
        push @{ $returnRef->{'buffs'} }, $tb->toHashRef();
      } 
    }
    return $returnRef;
  }

  method readFromFile() {
    my $name = $self->name();
    my $SkillBookFileName = $name . '.yaml';
    my $SkillBookShare =
      File::Spec->catfile(dist_dir('Game-EvonyTKR'), 'skillbooks');
    my $FileWithPath = File::Spec->catfile($SkillBookShare, $SkillBookFileName);
    if (-T -s -r $FileWithPath) {
      $self->logger()->debug("$SkillBookFileName exists as expected");
      my $data      = LoadFile($FileWithPath);

      if(exists $data->{'text'}) {
        $self->set_text($data->{'text'});
      }
      else {
        $self->set_text('Original Text Not Available.');
      }

      my @dataBuffs = @{ $data->{'buff'} };
      $self->logger()
        ->debug("$name has " . scalar @dataBuffs . " buffs in the file");
      for my $sbb (@dataBuffs) {
        my $v;
        my $b;
        my @sbKeys = keys %{$sbb};

        if (any { $_ eq 'value' } @sbKeys) {
          $self->logger()->debug("SkillBook $name has a buff with a value");
          $v = Game::EvonyTKR::Buff::Value->new(
            number => $sbb->{'value'}->{'number'},
            unit   => $sbb->{'value'}->{'unit'},
          );
          if (any { $_ eq 'class' } @sbKeys) {
            $b = Game::EvonyTKR::Buff->new(
              attribute => $sbb->{'attribute'},
              value     => $v,
              buffClass => $sbb->{'class'},
            );
          }
          else {
            $b = Game::EvonyTKR::Buff->new(
              attribute => $sbb->{'attribute'},
              value     => $v,
            );
          }
        }
        else {
          $self->logger()->warn("SkillBook $name has a buff with no value.");
        }
        if (defined $b) {
          if (any { $_ eq 'condition' } @sbKeys) {
            my @conditions = @{ $sbb->{condition} };
            foreach my $sbc (@conditions) {
              $b->set_condition($sbc);
            }
          }
          $self->logger()->info("from SkillBook $name; Adding buff " . np $b);
          $self->add_buff($b);
        }
        else {
          $self->logger()->warn("No buff defined in readFromFile for $name");
        }
      }
      $self->logger()
        ->debug("$name has " . scalar $self->buffs() . " buffs after reading all in");
    }
  }

}

1;
__END__


# ABSTRACT: Module for processing information about Evony TKR SkillBooks Built-in to each General.

=head1 DESCRIPTION

=for SkillBooks one of several ways that a General can provide Buffs for Troops.

The SkillBook::Special class is for the built-in SkillBooks are intrinsic to a particular General.

While there is no particular logic here, by requiring this class, I ensure that a General is not accidentally created with a Standard SkillBook.

=cut

=attr $builtIn

Most of the time when dealing with SkillBook::Special, we are dealing with the general's built in skill book.  Occasionally we are dealing with a removable skill set that the game displays as a "skin" or alternative outfit/appearance for the general.  These alternative apperances come with buffs that behave functionally equivalent to an additional SkillBook.
=cut

=method is_builtin()

an assertion that tells us if this is the built in skill or an extra this general has from an add-on.
=cut

