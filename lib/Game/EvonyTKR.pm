package Game::EvonyTKR;
use v5.40.0;
use Carp;

use base qw(App::Cmd::Simple);
use File::ShareDir ':ALL';
use File::Spec;
use YAML::XS;
use Data::Dumper;
use Game::EvonyTKR::General;
use Game::EvonyTKR::SkillBook::Special;
use namespace::autoclean;

# ABSTRACT: Perl Modules providing utilities for players of Evony The King's Return.

=head1 DESCRIPTION

Serious players of the game will find themselves needing to do reasonably complex analysis to make optimal choices about

=over

=item * Which generals to invest time and resources in

=item * How to best pair generals for different senarios

=item * The effects of Armor, Spiritual Beats, and Dragons on buffs and debuffs

=item * Calculating your total buffs and debuffs in different senarios given the
ever increasing number of sources for these buffs and debuffs

=back

This distribution aims to help players create and process this wealth of data. 
=cut

sub opt_spec {
  return (
    [ "option1|a",  "do option 1" ],
  );
}

sub validate_args {
  my ($self, $opt, $args) = @_;

  # no args allowed but options
  $self->usage_error("No args allowed") if @$args;
}

sub execute {
  my ($self, $opt, $args) = @_;

  if ($opt->{option1}) {
      # do option 1 stuff
  } else {
    my %generals = read_generals();
    say "start";
    say Dumper(%generals);
    say "done";
  }
}

sub read_generals {
  my ($self, $opt, $args) = @_;
  my $debug = 0;
  if($debug) { say "read_generals";}
  my $general_share = File::Spec->catfile(dist_dir('Game-EvonyTKR', 'generals'),"generals");
  my @found = grep { -T -s -r } glob("$general_share/*.yaml");
  if($debug) {
    say "$general_share";
    say scalar @found;
  }
  my %generals;
  foreach my $tg (@found) {
    if(defined($tg)) {
      open(my ($fh), '<', $tg) or croak "$!";
      my $yaml = do { local $/; <$fh> };
      my $data = Load $yaml;
      close $fh;
      my $name = $data->{'general'}->{'name'};
      if($debug) {say Dumper($name);}
      $generals{$name} = Game::EvonyTKR::General->new(
        name                  => $data->{'general'}->{'name'},
        leadership            => $data->{'general'}->{'leadership'},
        leadership_increment  => $data->{'general'}->{'leadership_increment'},
        attack                => $data->{'general'}->{'attack'},
        attack_increment      => $data->{'general'}->{'attack_increment'},
        defense               => $data->{'general'}->{'defense'},
        defense_increment     => $data->{'general'}->{'defense_increment'},
        politics              => $data->{'general'}->{'politics'},
        politics_increment    => $data->{'general'}->{'politics_increment'},
      );
    }
  }
  return %generals;
}

1;
