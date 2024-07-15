use 5.38.0;
use strict;
use warnings;
package Game::EvonyTKR;
use base qw(App::Cmd::Simple);

=head1 NAME

Game::EvonyTKR - Perl Modules providing utilities for players of Evony The King's Return.

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
      # do regular stuff
  }
}

1;

