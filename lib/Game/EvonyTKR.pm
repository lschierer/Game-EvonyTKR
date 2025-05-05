use v5.40.0;
use feature 'try';
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Path::Tiny;

use YAML::PP;
require File::Share;
require Log::Log4perl;
use namespace::clean;

package Game::EvonyTKR {
  use parent qw(App::Cmd::Simple);
  use namespace::autoclean;
  use Carp;
  use Path::Tiny;
  use Game::EvonyTKR::Logger::Config;
  use File::FindLib 'lib';
  our $VERSION = 'v0.40.0';

  my $cg;
  my $ascension;
  my $generals;
  my $skillBooks;
  my $specialities;

  sub opt_spec {
    return (
      ["output|o=s",    "output directory", { required => 1 }],
      ["conflicts|c=s", "conflicts file",   { required => 1 }],
      [
        "input|i=s", "input directory containing collections", { required => 1 }
      ],
      ["mode|m=s", "mode=production|development", { required => 0 }],
    );
  }

  sub validate_args {
    my ($self, $opt, $args) = @_;
    # no args allowed but options!
    $self->usage_error("No args allowed") if @$args;
  }

  sub execute {
    my ($self, $opt, $args) = @_;

    my $distDir = Path::Tiny::path(File::Share::dist_dir('Game-EvonyTKR'));

    my $logConfig;
    if (exists $opt->{mode} && $opt->{mode} eq 'production') {
      $logConfig = $distDir->child('log4perl.conf');
    }
    else {
      $logConfig = $distDir->child('log4perl.development.conf');
    }

    Log::Log4perl::init_and_watch($logConfig->stringify());
    my $logger = Log::Log4perl->get_logger('Game-EvonyTKR');

    $logger->info("startup");

  }

}

1;

__END__

#ABSTRACT: The main Mojolicious configuration, command, and control module

=pod

=head1 DESCRIPTION

this module contains the primary Mojolicious command, control and configuration.

=cut
