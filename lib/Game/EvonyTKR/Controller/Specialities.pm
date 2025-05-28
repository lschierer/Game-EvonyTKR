use v5.40.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
use YAML::PP;
require Mojolicious::Plugin::DefaultHelpers;
require Path::Tiny;
require Game::EvonyTKR::Role::MarkdownRenderer;
use namespace::clean;

package Game::EvonyTKR::Controller::Specialities {
  use List::Util qw( any );
  use Mojo::Base 'Mojolicious::Controller', -strict, -signatures;
  use Role::Tiny::With;
  with 'Game::EvonyTKR::Role::MarkdownRenderer';

  use Log::Log4perl;
  use Mojo::File::Share qw(dist_dir dist_file);
  require Game::EvonyTKR::Speciality;
  use Carp;
  our $VERSION = 'v0.01.0';

  my $distDir = Mojo::File::Share::dist_dir('Game::EvonyTKR');

  my $specialities = {};

  sub show($self, $c) {
    my $logger = Log::Log4perl->get_logger(__PACKAGE__);

    my $name = $c->param('name');

    return $c->reply->not_found unless $self->preShow($c, $name);

    if ($c->stash('no_layout')) {
      return $c->render(template => 'specialities/details', layout => undef);
    }
    else {
      return $c->render(
        template => 'specialities/details',
        layout   => 'default'
      );
    }

  }

  # Show details for a specific general
  sub preShow($self, $c, $name) {
    my $logger = Log::Log4perl->get_logger(__PACKAGE__);

    if (not defined $specialities or not scalar keys %$specialities) {
      $logger->trace("show_speciality detected need for total import");
      $self->importAll();
      $logger->trace("show_speciality recieved " . scalar keys %$specialities);
    }
    else {
      my $specialityCount = scalar keys %$specialities;
      $logger->trace(
        "show_speciality has $specialityCount specialities available");
    }

    my $speciality;
    if (exists $specialities->{$name}) {
      $speciality = $specialities->{$name};

      $logger->trace(
        "speciality " . $speciality->name . "found in specialities");

      $c->stash(
        name            => $name,
        speciality      => $speciality,
        title           => $name,
        speciality_name => $name,
      );

    }
    else {
      $logger->error(
        "Cannot find speciality $name in specialities even after laoding");
      return 0;    # the speciality does not exist.
    }
    return 1;
  }

  sub index($self) {
    my $logger = Log::Log4perl->get_logger(__PACKAGE__);

    if (not scalar keys %$specialities) {
      $self->importAll();
    }

    $logger->debug(
      'Start of "Game::EvonyTKR::Controller::Specialities" index handler');
    $self->accepts('html');
    $self->res->headers->cache_control('max-age=1, no-cache');
    $self->stash(specialities => $specialities);
    $self->render(template => 'specialities/index', layout => 'default');
  }

  sub importAll ($self) {
    # Use the class name to get the logger
    my $class  = ref($self);
    my $logger = Log::Log4perl->get_logger($class);
    my $importDir =
      Path::Tiny::path($distDir->child("collections")->child("specialities"));
    my @specialityFiles = $importDir->children(qr/\.yaml\z/);

    my $ypp = YAML::PP->new(
      schema       => [qw/ + Perl /],
      yaml_version => ['1.2', '1.1'],
    );

    foreach my $specialityFile (@specialityFiles) {
      $logger->trace("importing $specialityFile");
      $specialityFile =
        Path::Tiny::path(Encode::decode('utf8', $specialityFile->stringify()));
      my $specialityData = $specialityFile->slurp_utf8;
      my $so             = $ypp->load_string($specialityData);

      eval {
        $logger->warn('bad input of $specialityFile, result is not a HASH');
        next;
      } unless ref $so eq 'HASH' && $so->{name};

      my $speciality = Game::EvonyTKR::Speciality->new(name => $so->{name},);
      $speciality->readFromFile($distDir);
      $specialities->{ $so->{name} } = $speciality;
    }

  }

};
1;

__END__

#ABSTRACT: Handle the /Generals/* routes

=pod

=head1 DESCRIPTION

Handle the routes for /Generals/*

=cut
