use v5.40.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
use YAML::PP;

require Mojolicious::Plugin::DefaultHelpers;
use namespace::clean;

package Game::EvonyTKR::Controller::Generals {
  use Mojo::Base 'Mojolicious::Controller', -signatures;
  use Role::Tiny::With;
  with 'Game::EvonyTKR::Role::MarkdownRenderer';

  use Log::Log4perl;
  use Mojo::File::Share qw(dist_dir);

  my $distDir = Mojo::File::Share::dist_dir('Game::EvonyTKR');

  my $generalImporter = Game::EvonyTKR::General::Importer->new(
    inputDir => $distDir->child('collections')->child('generals'),);

  my $generals = $generalImporter->importAll();
# Show details for a specific general
  sub show_general($self) {
    my $logger = Log::Log4perl->get_logger(__PACKAGE__);
    if (not defined $generals) {
      $generals = $generalImporter->importAll();
    }

    my $name = $self->param('name');
    $logger->debug("Showing general: $name");

    # Here you would fetch data for the specific general
    # and pass it to the template
    if (exists $generals->{$name}) {
      $self->stash(general_name => $name);
    }
    else {
      self->reply->not_found;
    }
    my $general = $generals->{$name};
    $self->stash(
      title               => $general->name(),
      attackBase          => $general->basicAttributes->leadership->base(),
      attackIncrement     => $general->basicAttributes->leadership->increment(),
      defenseBase         => $general->basicAttributes->leadership->base(),
      defenseIncrement    => $general->basicAttributes->leadership->increment(),
      leadershipBase      => $general->basicAttributes->leadership->base(),
      leadershipIncrement => $general->basicAttributes->leadership->increment(),
      politicsBase        => $general->basicAttributes->leadership->base(),
      politicsIncrement   => $general->basicAttributes->leadership->increment(),
      typeValue           => $general->type(),
      ascendingValue      => $general->ascending(),
      general             => $general,
    );

    $self->render(template => 'generals/details', layout => 'default');
  }

  sub index($self) {
    my $logger = Log::Log4perl->get_logger(__PACKAGE__);
    $logger->debug(
      'Start of "Game::EvonyTKR::Controller::Generals" index handler');
    $self->accepts('html');
    $self->res->headers->cache_control('max-age=1, no-cache');
    $self->render_markdown_file($self,
      $distDir->child("pages")->child("Generals")->child("index.md"));
  }

};
1;

__END__

#ABSTRACT: Handle the /Generals/* routes

=pod

=head1 DESCRIPTION

Handle the routes for /Generals/*

=cut
