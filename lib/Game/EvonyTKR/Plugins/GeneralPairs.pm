use v5.40.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Game::EvonyTKR::Model::General::Pair;
require Game::EvonyTKR::Model::General::Pair::Manager;
require Data::Printer;
use namespace::clean;

package Game::EvonyTKR::Plugins::GeneralPairs {
  use Mojo::Base 'Game::EvonyTKR::Plugins::ControllerBase';

  my $base = '/Generals/Pairs/';

  sub getBase($self) {
    return $base;
  }

  # Override loadItem to add any generals-specific processing
  sub register($self, $app, $config = {}) {
    my $logger = Log::Log4perl->get_logger(__PACKAGE__);
    $logger->info("Registering routes for " . ref($self));
    $self->SUPER::register($app, $config);

    my @parts     = split(/::/, ref($self));
    my $baseClass = pop(@parts);

    my $controller_name =
        $self->can('controller_name')
      ? $self->controller_name()
      : $baseClass;

    $logger->debug("got controller_name $controller_name.");

    my $r = $app->routes;

    my $routes = $r->any("$base");
    $routes->get('/')
      ->to(controller => $controller_name, action => 'index')
      ->name("${base}_index");

    $app->plugins->on('evonytkrtips_initialized' => sub($self, $manager){
      my $r = $app->routes;
      my $routes = $r->any("$base");

      $logger->debug("evonytkrtips_initialized sub has controller_name $controller_name.");

      if( not defined $manager ) {
        $logger->logcroak('No Manager Defined');
      }

      my $pm = $manager->generalPairManager;
      if( not defined $pm ) {
        $logger->logcroak('No pair manager in manager')
      }

      my @generalTypes =  $pm->get_pair_types();
      $logger->debug("got generalTypes " . Data::Printer::np(@generalTypes) );
      foreach my $type ( @generalTypes ) {
        $routes->get("/$type/")
          ->to(controller => $controller_name, action => 'typeIndex', generalType => $type)
          ->name("${base}_${type}_index");
      }
    } );


    $app->helper(
      get_general_pair_manager => sub ($c) {
        return $c->app->get_root_manager->generalPairManager;
      }
    );
  }

  sub typeIndex ($self) {
    my $logger = Log::Log4perl->get_logger(__PACKAGE__);
    # Check if markdown exists for this
    my $distDir       = Mojo::File::Share::dist_dir('Game::EvonyTKR');

    my $generalType = $self->stash('generalType');

    my $markdown_path = $distDir->child("pages/generals/pairs/$generalType/index.md");

  }

  sub index ($self) {
    my $logger = Log::Log4perl->get_logger(__PACKAGE__);
    # Check if markdown exists for this
    my $distDir       = Mojo::File::Share::dist_dir('Game::EvonyTKR');
    my $markdown_path = $distDir->child("pages/generals/pairs/index.md");

    my @parts     = split(/::/, ref($self));
    my $baseClass = pop(@parts);
    $self->stash(
      items =>[ $self->app->get_root_manager->generalPairManager->get_pair_types() ],
      controller_name => $baseClass,
    );

    my $template = 'generals/pairs/index';
    $self->stash(template => $template);
    if (-f $markdown_path) {
      $logger->debug(
        "GeneralPairs plugin found a markdown index file");
      # Render with markdown
      $self->stash(template => $template);
      return $self->SUPER::render_markdown_file($self, $markdown_path);
    }
    else {
      $logger->debug(
        "GeneralPairs plugin rendering without markdown file");
      # Render just the items
      return $self->render(template => $template);
    }

  }
}
1;
