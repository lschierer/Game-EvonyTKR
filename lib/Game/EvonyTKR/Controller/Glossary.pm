use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Game::EvonyTKR::Model::Book::BuiltinBook;
require Game::EvonyTKR::Model::Book::Manager;
use namespace::clean;

package Game::EvonyTKR::Controller::Glossary {
  use Mojo::Base 'Game::EvonyTKR::Controller::ControllerBase';

  sub controller_name ($self) {
    return "Glossary";
  }

  my $base = '/Reference/Glossary';

  sub getBase($self) {
    $base =~ s{/$}{};
    return $base;
  }

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

    my $mainRoutes = $app->routes->any($base);
    $mainRoutes->get('/')
      ->to(controller => $controller_name, action => 'index')
      ->name("${base}_index");

    $app->add_navigation_item({
      title => 'Glossary of Terms',
      path  => $base,
      order => 60,
    });

    $app->helper(
      getDefinitionForTerm => sub ($self, $term) {
        return
          $self->app->get_root_manager->glossaryManager->getDefinitionForTerm(
          $term);
      }
    );

    $app->helper(
      getTermsByLetter => sub ($self, $letter) {
        return $self->app->get_root_manager->glossaryManager->getByLetter(
          $letter);
      }
    );

  }

  sub index($c) {
    my $logger = Log::Log4perl->get_logger(__PACKAGE__);
    $logger->debug("Start of index method for " . __PACKAGE__);
    my $glossary_manager = $c->app->get_root_manager->glossaryManager;
    my $terms            = $glossary_manager->getAll();

    $c->render(
      template          => 'glossary/index',
      available_letters => $glossary_manager->getAvailableLetters(),
    );

  }

}
1;
__END__
