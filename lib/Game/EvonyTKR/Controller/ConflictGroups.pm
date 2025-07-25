use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Game::EvonyTKR::Model::General;
require Game::EvonyTKR::Model::General::Manager;
require Game::EvonyTKR::Model::General::Pair;
require Game::EvonyTKR::Model::General::Pair::Manager;
require Game::EvonyTKR::Model::Buff::Summarizer;
require Game::EvonyTKR::Control::Generals::Routing;
require Game::EvonyTKR::Model::Data;
require Data::Printer;
use namespace::clean;

package Game::EvonyTKR::Controller::ConflictGroups {
  use Mojo::Base 'Game::EvonyTKR::Controller::ControllerBase';
  require Mojo::Util;
  use List::AllUtils qw( all any none );
  use Carp;


  sub controller_name ($self) {
    return "ConflictGroups";
  }

  my $base = '/Reference/Conflict Groups';


  sub getBase($self) {
    return $base;
  }

  sub get_manager ($self) {
    return $self->app->get_root_manager->generalConflictGroupManager;
  }

  sub register($self, $app, $config = {}) {
    my $logger = Log::Log4perl->get_logger(__PACKAGE__);
    $logger->info("Registering routes for " . ref($self));
    $self->SUPER::register($app, $config);

    $app->helper(
      get_conflictgroup_manager => sub {
        return $app->get_root_manager->generalConflictGroupManager;
      }
    );

    my $routes = $app->routes->any($base);
    my $controller_name = $self->controller_name();


    $routes->get('/')
      ->to(controller => $controller_name, action => 'index')
      ->name("${base}_index");

    $app->add_navigation_item({
      title  => 'General Conflict Group Details',
      path   => $base,
      parent => '/Reference/',
      order  => 60,
    });

    my $gcm = $app->get_root_manager->generalConflictGroupManager;
    if(not defined $gcm) {
      $logger->logcroak("No ConflictManager in manager");
    }

    $app->plugins->on(
      'evonytkrtips_initialized' => sub($self, $manager) {
        $logger->debug(
          "evonytkrtips_initialized sub has controller_name $controller_name"
        );

        if(not defined $gcm) {
          $logger->logcroak("No ConflictManager in manager");
        }
        my %allConflictGroups = %{$gcm->get_conflict_groups()};
        $logger->info(sprintf(
          'there are %s conflict groups to build routes for',
          scalar keys %allConflictGroups
        ));

        foreach my $key (keys %allConflictGroups) {
          $logger->debug("Building Reference Routes for $key.");
          my $gr = "${base}/${key}";
          my $grn = "${key}ReferenceRoute";

          $routes->get("/${key}" => {conflictGroupId => $key })
            ->to(controller => 'ConflictGroups', action => 'show')
            ->name($grn);

          $app->add_navigation_item({
            parent  => $base,
            path    => $gr,
            order   => 60,
            title   => "Details for " . $allConflictGroups{$key}->name,
          });
        }
      });
  }

  sub index ($self) {
    my $logger     = Log::Log4perl->get_logger(__PACKAGE__);
    $logger->debug("Rendering index for " . __PACKAGE__);

    my $items = $self->get_manager()->get_conflict_groups();
    my @parts     = split(/::/, ref($self));
    my $baseClass = pop(@parts);
    my $base      = $self->getBase();

    $logger->debug(
      sprintf('Items: %s with %s keys.', ref($items), scalar(keys %$items)));
    $self->stash(
      linkBase        => $base,
      items           => $items,
      controller_name => $baseClass,
    );

    return $self->render(template => '/general conflict groups/index');
  }

  sub show ($self) {
    my $logger = Log::Log4perl->get_logger(ref($self));
    $logger->debug("start of show method");
    my $conflictGroupId;
    $conflictGroupId = $self->param('conflictGroupId');

    $logger->debug("show detects id $conflictGroupId, showing details.");
    my $cg = $self->get_manager()->get_conflict_group($conflictGroupId);
    unless (defined $cg && $cg) {
      $logger->error("conflict group '$conflictGroupId' was not found.");
      $self->reply->not_found;
      return;
    }
    $logger->debug("retrieved conflict group " . $cg->name);
    $self->stash(
      template  => '/general conflict groups/details',
      item      => $cg,
      layout    => 'default',
    );
    return $self->render();

  }
}
1;
__END__
