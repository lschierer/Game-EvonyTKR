use v5.40.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Game::EvonyTKR::Model::General::Pair;
require Game::EvonyTKR::Model::General::Pair::Manager;
require Data::Printer;
require Log::Log4perl;
require Sereal;
use namespace::clean;

package Game::EvonyTKR::Plugins::Generals::Pairs {
  use Mojo::Base 'Game::EvonyTKR::Plugins::ControllerBase';
  use Carp;
  use List::AllUtils qw( all any none );
  use Mojo::Promise;

  my $base = '/Generals/Pairs/';

  my @defaultColumns = qw(
    primary secondary marchbuff
    attackbuff defensebuff hpbuff
    groundattackdebuff grounddefensedebuff groundhpdebuff
    mountedattackdebuff mounteddefensedebuff mountedhpdebuff
    rangedattackdebuff rangeddefensedebuff rangedhpdebuff
    siegeattackdebuff siegedefensedebuff siegehpdebuff
  );

  sub getBase($self) {
    return $base;
  }

  sub controller_name ($self) {
    return "Generals::Pairs";
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

    $app->plugins->on(
      'evonytkrtips_initialized' => sub($self, $manager) {
        $logger->debug(
          "evonytkrtips_initialized sub has controller_name $controller_name.");

        if (not defined $manager) {
          $logger->logcroak('No Manager Defined');
        }

        my $pm = $manager->generalPairManager;
        if (not defined $pm) {
          $logger->logcroak('No pair manager in manager');
        }

        my @generalTypes = $pm->get_pair_types();
        $logger->debug("got generalTypes " . Data::Printer::np(@generalTypes));
        foreach my $type (@generalTypes) {
          my $linkTarget = $type =~ s/_/ /gr;
          $linkTarget =~ s/(\w)(\w+)( specialist)?/\U$1\L$2 \UP\Lairs/;
          $routes->get("/$linkTarget/" =>
              { generalType => $type, linkTarget => $linkTarget })
            ->to(controller => $controller_name, action => 'typeIndex')
            ->name("${base}_${type}_index");

          $routes->get("/$linkTarget/data.json" =>
              { generalType => $type, linkTarget => $linkTarget })
            ->to(controller => $controller_name, action => 'typeIndex_json')
            ->name("${base}_${type}_json");
        }
      }
    );

    $app->helper(
      get_general_pair_manager => sub ($c) {
        return $c->app->get_root_manager->generalPairManager;
      }
    );
  }

  sub typeIndex ($self) {
    my $logger      = Log::Log4perl->get_logger(__PACKAGE__);
    my $distDir     = Mojo::File::Share::dist_dir('Game::EvonyTKR');
    my $generalType = $self->stash('generalType');
    my $linkTarget  = $self->stash('linkTarget');

    # Stash data for the template
    $self->stash(template => 'generals/pairs/typeIndex',);

    my $markdown_path =
      $distDir->child("pages/generals/pairs/$linkTarget/index.md");

    if (-f $markdown_path) {
      $logger->debug("Rendering from markdown index file");
      return $self->SUPER::render_markdown_file($self, $markdown_path);
    }
    else {
      $logger->debug("Rendering without markdown file");
      return $self->render;
    }
  }

  sub typeIndex_json ($self) {
    my $logger      = Log::Log4perl->get_logger(__PACKAGE__);
    my $distDir     = Mojo::File::Share::dist_dir('Game::EvonyTKR');
    my $generalType = $self->stash('generalType');
    my $linkTarget  = $self->stash('linkTarget');

    my $pm    = $self->app->get_general_pair_manager();
    my @pairs = @{ $pm->get_pairs_by_type($generalType) };

    $logger->debug(sprintf('There are %s pairs to update.', scalar(@pairs)));

    # Ensure all pairs have rootManager and updated buffs
    foreach my $pair_index (0 .. $#pairs) {
      my $pair = $pairs[$pair_index];

      $logger->debug("Updating pair $pair_index.");

      if (!$pair->rootManager) {
        $logger->warn("Pair $pair_index has no manager set");
        $pair->setRootManager($self->app->get_root_manager());
      }

      $pair->updateBuffs($generalType);
    }

    my @json_data = map { $_->TO_JSON } @pairs;
    $self->render(json => { data => \@json_data });
  }

  sub index ($self) {
    my $logger = Log::Log4perl->get_logger(__PACKAGE__);
    # Check if markdown exists for this
    my $distDir       = Mojo::File::Share::dist_dir('Game::EvonyTKR');
    my $markdown_path = $distDir->child("pages/generals/pairs/index.md");

    my @parts     = split(/::/, ref($self));
    my $baseClass = pop(@parts);
    $self->stash(
      items =>
        [$self->app->get_root_manager->generalPairManager->get_pair_types()],
      controller_name => $baseClass,
    );

    my $template = 'generals/pairs/index';
    $self->stash(template => $template);
    if (-f $markdown_path) {
      $logger->debug("GeneralPairs plugin found a markdown index file");
      # Render with markdown
      $self->stash(template => $template);
      return $self->SUPER::render_markdown_file($self, $markdown_path);
    }
    else {
      $logger->debug("GeneralPairs plugin rendering without markdown file");
      # Render just the items
      return $self->render(template => $template);
    }

  }
}
1;
