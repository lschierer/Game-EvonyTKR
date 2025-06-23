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

package Game::EvonyTKR::Controller::Generals::Pairs {
  use Mojo::Base 'Game::EvonyTKR::Controller::ControllerBase';
  use Carp;
  use List::AllUtils qw( all any none );
  use Mojo::Promise;
  use Mojo::Util qw(url_unescape);

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

  sub get_manager ($self) {
    return $self->app->get_root_manager->generalPairManager;
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

    # this is the new route
    $routes->get('/:linkTarget/pair')
      ->to(controller => $controller_name, action => 'pair_buffs')
      ->name("${base}_single_pair_buffs");

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
    my $generalType = $self->stash('generalType');
    my $linkTarget  = $self->stash('linkTarget');

    my $pm    = $self->app->get_general_pair_manager();
    my @pairs = @{ $pm->get_pairs_by_type($generalType) };

    $logger->debug(sprintf('There are %s pairs to return.', scalar(@pairs)));

    # Return just the basic pair information without computing buffs
    my @json_data = map { {
      primary   => { name => $_->primary->name },
      secondary => { name => $_->secondary->name }
    } } @pairs;

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

  sub pair_buffs ($self) {
    my $logger        = Log::Log4perl->get_logger(__PACKAGE__);
    my $primaryName   = url_unescape($self->param('primary'));
    my $secondaryName = url_unescape($self->param('secondary'));
    my $linkTarget    = $self->param('linkTarget');

    $logger->debug(
"Computing pair buffs for primary '$primaryName' secondary '$secondaryName', linkTarget '$linkTarget'"
    );

    if (any { !defined($_) || $_ eq '' }
      ($primaryName, $secondaryName, $linkTarget)) {
      $logger->error("Missing parameter");
      return $self->render(
        json   => { error => "Missing parameter" },
        status => 400
      );
    }

    my $typeMap = {
      'Ground Pairs'  => 'ground_specialist',
      'Ranged Pairs'  => 'ranged_specialist',
      'Siege Pairs'   => 'siege_specialist',
      'Mounted Pairs' => 'mounted_specialist',
    };

    my $targetType = $typeMap->{$linkTarget};
    unless ($targetType) {
      $logger->error("Invalid linkTarget: $linkTarget");
      return $self->render(
        json   => { error => "Unknown pair category" },
        status => 404
      );
    }

    my $pm = $self->app->get_general_pair_manager();

    my ($pair) = grep {
      $_->primary->name eq $primaryName && $_->secondary->name eq $secondaryName
    } $pm->get_pairs_by_type($targetType)->@*;
    unless ($pair) {
      $logger->warn("Pair not found: $primaryName / $secondaryName");
      return $self->render(
        json   => { error => "Pair not found" },
        status => 404
      );
    }

    $pair->updateBuffs();
    return $self->render(json => $pair);
  }
}
1;
