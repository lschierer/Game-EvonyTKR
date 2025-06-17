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
          $routes->get("/$linkTarget/")->to(
            controller  => $controller_name,
            action      => 'typeIndex',
            generalType => $type,
            linkTarget  => $linkTarget
          )->name("${base}_${type}_index");
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

    my $pm    = $self->app->get_general_pair_manager();
    my @pairs = @{ $pm->get_pairs_by_type($generalType) };
    $logger->debug(sprintf('there are %s pairs to update.', scalar(@pairs)));

    # Process all pairs synchronously for now
    foreach my $pair_index (0 .. $#pairs) {
      $logger->debug("updating pair $pair_index.");
      if (not $pairs[$pair_index]->rootManager) {
        $logger->warn("pair $pair_index has no manager set");
        $pairs[$pair_index]->setRootManager($self->app->get_root_manager());
      }
      $pairs[$pair_index]->updateBuffs();
    }

    # Get sort parameters from query
    my $sort_param = $self->param('sort')
      // 'primary,secondary,marchbuff';    # Default sort columns
    my $dir_param = $self->param('dir')
      // 'desc,desc,desc';                 # Default directions

    # Parse sort parameters
    my @sort_columns = split(',', $sort_param);
    my @sort_dirs    = split(',', $dir_param);

    # Ensure we have directions for all columns (default to desc)
    while (scalar @sort_dirs < scalar @sort_columns) {
      push @sort_dirs, 'desc';
    }

    # Create a sort specification for the template
    my %sort_spec =
      map { $sort_columns[$_] => $sort_dirs[$_] } 0 .. $#sort_columns;

    $logger->debug("Sorting by: " . Data::Printer::np(%sort_spec));

    # Sort the pairs based on parameters
    @pairs = sort {
      my $result = 0;

      # Apply each sort column in order
      for my $i (0 .. $#sort_columns) {
        my $col = $sort_columns[$i];
        my $dir = $sort_dirs[$i];

        my $cmp = 0;
        if ($col eq 'primary') {
          $cmp = $a->primary->name cmp $b->primary->name;
        }
        elsif ($col eq 'secondary') {
          $cmp = $a->secondary->name cmp $b->secondary->name;
        }
        elsif ($col eq 'marchbuff') {
          $cmp =
            $a->marchbuff <=> $b->marchbuff;  # Numeric comparison for marchbuff
        }
        # Add more columns here as you implement them

        # Apply sort direction
        $cmp = -$cmp if $dir eq 'desc';

        # If this column gives a definitive result, use it
        if ($cmp != 0) {
          $result = $cmp;
          last;
        }
      }

      return $result;
    } @pairs;

    # Stash the sorted pairs and sort parameters for the template
    $self->stash(
      items        => \@pairs,
      sort_columns => \@sort_columns,
      sort_dirs    => \@sort_dirs,
      sort_spec    => \%sort_spec
    );

    my $markdown_path =
      $distDir->child("pages/generals/pairs/$linkTarget/index.md");

    my $template = 'generals/pairs/typeIndex';
    $self->stash(template => $template);

    if (-f $markdown_path) {
      $logger->debug("GeneralPairs plugin found a markdown index file");
      # Render with markdown
      return $self->SUPER::render_markdown_file($self, $markdown_path);
    }
    else {
      $logger->debug("GeneralPairs plugin rendering without markdown file");
      # Render just the items
      return $self->render(template => $template);
    }
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
