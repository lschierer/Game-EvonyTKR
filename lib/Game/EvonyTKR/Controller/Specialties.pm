use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Game::EvonyTKR::Model::Specialty;
use namespace::clean;

package Game::EvonyTKR::Controller::Specialties {
  use Mojo::Base 'Game::EvonyTKR::Controller::ControllerBase';
  use List::AllUtils qw( all any none first);
  use Carp;

  my $logger;
  # Specify which collection this controller handles
  sub collection_name {'Specialties'}

  sub get_manager($self) {
    return $self->app->get_root_manager->specialtyManager;
  }

  my $base = '/Reference/Specialties';

  sub getBase($self) {
    return $base;
  }

  sub controller_name ($self) {
    return "Specialties";
  }

  sub get_all_specialties {
    state %specialties;
    return \%specialties;
  }

  sub register($c, $app, $config = {}) {
    $logger = Log::Log4perl->get_logger(__PACKAGE__);
    $logger->info("Registering routes for " . ref($c));
    $c->SUPER::register($app, $config);

    $app->add_navigation_item({
      title  => 'Details of General Specialties',
      path   => $base,
      parent => '/Reference',
      order  => 40,
    });

    my @parts     = split(/::/, ref($c));
    my $baseClass = pop(@parts);

    my $controller_name =
        $c->can('controller_name')
      ? $c->controller_name()
      : $baseClass;

    $logger->debug("got controller_name $controller_name.");

    my $mainRoutes = $app->routes->any($base);

    $mainRoutes->get('/')
      ->to(controller => $controller_name, action => 'index')
      ->name("${base}_index");

    $app->plugins->on(
      'mojo_worker_started' => sub($self, $manager) {
        $logger->debug(
          "mojo_worker_started sub has controller_name $controller_name.");
        $c->load_specialities($app);

        $app->plugins->on(
          specialties_imported => sub ($self, $args) {
            $c->build_routes($app, $mainRoutes, $controller_name);
          }
        );
      }
    );

    $app->helper(
      get_all_specialties => sub {
        return $c->get_all_specialties;
      }
    );

    $app->helper(
      specialty_level_names => sub ($self, $level = '', $printable = 0) {
        $level //= '';    # Ensure defined
        if (length($level) == 0) {
          my $nameList = [];
          foreach
            my $orig_name ($c->SUPER::getConstants->SpecialtyLevelValues->@*) {
            $logger->debug(
              "specialty_level_names evaluating specialty level name $orig_name"
            );
            my $name;
            if ($printable) {
              $name = $orig_name =~ s/(\w)(\w*)/\U$1\L$2/r;
            }
            else {
              $name = $orig_name;
            }
            push @$nameList, $name;
          }
          return $nameList;
        }
        else {
          $logger->debug(
            "specialty_level_names sees levels"
              . Data::Printer::np(
              $c->SUPER::getConstants->SpecialtyLevelValues->@*
              )
          );
          my $match = first { $level =~ /$_/i }
            $c->SUPER::getConstants->SpecialtyLevelValues->@*;
          $match =~ s/(\w)(\w*)/\U$1\L$2/;
          return $match;
        }
      }
    );

  }

  sub build_routes ($c, $app, $mainRoutes, $controller_name) {
    foreach my $specialty (values $c->get_all_specialties()->%*) {
      my $name = $specialty->name;

      my $clean_name = $name;
      $clean_name =~ s{^/}{};

      $mainRoutes->get($clean_name => { name => $clean_name })
        ->to(controller => $controller_name, action => 'show')
        ->name("${base}_show");

      $app->add_navigation_item({
        title  => "Details for $name",
        path   => "$base/$name",
        parent => "$base",
        order  => 40,
      });

      $logger->debug(
        sprintf('added route and nav item for name "%s" ', $name)
          . sprintf(
          'cleaned to "%s" with path "%s/%s"',
          $clean_name, $base, $name
          )
      );
    }
  }

  sub load_single_specialty($c, $app, $fileName, $index) {
    # some filenames have UTF-8 characters.
    # these import oddly unless handled carefully.
    my $specialtyFile =
      Mojo::File->new(Encode::decode_utf8($fileName->to_string));
    $logger->debug("importing Specialty file $specialtyFile ");
    my $data       = $specialtyFile->slurp('UTF-8');
    my $hashObject = YAML::PP->new(
      schema       => [qw/ + Perl /],
      yaml_version => ['1.2', '1.1'],
    )->load_string($data);
    unless (exists $hashObject->{name} && length($hashObject->{name})) {
      $logger->error(sprintf(
        'Name is required for a Specialty.  ' . 'Cannot Import %s',
        $specialtyFile
      ));
      return;
    }

    my $s = Game::EvonyTKR::Model::Specialty->from_hash($hashObject);
    #unless ($s) {
    #  $logger->error(
    #    sprintf('Failed to import Specialty from %s.', $specialtyFile));
    #  return;
    #}
    #$c->get_all_specialties()->{ $c->SUPER::getConstants->normalize($s->name) }
    #  = $s;
    #$logger->debug(
    #  sprintf('successfully imported %s from %s', $s->name, $specialtyFile));
    #$app->plugins->emit(specialty_imported => { specialty => $s });
  }

  sub load_specialities ($c, $app) {
    my $expectedTotal = 0;

    $app->plugins->on(
      specialty_imported => sub($self, $args) {
        my $specialty = $args->{speciality};
        my $alls      = $c->get_all_specialties();
        my $count     = scalar(keys $alls->%*);
        if ($count >= $expectedTotal) {
          $logger->info(sprintf('All %s Specialties imported.', $count));
          $app->plugins->emit(specialties_imported => { count => $count });
        }
        else {
          $logger->debug(
            sprintf('imported %s of %s Specialties.', $count, $expectedTotal));
        }
      }
    );

    Mojo::File->new($app->config('distDir'))
      ->child('collections/data/specialties/')
      ->list_tree->grep(sub {qr/\.y\{a\}?ml$/})->each(
      sub ($e, $index) {
        $expectedTotal++;
        my $delay = rand(4.0);
        Mojo::IOLoop->timer(
          $delay => sub {
            $c->load_single_specialty($app, $e, $index);
          }
        );
      }
      );
    $logger->info(
      sprintf('Async import of %s specialities started.', $expectedTotal));
  }

  sub sort_levels($self, $levels) {
    # Define the order of levels (if they don't sort alphabetically)
    my %level_order = (
      'Green'  => 1,
      'Blue'   => 2,
      'Purple' => 3,
      'Orange' => 4,
      'Gold'   => 5,
    );

    # Return sorted array
    return [
      sort {
        # Use the defined order if available,
        # otherwise fall back to string comparison
        ($level_order{ $a->{level} } // 999)
          <=> ($level_order{ $b->{level} } // 999)
          || $a->{level} cmp $b->{level}
      } @$levels
    ];
  }

  sub index($c) {
    my $collection = collection_name();
    $logger->debug("Rendering index for $collection");

    # Check if markdown exists for this collection
    my $distDir       = Mojo::File::Share::dist_dir('Game::EvonyTKR');
    my $markdown_path = $distDir->child("pages/$collection/index.md");

    my @parts     = split(/::/, ref($c));
    my $baseClass = pop(@parts);
    my $base      = $c->getBase();
    $logger->debug("Specialties index method has base $base");

    my $items = $c->get_all_specialties();
    $logger->debug(
      sprintf('Items: %s with %s items.', ref($items), scalar(@$items)));
    $c->stash(
      linkBase        => $base,
      items           => $items,
      collection_name => $collection,
      controller_name => $baseClass,
    );

    if (-f $markdown_path) {
      # Render with markdown
      $c->stash(template => 'specialties/index');

      return $c->render_markdown_file($markdown_path,
        { template => 'specialties/index' });
    }
    else {
      # Render just the items
      return $c->render(template => 'specialties/index');
    }
  }

  sub show ($c) {
    $logger->debug("start of show method");
    my $name;
    $name = $c->param('name');
    $logger->debug("show detects name $name, showing details.");

    my $specialty =
      $c->get_all_specialties->{ $c->SUPER::getConstants->normalize($name) };

    unless ($specialty) {
      $logger->error("speciality '$name' was not found.");
      return $c->reply->not_found;
    }
    $logger->debug("retrieved specialty $specialty");

    $c->stash(
      item     => $specialty,
      template => 'specialties/details',
      layout   => 'default',
    );
    return $c->render();
  }

}

1;
