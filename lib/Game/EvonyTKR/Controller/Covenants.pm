use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';

require YAML::PP;
require Mojo::Promise;
require Mojo::Util;
require List::Util;

require Game::EvonyTKR::Model::General;
require Game::EvonyTKR::Model::General::Pair;
require Game::EvonyTKR::Model::Data;
require Game::EvonyTKR::Model::Covenant;

require UUID;
require Data::Printer;
use namespace::autoclean;

package Game::EvonyTKR::Controller::Covenants {
  use Mojo::Base 'Game::EvonyTKR::Controller::ControllerBase';
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::Covenants', -role;
  use Mojo::IOLoop;
  use Mojo::Promise;
  use MIME::Base64   qw(encode_base64);
  use List::AllUtils qw( all any none );
  use Carp;
  use diagnostics;

  # Specify which collection this controller handles
  sub collection_name {
    return 'covenants';
  }

  sub controller_name ($self) {
    return "Covenants";
  }

  my $base = '/Reference/Covenants';

  ## in part because parent classes use this to override different values of $base
  sub getBase($self) {
    $base =~ s{/$}{};
    return $base;
  }

  has prereqs => sub {
    return [qw(
      load_all_generals
      load_all_covenants
      build_general_indexes
    )];
  };

  # Register this when the application starts
  sub register($c, $app, $config = {}) {
    $c->SUPER::register($app, $config);
    $c->log_info(sprintf('Registering routes for %s', __PACKAGE__));

    eval {
      $c->log_debug(sprintf('setup_helpers for %s', __PACKAGE__));
      $c->setup_helpers($app);
      1;
    } or do {
      my $em = sprintf('setup_helpers failed in %s', __PACKAGE__);
      $c->log_error($em);
      say $em;
    };

    eval {
      $c->log_debug(sprintf('setup_routes for %s', __PACKAGE__));
      $c->setup_routes($app);
      1;
    } or do {
      my $em = sprintf('setup_routes failed in %s', __PACKAGE__);
      $c->log_error($em);
      say $em;
    };

    $c->log_debug("end of register method");
  }

  sub setup_helpers ($c, $app) {

    $app->helper(
      covenant_category_labels => sub {
        return [$c->CovenantCategoryLabels->@*];
      }
    );

    $app->helper(
      covenant_category_names => sub {
        return [$c->CovenantCategoryValues->@*];
      }
    );

  }

  sub setup_routes ($c, $app) {
    if ($c->are_prereqs_outstanding($app->minion, $c->prereqs)) {
      Mojo::IOLoop->timer(
        $c->standard_delay => sub {
          $c->setup_routes($app);
        }
      );
      return;
    }

    $app->add_navigation_item({
      title => 'Details of General Covenants',
      path  => $c->getBase(),
      order => 50,
    });

    my @parts     = split(/::/, __PACKAGE__);
    my $baseClass = pop(@parts);
    my $controller_name =
        $c->can('controller_name')
      ? $c->controller_name()
      : $baseClass;
    $c->log_debug("got controller_name $controller_name.");

    my $mainRoutes = $app->routes->any($base);
    $mainRoutes->get('/')
      ->to(controller => $controller_name, action => 'index')
      ->name("${base}_index");

    # Dynamic catch-all route for individual covenants
    $mainRoutes->get('/:name')
      ->to(controller => $controller_name, action => 'show')
      ->name('covenant_details');

    $c->_ensure_navigation_built($app);
  }

  sub _build_covenant_nav($c, $covenant_name, $app) {
    use Encode qw(is_utf8 decode_utf8);

    # Guard against undefined values - use fallbacks instead of failing
    unless (defined $covenant_name && length($covenant_name)) {
      $c->log_error(
        '_build_covenant_nav called with empty covenant_name, skipping');
      return;
    }

    unless (defined $app) {
      $c->log_error('_build_covenant_nav called with undefined app, skipping');
      return;
    }

    my $display_name =
      is_utf8($covenant_name) ? $covenant_name : decode_utf8($covenant_name);
    my $base = $c->getBase();
    my $path = sprintf('%s/%s', $base, $display_name);

    eval {
      $app->add_navigation_item({
        title  => "Details for ${display_name}",
        path   => $path,
        parent => $base,
        order  => 40,
      });
    };
    if ($@) {
      $c->log_error(sprintf(
        'Failed to add nav item for covenant %s: %s',
        $covenant_name, $@
      ));
    }
  }

  sub _ensure_navigation_built($c, $app) {
    state $nav_built = 0;
    return if $nav_built;

    my @covenant_names = eval { $c->list_covenants()->@* };
    return unless @covenant_names;

    my $test_covenant = eval { $c->get_covenant($covenant_names[0]) };
    return unless $test_covenant;

    foreach my $covenant_name (@covenant_names) {
      my $covenant = eval { $c->get_covenant($covenant_name) };

      # Determine display name with fallbacks
      my $display_name;
      if ($covenant && defined($covenant->primary)) {
        $display_name = eval { $covenant->primary->name };
      }

      # Fallback to covenant filename if primary name unavailable
      if (!defined($display_name) || !length($display_name)) {
        $c->log_warn(sprintf(
'Covenant %s has no valid primary name, using covenant name as fallback',
          $covenant_name // 'undef'));
        $display_name = $covenant_name;
      }

      # Always build nav, even with degraded data
      $c->_build_covenant_nav($display_name, $app);
    }

    $nav_built = 1;
    $c->log_info(
      "Built navigation items for " . scalar(@covenant_names) . " covenants");
  }

  sub index($c) {
    return if $c->check_prereqs_or_wait($c->prereqs);
    $c->log_debug(sprintf('Rendering index for %s', __PACKAGE__));

    # Build navigation items if not already done
    $c->_ensure_navigation_built($c->app);

    # Check if markdown exists for this collection
    my $distDir       = Mojo::Home->new->detect('Game::EvonyTKR');
    my $markdown_path = $distDir->child("share/pages/Covenants/index.md");
    $c->log_debug("markdown_path is $markdown_path");

    my @parts     = split(/::/, ref($c));
    my $baseClass = pop(@parts);
    my $base      = $c->getBase();
    $c->log_debug("Covenants index method has base $base");

    my @items;
    foreach my $cn ($c->list_covenants()->@*) {
      $cn = join(' ', map {ucfirst} split / /, $cn);
      my $path     = sprintf('%s/%s', $c->getBase(), $cn);
      my $covenant = $c->get_covenant($cn);
      push @items, $covenant->primary->name;
    }

    $c->log_debug(sprintf('Items: %s items.', scalar(@items)));
    $c->stash(
      linkBase        => $base,
      items           => \@items,
      collection_name => collection_name(),
      controller_name => $baseClass,
      template        => 'covenants/index',
    );

    if (-f $markdown_path) {
      # Render with markdown
      return $c->render_markdown_page($markdown_path,
        { template => 'covenants/index' });
    }
    else {
      $c->log_debug("no markdown index content found at $markdown_path");
      # Render just the items
      return $c->render(template => 'covenants/index');
    }
  }

  sub show ($c) {
    return if $c->check_prereqs_or_wait($c->prereqs);
    $c->log_debug("start of show method");

    # Build navigation items if not already done
    $c->_ensure_navigation_built($c->app);

    use Encode qw(decode is_utf8);
    my $name = $c->param('name') // '';
    $name = decode('UTF-8', $name) unless is_utf8($name);

    $c->log_debug("show detects name $name, showing details.");

    my $outstanding = $c->outstanding_prereqs([
      'load_all_generals',    'load_all_builtin_books',
      'load_all_specialties', 'load_all_ascending_attributes',
      'load_all_covenants',
    ]);

    if ($outstanding) {
      $c->log_debug(
        sprintf('%s prereq check detected outstanding prereqs.', __PACKAGE__));
      my $delay = $outstanding * 5;
      return $c->render(
        template     => 'shared/loading',
        layout       => 'default',
        title        => 'Loading',
        delay        => $delay,
        redirect_url => $c->url_for->to_abs
      );
    }

    # Normalize name for lookup
    my $normalized_name = $c->normalize($name);
    my $covenant        = $c->get_covenant($normalized_name);

    unless ($covenant) {
      $c->log_error(
        "covenant for '$name' (normalized: '$normalized_name') was not found.");
      $c->reply->not_found;
    }
    $c->log_debug(sprintf(
      'retrieved covenant for "%s": %s',
      $name, Data::Printer::np($covenant)
    ));

    $c->stash(
      item     => $covenant,
      template => 'covenants/details',
      layout   => 'default',
    );
    return $c->render();
  }

}
1;
__END__

  sub setup_helpers($c, $app) {
  #  $app->helper(
  #    get_all_covenants => sub {
  #      return $c->get_all_covenants();
  #    }
  #  );

  #  $app->helper(
  #    getCovenant => sub($self, $general) {
  #      my $name;
  #      if (blessed($general)
  #        && $general->isa('Game::EvonyTKR::Model::General')) {
  #        $name = $general->name;
  #      }
  #      else {
  #        $name = $general;
  #      }
  #      return $c->get_covenant_by_name($name);
  #    }
  #  );
  }

  sub import_single_covenant ($c, $app, $fileName, $index) {
  #  # some filenames have UTF-8 characters.
  #  # these import oddly unless handled carefully.
  #  my $covenantFile =
  #    Mojo::File->new(Encode::decode_utf8($fileName->to_string));
  #  $logger->debug("importing covenant file $covenantFile ");
  #  my $data       = $covenantFile->slurp('UTF-8');
  #  my $hashObject = YAML::PP->new(
  #    schema       => [qw/ + Perl /],
  #    yaml_version => ['1.2', '1.1'],
  #  )->load_string($data);
  #  unless (exists $hashObject->{name} && length($hashObject->{name})) {
  #    $logger->error(sprintf(
  #      'Name is required for a Covenant.  ' . 'Cannot Import %s',
  #      $covenantFile
  #    ));
  #    return;
  #  }
  #  my $primary = $app->get_general($hashObject->{name});
  #  unless ($primary) {
  #    $logger->error("cannot find primary for covenant $hashObject->{name}");
  #    return;
  #  }
  #  my $covenant =
  #    Game::EvonyTKR::Model::Covenant->from_hash($hashObject, $primary);
  #  unless ($covenant) {
  #    $logger->error(
  #      sprintf('failed to build covenant from %s.', $covenantFile));
  #    return;
  #  }
  #  my $allc = $app->get_all_covenants();
  #  $allc->{ $c->SUPER::getConstants()->normalize($covenant->primary->name) } =
  #    $covenant;
  #  $app->plugins->emit(covenant_imported => { covenant => $covenant });
  }

  sub _build_covenant_routes($c, $covenant, $name, $app, $controller_name,
    $mainRoutes) {

  #  $c->log_debug("building route for " . $covenant->primary->name);

  #  my $clean_name = $name;
  #  $clean_name =~ s{^/}{};

  #  $mainRoutes->get($clean_name => { name => $clean_name })
  #    ->to(controller => $controller_name, action => 'show')
  #    ->name("${base}_show");

  #  $app->add_navigation_item({
  #    title  => sprintf('Details for %s\'s Covenant', $name),
  #    path   => "$base/$name",
  #    parent => $base,
  #    order  => 50,
  #  });
  }





}

1;
__END__
