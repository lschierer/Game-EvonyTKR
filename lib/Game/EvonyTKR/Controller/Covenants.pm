use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require JSON::PP;
require YAML::PP;
require Mojo::Promise;
require Mojo::Util;
require List::Util;

require Game::EvonyTKR::Model::General;
require Game::EvonyTKR::Model::General::Pair;
require Game::EvonyTKR::Model::Buff::Summarizer;
require Game::EvonyTKR::Control::Generals::Routing;
require Game::EvonyTKR::Model::Data;
require Game::EvonyTKR::Service::Cache;
require Game::EvonyTKR::Model::Covenant;

require UUID;
require Data::Printer;
use namespace::autoclean;

package Game::EvonyTKR::Controller::Covenants {
  use Mojo::Base 'Game::EvonyTKR::Controller::ControllerBase';
  use Mojo::Base 'Game::EvonyTKR::Role::Logger', -role, -signatures;
  use Mojo::Base 'Game::EvonyTKR::Controller::Role::Generals', -role,
    -signatures;
  use Mojo::IOLoop;
  use Mojo::Promise;
  use Mojo::JSON     qw(to_json encode_json);
  use MIME::Base64   qw(encode_base64);
  use List::AllUtils qw( all any none );
  use Carp;

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

  # Register this when the application starts
  sub register($c, $app, $config = {}) {
    $c->SUPER::register($app, $config);
    $c->logger->info(sprintf('Registering routes for %s', __PACKAGE__));

    #eval {
    #  say sprintf('setup_helpers for %s', __PACKAGE__);
    #  $c->setup_helpers($app);
    #  1;
    #} or do {
    #  my $em = sprintf('setup_helpers failed in %s', __PACKAGE__);
    #  $c->logger->error($em);
    #  say $em;
    #};

    #eval {
    #  say sprintf('setup_routes for %s', __PACKAGE__);
    #  $c->setup_routes($app);
    #  1;
    #} or do {
    #  my $em = sprintf('setup_routes failed in %s', __PACKAGE__);
    #  $c->logger->error($em);
    #  say $em;
    #};

    $c->logger->debug("end of register method");
  }
}
1;
__END__

  sub setup_routes ($c, $app) {
  #  $app->add_navigation_item({
  #    title => 'Details of General Covenants',
  #    path  => $c->getBase(),
  #    order => 50,
  #  });

  #  my @parts     = split(/::/, ref($c));
  #  my $baseClass = pop(@parts);

  #  my $controller_name =
  #      $c->can('controller_name')
  #    ? $c->controller_name()
  #    : $baseClass;

  #  $c->logger->debug("got controller_name $controller_name.");

  #  my $mainRoutes = $app->routes->any($base);
  #  $mainRoutes->get('/')
  #    ->to(controller => $controller_name, action => 'index')
  #    ->name("${base}_index");

  #  # for backwards compatibility
  #  $mainRoutes->any('/details')->to(
  #    cb => sub ($c) {
  #      $c->redirect_to($c->getBase());
  #    }
  #  );

  #  $app->plugins->on(
  #    all_covenants_imported => sub {
  #      foreach my $covenant (sort { $a->primary->name cmp $b->primary->name }
  #        values $c->get_all_covenants->%*) {
  #        $c->_build_covenant_routes($covenant, $covenant->primary->name,
  #          $app, $controller_name, $mainRoutes);
  #      }
  #    }
  #  );
  }

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

  #  $c->logger->debug("building route for " . $covenant->primary->name);

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

  sub index($c) {
  #  my $collection = collection_name();
  #  $c->logger->debug("Rendering index for $collection");

  #  # Check if markdown exists for this collection
  #  my $distDir =
  #    Path::Tiny::path(Mojo::File::Share::dist_dir('Game::EvonyTKR'));
  #  my $markdown_path = $distDir->child("pages/Covenants/index.md");

  #  my @parts     = split(/::/, __PACKAGE__);
  #  my $baseClass = pop(@parts);
  #  my $base      = $c->getBase();
  #  $c->logger->debug("Covenants index method has base $base");

  #  my @items = values $c->get_all_covenants->%*;
  #  $c->logger->debug(sprintf('Items: %s items.', scalar(@items)));
  #  $c->stash(
  #    linkBase        => $base,
  #    items           => \@items,
  #    collection_name => $collection,
  #    controller_name => $baseClass,
  #    template        => 'covenants/index',
  #  );

  #  if (-f $markdown_path) {
  #    # Render with markdown
  #    return $c->render_markdown_file($markdown_path,
  #      { template => 'covenants/index' });
  #  }
  #  else {
  #    $c->logger->debug("no markdown index content found at $markdown_path");
  #    # Render just the items
  #    return $c->render(template => 'covenants/index');
  #  }
  }

  sub show ($c) {
  #  $c->logger->debug("start of show method");
  #  my $name;
  #  $name = $c->param('name');
  #  $c->logger->debug("show detects name $name, showing details.");

  #  my $covenant = $c->app->getCovenant($name);

  #  unless ($covenant) {
  #    $c->logger->error("covenant for '$name' was not found.");
  #    $c->reply->not_found;
  #  }
  #  $c->logger->debug("retrieved covenant $covenant");

  #  $c->stash(
  #    item     => $covenant,
  #    template => 'covenants/details',
  #    layout   => 'default',
  #  );
  #  return $c->render();
  }

}

1;
__END__
