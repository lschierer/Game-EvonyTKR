use v5.40.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require Role::Tiny;
use namespace::clean;

package Game::EvonyTKR::Controller::CollectionBase {
  use Mojo::Base 'Game::EvonyTKR::Controller::ControllerBase';

  use Mojo::File::Share qw(dist_dir);
  require Path::Tiny;
  use YAML::PP;

  sub collection_name ($self) {
    return '';
  }

  my $base;

  sub register {
    my ($self, $app, $config) = @_;
    my $logger = Log::Log4perl->get_logger(__PACKAGE__);
    $logger->info("Registering navigation plugin");

    # Store navigation items
    my $navigation_items = [];

    # Helper to add navigation items
    $app->helper(
      add_navigation_item => sub {
        my ($c, $item) = @_;

        # Validate item structure
        unless (ref $item eq 'HASH'
          && exists $item->{title}
          && exists $item->{path}) {
          $logger->error(
            "Invalid navigation item: " . Data::Printer::np($item));
          return;
        }

        # Add the item
        push @$navigation_items, $item;
        $logger->debug(
          "Added navigation item: " . $item->{title} . " => " . $item->{path});
        return 1;
      }
    );

    # Helper to get all navigation items
    $app->helper(
      get_navigation_items => sub {
        my $c = shift;
        return $navigation_items;
      }
    );

    # Add the navigation to every request
    $app->hook(
      before_render => sub {
        my ($c, $args) = @_;

        # Skip for API routes and non-HTML responses
        return
          if $args->{json} || $args->{text} || $c->req->url->path =~ /\.json$/;

        # Generate navigation and add to stash
        $c->stash(navigation => $c->generate_navigation);
      }
    );
  }

  # Optional method for custom template
  sub details_template($self) {
    return $self->collection_name . "/details";
  }

  # Index template - can be overridden
  sub index_template($self) {
    return $self->collection_name . "/index";
  }

  # Load the index of available items
  sub loadIndex($self) {
    my $collection = $self->collection_name;
    my $logger     = Log::Log4perl->get_logger(ref($self));
    $logger->debug("Loading index for $collection");

    my $distDir = Mojo::File::Share::dist_dir('Game::EvonyTKR');
    my $dir     = $distDir->child("collections/$collection");

    unless (-d $dir) {
      $logger->error("Collection directory not found: $dir");
      return {};
    }

    my %items;
    my @files = $dir->list->grep(qr/\.ya?ml$/)->each;

    foreach my $file (@files) {
      # Get the basename without extension
      my $basename = Path::Tiny::path($file)->basename('.yaml');

      # Ensure the filename is properly decoded as UTF-8
      utf8::decode($basename) unless utf8::is_utf8($basename);

      # Use the decoded basename as the name
      my $name = $basename;

      $items{$name} = {
        name => $name,
        file => $file,
        # Store the original filename for debugging
        original_filename => Path::Tiny::path($file)->basename,
      };
    }

    $logger->debug("Indexed " . scalar(keys %items) . " items for $collection");
    return \%items;
  }

  # Check if an item exists
  sub itemExists($self, $name) {
    my $items = $self->getItems();
    return exists $items->{$name};
  }

  # Get all items (implement caching if needed)
  sub getItems($self) {
    my $collection = $self->collection_name;
    state %collection_items;
    $collection_items{$collection} //= $self->loadIndex();
    return $collection_items{$collection};

  }

  # Add a method that can be called by controllers
  sub _base_load_item($self, $name) {
    # Original loadItem implementation
    my $items  = $self->getItems();
    my $logger = Log::Log4perl->get_logger(ref($self));

    unless (exists $items->{$name}) {
      $logger->error("Item not found: $name");
      return undef;
    }

    my $file = $items->{$name}->{file};
    my $data = Path::Tiny::path($file)->slurp_utf8();
    my $yaml = YAML::PP->new(
      schema       => [qw/ + Perl /],
      yaml_version => ['1.2', '1.1'],
    )->load_string($data);

    return $yaml;
  }

  # The original method now delegates to the base implementation
  sub loadItem($self, $name) {
    return $self->_base_load_item($name);
  }

  # Default index action
  sub index($self) {
    my $collection = $self->collection_name;
    my $items      = $self->getItems();
    my $logger     = Log::Log4perl->get_logger(ref($self));

    $logger->debug("Rendering index for $collection");

    # Check if markdown exists for this collection
    my $distDir       = Mojo::File::Share::dist_dir('Game::EvonyTKR');
    my $markdown_path = $distDir->child("pages/$collection/index.md");

    my @parts     = split(/::/, ref($self));
    my $baseClass = pop(@parts);
    $self->stash(
      items           => $items,
      collection_name => $collection,
      controller_name => $baseClass,
    );

    if (-f $markdown_path) {
      # Render with markdown
      $self->stash(template => $self->index_template);
      return $self->render_markdown_file($self, $markdown_path);
    }
    else {
      # Render just the items
      return $self->render(template => $self->index_template);
    }
  }

  sub get_manager($self) {
    my $collection = $self->collection_name;
    my $logger     = Log::Log4perl->get_logger(ref($self));

    $logger->error(
"FIXME: No manager defined for collection '$collection'. Implement get_manager in "
        . ref($self));
    return undef;    # Return undef to trigger fallback to legacy method
  }

  # Default show action
  sub show($self) {
    my $logger = Log::Log4perl->get_logger(ref($self));
    $logger->debug("start of legacy show method");
    my $name;
    $name = $self->param('name');

    $logger->debug("show detects name $name");

    my $manager = $self->get_manager();
    if ($manager) {
      my $method =
          $manager->can('getBook')       ? 'getBook'
        : $manager->can('getGeneral')    ? 'getGeneral'
        : $manager->can('get_item')      ? 'get_item'
        : $manager->can('getSpeciality') ? 'getSpeciality'
        :                                  undef;

      if ($method && (my $item = $manager->$method($name))) {
        $logger->debug("Item '$name' found via manager");
        $self->stash(item => $item);
      }
      else {
        $logger->error("Item not found: $name");
        return $self->reply->not_found;
      }
    }
    else {
      unless ($self->itemExists($name)) {
        $logger->error("Item not found: $name");
        return $self->reply->not_found;
      }
      my $item = $self->loadItem($name);
      $self->stash(item => $item);
    }

    if ($self->stash('no_layout')) {
      return $self->render_to_string(
        template => $self->details_template,
        layout   => undef
      );
    }
    else {
      return $self->render(template => $self->details_template);
    }
  }

}

1;
