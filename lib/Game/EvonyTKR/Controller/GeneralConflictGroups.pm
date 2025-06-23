use v5.40.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Game::EvonyTKR::Model::General::ConflictGroup;
require Game::EvonyTKR::Model::General::ConflictGroup::Manager;
use namespace::clean;

package Game::EvonyTKR::Controller::GeneralConflictGroups {
  use Mojo::Base 'Game::EvonyTKR::Controller::CollectionBase';

  # Specify which collection this controller handles
  sub collection_name {'general conflict groups'}

  # Override loadItem to add any generals-specific processing
  sub register($self, $app, $config = {}) {
    my $logger = Log::Log4perl->get_logger(__PACKAGE__);
    $logger->info("Registering routes for " . ref($self));
    $self->SUPER::register($app, $config);

    $app->helper(
      get_general_conflictgroup_manager => sub ($c) {
        return $c->app->get_root_manager->generalConflictGroupManager;
      }
    );
  }

  sub index ($self) {
    my $logger = Log::Log4perl->get_logger(__PACKAGE__);
    # Check if markdown exists for this collection
    my $collection    = $self->collection_name;
    my $distDir       = Mojo::File::Share::dist_dir('Game::EvonyTKR');
    my $markdown_path = $distDir->child("pages/$collection/index.md");

    my @parts     = split(/::/, ref($self));
    my $baseClass = pop(@parts);
    $self->stash(
      items => $self->app->get_root_manager->generalConflictGroupManager
        ->get_conflict_groups(
        ),
      collection_name => $collection,
      controller_name => $baseClass,
    );

    my $template = $self->SUPER::index_template();
    $self->stash(template => $template);
    if (-f $markdown_path) {
      $logger->debug(
        "GeneralConflictGroups plugin found a markdown index file");
      # Render with markdown
      $self->stash(template => $template);
      return $self->SUPER::render_markdown_file($self, $markdown_path);
    }
    else {
      $logger->debug(
        "GeneralConflictGroups plugin rendering without markdown file");
      # Render just the items
      return $self->render(template => $template);
    }

  }

  sub show($self) {
    my $logger = Log::Log4perl->get_logger(ref($self));
    $logger->debug("start of show method");
    my $id;

# this parameter is created by the parent class Game::EvonyTKR::Controller::CollectionBase
# 'name' is a reasonable unique identifier for everything *except* conflict conflict groups
# where the upstream source for the information *regularly* changes the names, and the
# process that takes the upstream source and generates files from it cannot figure out
# which existing file should be mapped to which name.  As a result, I treat names as
# disposable values.
    $id = $self->param('name');

    $logger->debug("show detects name $id");
    if (!$id or !length($id)) {
      $logger->error("No id provided for show controller");
      return $self->reply->not_found;
    }
    my $item = $self->app->get_root_manager->generalConflictGroupManager
      ->get_conflict_group($id);
    $self->stash(item => $item);

    if ( !defined $item
      || !blessed($item)
      || !$item->isa('Game::EvonyTKR::Model::General::ConflictGroup')) {
      $logger->error("Item not found or wrong type: $id");
      return $self->reply->not_found;
    }

    return $self->render(template => $self->details_template);
  }

}

1;
