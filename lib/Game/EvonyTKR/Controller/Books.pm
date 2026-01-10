use v5.42.0;
use utf8::all;
use File::FindLib 'lib';
require Game::EvonyTKR::Model::Book;
use namespace::autoclean;

package Game::EvonyTKR::Controller::Books {
  use Mooish::Base -standard;
  extends 'Game::EvonyTKR::Controller::ControllerBase';

  use List::AllUtils qw(all any none first);
  use Carp;
  use Future::AsyncAwait;
  use Path::Tiny qw(path);
  use URI::Escape qw(uri_unescape);

  # Specify which collection this controller handles
  sub collection_name { 'Books' }

  my $base = '/Reference/Books';

  sub getBase($self) {
    return $base;
  }

  sub controller_name ($self) {
    return "Books";
  }

  # Build method - replaces register() from Mojolicious
  sub build ($self) {
    $self->logger->info("Building Books controller");

    # Call parent to register common routes
    $self->SUPER::build();

    # Add navigation for main books page
    $self->add_navigation_route(
      $base,
      'Books',
      { order => 30, parent => '/Reference' }
    );

    # Add navigation for Skill Books
    $self->add_navigation_route(
      "$base/Skill",
      'Skill Books',
      { order => 10, parent => $base }
    );

    # Add navigation for Generic Books
    $self->add_navigation_route(
      "$base/Generic",
      'Generic Books',
      { order => 20, parent => $base }
    );

    # Register routes
    # Main books landing page
    $self->router->add($base, {
      to => async sub ($self, $ctx) {
        return await $self->index($ctx);
      },
      action => 'http.get',
    });

    # Skill books index
    $self->router->add("$base/Skill", {
      to => async sub ($self, $ctx) {
        return await $self->skill_books_index($ctx);
      },
      action => 'http.get',
    });

    # Generic books index
    $self->router->add("$base/Generic", {
      to => async sub ($self, $ctx) {
        return await $self->generic_books_index($ctx);
      },
      action => 'http.get',
    });

    # Skill book detail
    $self->router->add("$base/Skill/:book_name", {
      to => async sub ($self, $ctx, @args) {
        my $book_name = uri_unescape($args[0]);
        return await $self->show_skill_book($ctx, $book_name);
      },
      action => 'http.get',
    });

    # Generic book detail
    $self->router->add("$base/Generic/:book_name", {
      to => async sub ($self, $ctx, @args) {
        my $book_name = uri_unescape($args[0]);
        return await $self->show_generic_book($ctx, $book_name);
      },
      action => 'http.get',
    });

    # Build navigation items for individual books
    $self->build_nav_items();
  }

  sub build_nav_items ($self) {
    # Get books loader from app (registered by DataLoaders module)
    my $books_loader = $self->books_loader();

    unless ($books_loader) {
      $self->logger->error("Books loader not available");
      return;
    }

    # Add skill books to navigation
    foreach my $book_name ($books_loader->list_skill_books->@*) {
      my $book = eval { $books_loader->get_skill_book($book_name) };

      # Determine display name with fallbacks
      my $display_name;
      if ($book) {
        $display_name = eval { $book->name };
      }

      # Fallback to book filename if object name unavailable
      if (!defined($display_name) || !length($display_name)) {
        $self->logger->warn(sprintf(
          'Skill book %s has no valid name, using list name as fallback',
          $book_name // 'undef'));
        $display_name = $book_name;
      }

      # Add to navigation
      eval {
        $self->add_navigation_route(
          "$base/Skill/$display_name",
          $display_name,
          { order => 10, parent => "$base/Skill" }
        );
      };
      if ($@) {
        $self->logger->error(sprintf(
          'Failed to add nav item for skill book %s: %s',
          $book_name, $@
        ));
      }
      else {
        $self->logger->debug(sprintf(
          'Added nav item for skill book "%s" with path "%s/Skill/%s"',
          $display_name, $base, $display_name
        ));
      }
    }

    # Add generic books to navigation
    foreach my $book_key ($books_loader->list_generic_books->@*) {
      my $book = eval { $books_loader->get_generic_book($book_key) };

      # Determine display name with fallbacks
      my $display_name;
      if ($book) {
        $display_name = eval { $book->name };
        if ($book->can('level') && defined $book->level) {
          $display_name = sprintf("%s (Level %d)", $display_name, $book->level);
        }
      }

      # Fallback to book key if object name unavailable
      if (!defined($display_name) || !length($display_name)) {
        $self->logger->warn(sprintf(
          'Generic book %s has no valid name, using list name as fallback',
          $book_key // 'undef'));
        $display_name = $book_key;
      }

      # Add to navigation
      eval {
        $self->add_navigation_route(
          "$base/Generic/$display_name",
          $display_name,
          { order => 20, parent => "$base/Generic" }
        );
      };
      if ($@) {
        $self->logger->error(sprintf(
          'Failed to add nav item for generic book %s: %s',
          $book_key, $@
        ));
      }
      else {
        $self->logger->debug(sprintf(
          'Added nav item for generic book "%s" with path "%s/Generic/%s"',
          $display_name, $base, $display_name
        ));
      }
    }
  }

  # Main books landing page
  async sub index($self, $ctx) {
    $self->logger->debug("Rendering books landing page");

    my $vars = {
      title        => 'Books',
      current_year => (localtime)[5] + 1900,
      sidebar      => 1,
      navigation   => $self->render_navigation($ctx->req->path),
      site_logo    => $self->site_logo(),
    };

    return $self->render('books/index.tt', $vars);
  }

  # Skill books index
  async sub skill_books_index($self, $ctx) {
    $self->logger->debug("Rendering skill books index");

    my $books_loader = $self->books_loader();

    unless ($books_loader) {
      return $self->render_error(500, "Books data not loaded");
    }

    # Gather all skill books
    my $items = [];
    foreach my $book_name ($books_loader->list_skill_books->@*) {
      my $book = $books_loader->get_skill_book($book_name);
      unless ($book) {
        $self->logger->error(sprintf('Failed to get listed skill book "%s"', $book_name));
        next;
      }
      push @{$items}, $book;
    }

    $self->logger->debug(
      sprintf('Skill books: %s with %s items', ref($items), scalar(@$items))
    );

    my $vars = {
      items        => $items,
      title        => 'Skill Books',
      current_year => (localtime)[5] + 1900,
      css_files    => ['/css/collectionIndex.css'],
      sidebar      => 1,
      navigation   => $self->render_navigation($ctx->req->path),
      site_logo    => $self->site_logo(),
    };

    return $self->render('books/skill_books_index.tt', $vars);
  }

  # Generic books index
  async sub generic_books_index($self, $ctx) {
    $self->logger->debug("Rendering generic books index");

    my $books_loader = $self->books_loader();

    unless ($books_loader) {
      return $self->render_error(500, "Books data not loaded");
    }

    # Gather all generic books
    my $items = [];
    foreach my $book_key ($books_loader->list_generic_books->@*) {
      my $book = $books_loader->get_generic_book($book_key);
      unless ($book) {
        $self->logger->error(sprintf('Failed to get listed generic book "%s"', $book_key));
        next;
      }
      push @{$items}, $book;
    }

    $self->logger->debug(
      sprintf('Generic books: %s with %s items', ref($items), scalar(@$items))
    );

    my $vars = {
      items        => $items,
      title        => 'Generic Books',
      current_year => (localtime)[5] + 1900,
      css_files    => ['/css/collectionIndex.css'],
      sidebar      => 1,
      navigation   => $self->render_navigation($ctx->req->path),
      site_logo    => $self->site_logo(),
    };

    return $self->render('books/generic_books_index.tt', $vars);
  }

  # Show skill book details
  async sub show_skill_book ($self, $ctx, $book_name) {
    $self->logger->debug("Show details for skill book: $book_name");

    my $books_loader = $self->books_loader();

    unless ($books_loader) {
      return $self->render_error(500, "Books data not loaded");
    }

    my $book = $books_loader->get_skill_book($book_name);

    unless ($book) {
      $self->logger->debug("Skill book '$book_name' not found");
      return $self->render_error(404, "Skill book not found");
    }

    $self->logger->debug("Retrieved skill book: $book");

    my $vars = {
      item         => $book,
      title        => $book->name,
      current_year => (localtime)[5] + 1900,
      css_files    => ['/css/collectionDetails.css'],
      sidebar      => 1,
      navigation   => $self->render_navigation($ctx->req->path),
      site_logo    => $self->site_logo(),
    };

    return $self->render('books/details.tt', $vars);
  }

  # Show generic book details
  async sub show_generic_book ($self, $ctx, $book_name) {
    $self->logger->debug("Show details for generic book: $book_name");

    my $books_loader = $self->books_loader();

    unless ($books_loader) {
      return $self->render_error(500, "Books data not loaded");
    }

    my $book = $books_loader->get_generic_book($book_name);

    unless ($book) {
      $self->logger->debug("Generic book '$book_name' not found");
      return $self->render_error(404, "Generic book not found");
    }

    $self->logger->debug("Retrieved generic book: $book");

    my $vars = {
      item         => $book,
      title        => $book->name,
      current_year => (localtime)[5] + 1900,
      css_files    => ['/css/collectionDetails.css'],
      sidebar      => 1,
      navigation   => $self->render_navigation($ctx->req->path),
      site_logo    => $self->site_logo(),
    };

    return $self->render('books/details.tt', $vars);
  }
}

1;

__END__

=head1 NAME

Game::EvonyTKR::Controller::Books - Thunderhorse controller for Books

=head1 DESCRIPTION

Manages routes and views for Skill Books and Generic Books in EvonyTKR.

Routes:
- GET /Reference/Books - Main books landing page
- GET /Reference/Books/Skill - Index of all skill books
- GET /Reference/Books/Generic - Index of all generic books
- GET /Reference/Books/Skill/:name - Details for specific skill book
- GET /Reference/Books/Generic/:name - Details for specific generic book

=cut
