use v5.42.0;
use utf8::all;
use File::FindLib 'lib';
require Game::EvonyTKR::Model::Specialty;
use namespace::autoclean;

package Game::EvonyTKR::Controller::Specialties {
  use Mooish::Base -standard;
  extends 'Game::EvonyTKR::Controller::ControllerBase';

  use List::AllUtils qw( all any none first);
  use Carp;
  use Future::AsyncAwait;
  use Path::Tiny qw(path);

  # Specify which collection this controller handles
  sub collection_name { 'Specialties' }

  my $base = '/Reference/Specialties';

  sub getBase($self) {
    return $base;
  }

  sub controller_name ($self) {
    return "Specialties";
  }

  # Build method - replaces register() from Mojolicious
  sub build ($self) {
    $self->logger->info("Building Specialties controller");

    # Call parent to register common routes
    $self->SUPER::build();

    # Add navigation for main specialties page
    $self->add_navigation_route(
      $base,
      'Details of General Specialties',
      { order => 40, parent => '/Reference' }
    );

    # Register routes
    $self->router->add($base, {
      to => async sub ($self, $ctx) {
        return await $self->index($ctx);
      },
      action => 'http.get',
    });

    $self->router->add("$base/:specialty_name", {
      to => async sub ($self, $ctx, @args) {
        my $specialty_name = $args[0];
        return await $self->show($ctx, $specialty_name);
      },
      action => 'http.get',
    });

    # Build navigation items for individual specialties
    $self->build_nav_items();
  }

  sub specialty_level_names ($self, $level = '', $printable = 0) {
    $level //= '';
    if (length($level) == 0) {
      my $nameList = [];
      foreach my $orig_name ($self->getConstants->SpecialtyLevelValues->@*) {
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
      my $match = first { $level =~ /$_/i }
        $self->getConstants->SpecialtyLevelValues->@*;
      $match =~ s/(\w)(\w*)/\U$1\L$2/;
      return $match;
    }
  }

  sub build_nav_items ($self) {
    # Get specialty loader from app (registered by DataLoaders module)
    my $specialty_loader = $self->specialty_loader();

    unless ($specialty_loader) {
      $self->logger->error("Specialty loader not available");
      return;
    }

    foreach my $specialty_name ($specialty_loader->list_specialties->@*) {
      my $specialty = eval { $specialty_loader->get_specialty($specialty_name) };

      # Determine display name with fallbacks
      my $display_name;
      if ($specialty) {
        $display_name = eval { $specialty->name };
      }

      # Fallback to specialty filename if object name unavailable
      if (!defined($display_name) || !length($display_name)) {
        $self->logger->warn(sprintf(
          'Specialty %s has no valid name, using list name as fallback',
          $specialty_name // 'undef'));
        $display_name = $specialty_name;
      }

      # Add to navigation
      eval {
        $self->add_navigation_route(
          "$base/$display_name",
          "Details for the $display_name Specialty",
          { order => 40, parent => $base }
        );
      };
      if ($@) {
        $self->logger->error(sprintf(
          'Failed to add nav item for specialty %s: %s',
          $specialty_name, $@
        ));
      }
      else {
        $self->logger->debug(sprintf(
          'Added nav item for name "%s" with path "%s/%s"',
          $display_name, $base, $display_name
        ));
      }
    }
  }

  sub sort_levels($self, $levels) {
    # Define the order of levels
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
        ($level_order{ $a->{level} } // 999)
          <=> ($level_order{ $b->{level} } // 999)
          || $a->{level} cmp $b->{level}
      } @$levels
    ];
  }

  async sub index($self, $ctx) {
    my $collection = $self->collection_name();
    $self->logger->debug("Rendering index for $collection");

    my $specialty_loader = $self->specialty_loader();

    unless ($specialty_loader) {
      return $self->render_error(500, "Specialty data not loaded");
    }

    # Gather all specialties
    my $items = [];
    foreach my $sn ($specialty_loader->list_specialties->@*) {
      my $specialty = $specialty_loader->get_specialty($sn);
      unless ($specialty) {
        $self->logger->error(sprintf('Failed to get listed specialty "%s"', $sn));
        next;
      }
      push @{$items}, $specialty;
    }

    $self->logger->debug(
      sprintf('Items: %s with %s items', ref($items), scalar(@$items))
    );

    # Check if markdown exists for this collection
    my $markdown_path = path('share/pages')->child("$collection/index.md");

    my $vars = {
      linkBase        => $base,
      items           => $items,
      collection_name => $collection,
      controller_name => $self->controller_name(),
      title           => 'General Specialties',
      current_year    => (localtime)[5] + 1900,
      css_files       => ['/css/collectionIndex.css'],
      sidebar         => 1,
      navigation        => $self->render_navigation($ctx->req->path),
      site_logo       => $self->site_logo(),
    };

    if ($markdown_path->exists) {
      # Render with markdown content
      my ($frontmatter, $content_html) =
        $self->markdown->render_with_frontmatter($markdown_path->stringify);

      $vars->{content} = $content_html;
      $vars->{title} = $frontmatter->{title} // $vars->{title};

      return $self->render('specialties/index.tt', $vars);
    }
    else {
      # Render just the items list
      return $self->render('specialties/index.tt', $vars);
    }
  }

  async sub show ($self, $ctx, $specialty_name) {
    $self->logger->debug("Show details for specialty: $specialty_name");

    my $specialty_loader = $self->specialty_loader();

    unless ($specialty_loader) {
      return $self->render_error(500, "Specialty data not loaded");
    }

    my $specialty = $specialty_loader->get_specialty($specialty_name);

    unless ($specialty) {
      $self->logger->debug(
        "Specialty '$specialty_name' not found"
      );
      return $self->render_error(404, "Specialty not found");
    }

    $self->logger->debug("Retrieved specialty: $specialty");

    my $vars = {
      item         => $specialty,
      title        => "Details for the " . $specialty->name . " Specialty",
      current_year => (localtime)[5] + 1900,
      css_files    => ['/css/collectionDetails.css'],
      sidebar      => 1,
      navigation     => $self->render_navigation($ctx->req->path),
      site_logo    => $self->site_logo(),
    };

    return $self->render('specialties/details.tt', $vars);
  }
}

1;

__END__

=head1 NAME

Game::EvonyTKR::Controller::Specialties - Thunderhorse controller for General Specialties

=head1 DESCRIPTION

Manages routes and views for General Specialties in EvonyTKR.

Routes:
- GET /Reference/Specialties - Index of all specialties
- GET /Reference/Specialties/:name - Details for specific specialty

=cut
