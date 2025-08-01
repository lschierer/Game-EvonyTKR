use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
use namespace::clean;

package Game::EvonyTKR::Plugins::Navigation {
  use Mojo::Base 'Mojolicious::Plugin';
  use Carp;
  require Log::Log4perl;

  my %nav_items_by_path;
  my %raw_paths;

  my $rejected_items_by_path = {
    '/policy'         => 1,
    '/policy/privacy' => 1,
    '/index'          => 1,
  };

  sub register {
    my ($self, $app, $config) = @_;
    my $logger = Log::Log4perl->get_logger(__PACKAGE__);
    $logger->info("Registering navigation plugin");

    $app->helper(
      add_navigation_item => sub {
        my ($c, $item) = @_;
        my $log = Log::Log4perl->get_logger(__PACKAGE__);

        unless (ref $item eq 'HASH' && $item->{path}) {
          $log->error(
            "Invalid item (missing path): " . Data::Printer::np($item));
          return;
        }

        my $path = $item->{path};

        # DEBUG: Log all paths being registered
        $log->debug(
          "NAVIGATION: Registering path '$path' with title '$item->{title}'");

        if ($rejected_items_by_path->{$path}) {
          $log->debug("Skipping rejected path $path");
          return;
        }

        unless (exists $item->{title}) {
          $log->error("Item rejected: missing title for $path");
          return;
        }

        $raw_paths{$path} = 1;

        if (exists $nav_items_by_path{$path}) {
          my $existing = $nav_items_by_path{$path};
          if (exists $item->{order} && exists $existing->{order}) {
            if ($item->{order} < $existing->{order}) {
              $nav_items_by_path{$path} = $item;
            }
          }
          elsif (exists $item->{order}) {
            $nav_items_by_path{$path} = $item;
          }
          elsif (!exists $existing->{order}) {
            $log->error("Duplicate navigation item at $path without order");
          }
        }
        else {
          $nav_items_by_path{$path} = $item;
        }

        return 1;
      }
    );

    $app->helper(
      get_existing_navigation_items => sub {
        return \%nav_items_by_path;
      }
    );

    $app->helper(
      generate_navigation => sub {
        my $c         = shift;
        my $structure = {};

        foreach my $path (keys %nav_items_by_path) {
          $self->_add_path_to_structure($structure, $path,
            $nav_items_by_path{$path});
        }

        return $self->_prune_and_sort($structure, '');
      }
    );

    $app->hook(
      before_render => sub {
        my ($c, $args) = @_;
        return
          if $args->{json} || $args->{text} || $c->req->url->path =~ /\.json$/;
        $c->stash(navigation => $c->generate_navigation);
      }
    );
  }

  sub _add_path_to_structure {
    my ($self, $tree, $path, $item) = @_;
    my @segments  = grep {length} split '/', $path;
    my $current   = $tree;
    my $full_path = '';

    # sanity checks
    return unless defined $path && length $path;
    return unless ref($item) eq 'HASH';
    return unless defined $item->{title};

    for my $i (0 .. $#segments) {
      my $seg = $segments[$i];
      if (!$seg) {
        return;    # <--- # another sanity check
      }
      $full_path .= "/$seg";
      my $is_leaf = $i == $#segments;

      $current->{$seg} //= {
        title    => $self->_titleize($seg),
        path     => $full_path,
        order    => 9999,
        children => {},
      };

      if ($is_leaf) {
        $current->{$seg}->{title} = $item->{title};
        $current->{$seg}->{order} = $item->{order} // 9999;
      }

      $current = $current->{$seg}->{children};
    }
  }

  sub _prune_and_sort {
    my ($self, $tree, $prefix) = @_;
    my $logger = Log::Log4perl->get_logger(__PACKAGE__);
    my @result;

    foreach my $key (sort keys %$tree) {
      my $node = $tree->{$key};
      my $has_valid_children =
        scalar keys %{ $node->{children} };    # Will check recursively

      my $children = $self->_prune_and_sort($node->{children}, $node->{path});

      push @result,
        {
        title    => $node->{title},
        path     => $node->{path},
        order    => $node->{order} // 9999,
        children => $children,
        }
        if @$children
        || exists $raw_paths{ $node->{path} }
        || exists $raw_paths{"$node->{path}/index"};
    }

    @result = sort {
      $a->{order} <=> $b->{order}
        || lc($a->{title}) cmp lc($b->{title})
    } @result;
    $logger->debug(
      "_prune_and_sort returning result " . Data::Printer::np(@result));
    return \@result;
  }

  sub _titleize {
    my ($self, $seg) = @_;
    $seg =~ s/_/ /g;
    return join ' ', map { ucfirst lc } split ' ', $seg;
  }
}

1;
