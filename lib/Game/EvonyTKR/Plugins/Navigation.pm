use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
use namespace::autoclean;

package Game::EvonyTKR::Plugins::Navigation {
  use Mojo::Base 'Mojolicious::Plugin';
  use Mojo::Base 'Game::EvonyTKR::Role::Logging', -role;
  use Carp;

  my %nav_items_by_path;
  my %raw_paths;

  my $rejected_items_by_path = {
    '/policy'         => 1,
    '/policy/privacy' => 1,
    '/index'          => 1,
  };

  sub register ($self, $app, $config = {}) {
    my $register_message = sprintf(
      'Registering %s plugin; %s;  %s',
      __PACKAGE__,
      $self->debug_log_level(),
      $self->debug_log_category()
    );
    $self->log_info($register_message);

    $app->helper(
      add_navigation_item => sub {
        my ($c, $item) = @_;

        unless (ref $item eq 'HASH' && $item->{path}) {
          $self->log_error(
            "Invalid item (missing path): " . Data::Printer::np($item));
          return;
        }

        my $path = $item->{path};

        # DEBUG: Log all paths being registered
        $self->log_debug(
          "NAVIGATION: Registering path '$path' with title '$item->{title}'");

        if ($rejected_items_by_path->{$path}) {
          $self->log_debug("Skipping rejected path $path");
          return;
        }

        unless (exists $item->{title}) {
          $self->log_error("Item rejected: missing title for $path");
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
            $self->log_error(
              "Duplicate navigation item at $path without order");
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
      generate_sitemap_xml => sub {
        my $c        = shift;
        my $base_url = $c->req->url->base->to_string;
        $base_url =~ s{/$}{};    # Remove trailing slash

        my @urls;
        for my $path (sort keys %raw_paths) {
          push @urls,
            {
            loc        => "$base_url$path",
            lastmod    => $c->_get_lastmod_for_path($path),
            changefreq => $c->_get_changefreq_for_path($path),
            priority   => $c->_get_priority_for_path($path)
            };
        }

        return $c->_render_sitemap_xml(\@urls);
      }
    );

    $app->helper(
      _get_lastmod_for_path => sub {
        my ($c, $path) = @_;
        # Default to current date, controllers can override
        return $c->_format_sitemap_date(time());
      }
    );

    $app->helper(
      _get_changefreq_for_path => sub {
        my ($c, $path) = @_;
        return 'weekly' if $path =~ m{^/(generals|books|specialties|covenants)};
        return 'monthly';
      }
    );

    $app->helper(
      _get_priority_for_path => sub {
        my ($c, $path) = @_;
        return '1.0' if $path eq '/';
        return '0.8' if $path =~ m{^/(generals|books|specialties|covenants)$};
        return '0.6' if $path =~ m{^/(generals|books|specialties|covenants)/};
        return '0.4';
      }
    );

    $app->helper(
      _format_sitemap_date => sub {
        my ($c, $timestamp) = @_;
        my ($sec, $min, $hour, $mday, $mon, $year) = gmtime($timestamp);
        return sprintf('%04d-%02d-%02d', $year + 1900, $mon + 1, $mday);
      }
    );

    $app->helper(
      _render_sitemap_xml => sub {
        my ($c, $urls) = @_;

        my $xml = qq{<?xml version="1.0" encoding="UTF-8"?>\n};
        $xml .=
          qq{<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">\n};

        for my $url (@$urls) {
          $xml .= qq{  <url>\n};
          $xml .= qq{    <loc>} . $c->_xml_escape($url->{loc}) . qq{</loc>\n};
          $xml .= qq{    <lastmod>$url->{lastmod}</lastmod>\n};
          $xml .= qq{    <changefreq>$url->{changefreq}</changefreq>\n};
          $xml .= qq{    <priority>$url->{priority}</priority>\n};
          $xml .= qq{  </url>\n};
        }

        $xml .= qq{</urlset>\n};
        return $xml;
      }
    );

    $app->helper(
      _xml_escape => sub {
        my ($c, $text) = @_;
        # Ensure UTF-8 encoding
        utf8::decode($text) unless utf8::is_utf8($text);
        
        # XML escape
        $text =~ s/&/&amp;/g;
        $text =~ s/</&lt;/g;
        $text =~ s/>/&gt;/g;
        $text =~ s/"/&quot;/g;
        $text =~ s/'/&apos;/g;
        return $text;
      }
    );

    $app->helper(
      generate_navigation => sub {
        my $c         = shift;
        my $structure = {};

        foreach my $path (sort keys %nav_items_by_path) {
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
    $self->log_debug(
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
