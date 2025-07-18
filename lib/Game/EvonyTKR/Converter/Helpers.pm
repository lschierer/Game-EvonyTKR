use v5.42.0;
use utf8::all;
use experimental qw(class);
use File::FindLib 'lib';
require Data::Printer;
require HTML::TreeBuilder;
require HTTP::Tiny;
require IO::Socket::IP;    # for HTTP::Tiny;
require IO::Socket::SSL;
require Path::Tiny;
require Readonly;

#require Game::EvonyTKR;    # for dist_dir to work
require Game::EvonyTKR::Shared::Parser;
require Game::EvonyTKR::Model::Buff;
require Game::EvonyTKR::Model::Buff::Value;

class Game::EvonyTKR::Converter::Helpers :
  isa(Game::EvonyTKR::Shared::Constants) {
  use List::AllUtils qw( first all any none );
  use Carp;
  use namespace::autoclean;

  # PODNAME: Game::EvonyTKR::Converter::Helpers

  field $debug : param //= 0;

  method find_next_ul_after_element ($element) {
    # Get the container (go up until we find one with ul elements)
    my $container = $element;
    while ($container && !$container->look_down('_tag' => 'ul')) {
      $container = $container->parent;
      last if !$container || $container->tag eq 'body';
    }

    return undef unless $container;

    # Find all ul elements in the container
    my @all_uls = $container->look_down('_tag' => 'ul');

    # Find the first ul that comes after our h3 in document order
    my $h3_pos = $self->get_element_position($element);

    for my $ul (@all_uls) {
      my $ul_pos = $self->get_element_position($ul);
      if ($ul_pos gt $h3_pos) {    # String comparison instead of numeric
        return $ul;
      }
    }

    return undef;
  }
  method find_all_p_after_element ($element) {
      # Find the container (as you already do)
      my $container = $element;
      while ($container && !$container->look_down('_tag' => 'p')) {
          $container = $container->parent;
          last if !$container || $container->tag eq 'body';
      }

      return () unless $container;

      my $start_pos = $self->get_element_position($element);
      my @all_ps = $container->look_down('_tag' => 'p');

      my @after = grep {
          my $p_pos = $self->get_element_position($_);
          $p_pos gt $start_pos;
      } @all_ps;

      return @after;
  }

  method find_next_p_after_element ($element) {
    # Get the container (go up until we find one with p elements)
    my $container = $element;
    while ($container && !$container->look_down('_tag' => 'p')) {
      $container = $container->parent;
      last if !$container || $container->tag eq 'body';
    }

    return undef unless $container;

    # Find all p elements in the container
    my @all_ps = $container->look_down('_tag' => 'p');

    # Find the first p that comes after our h3 in document order
    my $h3_pos = $self->get_element_position($element);

    for my $p (@all_ps) {
      my $p_pos = $self->get_element_position($p);
      if ($p_pos gt $h3_pos) {    # String comparison instead of numeric
        return $p;
      }
    }

    return undef;
  }

  method find_next_table_after_element ($element) {
    # Get the container (go up until we find one with table elements)
    my $container = $element;
    while ($container && !$container->look_down('_tag' => 'table')) {
      $container = $container->parent;
      last if !$container || $container->tag eq 'body';
    }

    return undef unless $container;

    # Find all table elements in the container
    my @all_ps = $container->look_down('_tag' => 'table');

    # Find the first table that comes after our h3 in document order
    my $h3_pos = $self->get_element_position($element);

    for my $p (@all_ps) {
      my $p_pos = $self->get_element_position($p);
      if ($p_pos gt $h3_pos) {    # String comparison instead of numeric
        return $p;
      }
    }

    return undef;
  }

  method get_element_position ($element) {

    # Create a unique path-based position instead of arithmetic
    my @path;
    my $current = $element;

    while ($current) {
      my $parent = $current->parent;
      last unless $parent;

      # Count position among siblings
      my $sibling_pos = 0;
      for my $sibling ($parent->content_list) {
        last           if $sibling == $current;
        $sibling_pos++ if ref $sibling;           # Only count element nodes
      }

      unshift @path, $sibling_pos;
      $current = $parent;
    }

    # Convert path to a comparable string (lexicographic ordering)
    return join('.', map { sprintf("%04d", $_) } @path);

  }

  method extract_ul_details ($ul) {

    my @items;
    my @lis = $ul->look_down('_tag' => 'li');

    for my $li (@lis) {
      push @items, $li->as_trimmed_text;
    }

    return \@items;
  }
}
1;

__END__
