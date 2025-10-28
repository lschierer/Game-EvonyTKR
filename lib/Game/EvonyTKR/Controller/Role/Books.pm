use v5.42.0;
use utf8::all;
use File::FindLib 'lib';
require Log::Log4perl::Level;
require Mojo::File;

package Game::EvonyTKR::Controller::Role::Books {
  use Mojo::Base -role,                         -signatures;
  use Mojo::Base 'Game::EvonyTKR::Role::Cache', -role;
  use List::AllUtils qw(uniq);
  use Sereal::Encoder;
  use Sereal::Decoder;
  use Carp;

  our $namespace = 'books__';

  our $decoder = Sereal::Decoder->new();

  our $encoder = Sereal::Encoder->new({
    canonical          => 1,
    no_shared_hashkeys => 1,
    freeze_callbacks   => 1,
    freeze_unknown     => 1,
  });

  sub create_book_cache ($self) {
    $self->logger->debug(sprintf(
      '%s log level is %s',
      __PACKAGE__, Log::Log4perl::Level::to_level($self->logger->level())
    ));
    state $cache;
    $cache = $self->create_cache({
      namespace => $namespace,
    })
      unless (defined $cache);
    return $cache;
  }

  sub add_builtin_book ($self, $item, $store) {
    my $key = $item->name =~ s/ /_/gr;
    $key = $self->normalize($key);
    $self->add_book($key, $item, $store);
  }

  sub add_generic_book ($self, $item, $store) {
    my $nn = $item->name =~ s/ /_/gr;
    $nn = $self->normalize($nn);
    my $key = sprintf('%s_level_%s', $nn, $item->level);
    $self->add_book($key, $item, $store);
  }

  sub add_book ($self, $name, $item, $store) {
    my $key = $name =~ s/ /_/gr;
    my $ei  = $encoder->encode($item);
    my $ar  = $self->add_item($key, $ei, $store);
    $self->logger->debug(
      "attempting to add key '$key' for name '$name' was '$ar'");
    return $ar;
  }

  sub get_builtin_book ($self, $name, $store) {
    state $builtin_books = {};
    if (exists $builtin_books->{ $self->normalize($name) }) {
      $self->logger->debug("Returning builtin_books $name from local cache");
      return $builtin_books->{ $self->normalize($name) };
    }
    my $key = $name =~ s/ /_/gr;
    $key = $self->normalize($key);
    my $ei = $self->get_value($key, $store);
    if (defined($ei) && length($ei)) {
      $decoder->decode($ei, my $item);
      unless (blessed($item)
        && $item->isa('Game::EvonyTKR::Model::Book')
        && $item->name eq $name) {
        $self->logger->error(sprintf(
          'retrieved unexpected item: expected: '
            . '"%s" ne recieved: "%s" - a %s',
          $name, $item->name, blessed($item)
        ));
        return;
      }
      $builtin_books->{ $self->normalize($item->name) } = $item;
      return $item;
    }
    else {
      $self->logger->warn(sprintf(
        'unable to retrieve encoded item for %s with key %s',
        $name, $key
      ));
    }
  }

  sub get_generic_book ($self, $name, $level, $store) {
    state $generic_books = {};
    if (exists $generic_books->{ $self->normalize($name) }) {
      if (exists $generic_books->{ $self->normalize($name) }->{$level}) {
        $self->logger->debug("Returning generic book $name from local cache");
        return $generic_books->{ $self->normalize($name) }->{$level};
      }
    }
    my $key = $name =~ s/ /_/gr;
    $key = $self->normalize($key);
    $key = sprintf('%s_level_%s', $key, $level);
    my $ei = $self->get_value($key, $store);
    if (defined($ei) && length($ei)) {
      $decoder->decode($ei, my $item);
      unless (blessed($item)
        && $item->isa('Game::EvonyTKR::Model::Book')
        && $item->name eq $name
        && $item->level == $level) {
        $self->logger->error(sprintf(
          'retrieved unexpected item: expected: '
            . '"%s" level %s ne recieved: "%s" level %s - a %s',
          $name, $level, $item->name, $item->level, blessed($item)
        ));
        return;
      }
      $generic_books->{ $self->normalize($name) }->{$level} = $item;
      return $item;
    }
    else {
      $self->logger->warn(sprintf(
        'unable to retrieve encoded item for %s with key %s',
        $name, $key
      ));
    }
  }

  sub get_all_books ($self, $store) {
    my $all      = $self->get_all_items($store);
    my @keys     = keys $all->%*;
    my $keyCount = scalar @keys;
    if ($keyCount == 0) {
      $self->logger->warn(sprintf(
        'get_all_books helper found %s items', scalar keys $all->%*));
      my $all_keys = $self->get_value('_all_keys', $store) // '';
      my @keys     = sort { "$a" cmp "$b" }
        List::UtilsBy::uniq_by {"$_"} grep {length} split ',', $all_keys;
      if (length($all_keys) == 0) {
        $self->logger->warn('No keys present!');
      }
      else {
        $self->logger->warn(sprintf(
          'get_all_books available keys: %s',
          join ', ', map {"'$_'"} @keys
        ));
      }
    }

    my $result = {};
    foreach my $key (keys $all->%*) {

      unless (defined($key) && length($key)) {
        $self->logger->error("bogus key in get_all_items result!!");
        next;
      }
      my $ev = $all->{$key};
      unless (defined($ev) && length($ev)) {
        $self->logger->error("key '$key' points at undef!!");
        next;
      }
      $decoder->decode($ev, my $item);
      my $result->{total_books}++;
      my $name = $key =~ s/_/ /rg;
      if ($key =~ /_level_/) {
        $result->{ $self->normalize($name) }->{ $item->level } = $item;
      }
      else {
        $result->{ $self->normalize($name) } = $item;
      }
    }
    return $result;
  }

  sub list_generic_books ($self, $app) {
    unless (defined($app)) {
      $self->logger->logcroak('$app must be defined');
    }
    my $collectionDir =
      Mojo::File->new($app->config('distDir'))->child('collections/data/');
    my $gbDir      = $collectionDir->child('generic books');
    my @suffixlist = ('.yaml', '.yml');
    my @gglist     = $gbDir->list->grep(sub { $_ =~ /\.ya?ml$/ && -f -r $_ })
      ->sort->map(sub { return $_->basename(@suffixlist) })->each;
    my @returnlist =
      List::UtilsBy::uniq_by { lc($self->normalize($_)) } @gglist;
    return \@returnlist;
  }

  sub list_builtin_books ($self, $app) {
    my $collectionDir =
      Mojo::File->new($app->config('distDir'))->child('collections/data/');
    my $gbDir      = $collectionDir->child('skill books');
    my @suffixlist = ('.yaml', '.yml');
    my @gglist     = $gbDir->list->grep(sub { $_ =~ /\.ya?ml$/ && -f -r $_ })
      ->sort->map(sub { return $_->basename(@suffixlist) })->each;
    my @returnlist =
      List::UtilsBy::uniq_by { lc($self->normalize($_)) } @gglist;
    return \@returnlist;
  }
}
1;
__END__
