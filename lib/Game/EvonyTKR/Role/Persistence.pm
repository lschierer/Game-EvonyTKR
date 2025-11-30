package Game::EvonyTKR::Role::Persistence;
use v5.42.0;
use utf8::all;
use Mojo::Base -role, -signatures;
use Game::EvonyTKR::Service::Persistence;

# Lazy-load persistence service
has persistence => sub {
  Game::EvonyTKR::Service::Persistence->new;
};

has 'ascending_attribute_cache' => sub ($self) {
  return Game::EvonyTKR::Service::Cache->new(namespace => 'ascending_attributes__');
};

has 'general_cache' => sub ($self) {
  return Game::EvonyTKR::Service::Cache->new(namespace => 'generals:');
};

has 'builtin_book_cache' => sub ($self) {
  return Game::EvonyTKR::Service::Cache->new(namespace => 'builtin_books:');
};

has 'generic_book_cache' => sub ($self) {
  return Game::EvonyTKR::Service::Cache->new(namespace => 'generic_books:');
};

has 'covenant_cache' => sub ($self) {
  return Game::EvonyTKR::Service::Cache->new(namespace => 'covenants:');
};

has 'specialty_cache' => sub ($self) {
  return Game::EvonyTKR::Service::Cache->new(namespace => 'specialties:');
};

##############################################################################
# Convenience methods for job tracking
##############################################################################

sub mark_task_completed ($self, $task_name, $notes = undef) {
  return $self->persistence->mark_job_completed($task_name, $notes);
}

sub is_task_completed ($self, $task_name) {
  return $self->persistence->is_job_completed($task_name);
}

##############################################################################
# Data access methods - Generals
##############################################################################

sub add_general ($self, $general) {
  my $name = $general->name;

  # Write to persistence layer
  $self->persistence->store_general($name, $general->to_wire_hash());

  # Update memcached
  my $key = lc($self->normalize($name));
  $key =~ s/ /_/g;
  $self->general_cache->set($key, $general->to_wire_hash());

  return 1;
}

sub get_general ($self, $name) {
  require Game::EvonyTKR::Model::Factory;

  state $generals = {};

  return if (not length($name));

  $self->logger->debug("get_general called for: $name");

  my $normalized_name = lc($self->normalize($name)) // '';
  $normalized_name =~ s/ /_/g;
  return unless (length($normalized_name));

  # Check state cache first
  if (exists $generals->{$normalized_name}) {
    $self->logger->debug("Returning general $name from state cache");
    return $generals->{$normalized_name};
  }

  # Try memcached
  my $wire_data = $self->general_cache->get($normalized_name);

  # Fall back to persistence if not in memcached
  unless (defined($wire_data)) {
    $self->logger->debug("Not in memcached, checking persistence");
    $wire_data = $self->persistence->get_general($name);

    # Populate memcached for next time
    if (defined($wire_data)) {
      $self->general_cache->set($normalized_name, $wire_data);
    }
  }

  unless (defined($wire_data)) {
    $self->logger->warn("No data found for: $name");
    return;
  }

  my $general = Game::EvonyTKR::Model::Factory->build_from_wire('General', $wire_data);

  unless (defined($general)) {
    $self->logger->error("Factory failed to build general from wire_data");
    return;
  }

  $self->logger->debug("Successfully built general: " . $general->name);
  $generals->{$normalized_name} = $general;
  return $general;
}

sub get_generals ($self) {
  my $result = [];
  foreach my $name ($self->list_generals->@*) {
    my $g = $self->get_general($name);
    if($g && ref($g) && blessed($g) && $g->isa('Game::EvonyTKR::Model::General')){
      push @{ $result }, $g;
    }
  }

  if(scalar(@{$result}) != $self->persistence->count_generals()){
    $self->logger->error('unable to fetch all generals.')
  }
  return $result;
}

sub list_generals ($self) {
  require Mojo::File;
  require Mojo::Home;
  require List::UtilsBy;

  my $mh = Mojo::File->new(Mojo::Home->new->detect('Game::EvonyTKR')->to_string());
  my $collectionDir = $mh->child('share/collections/data');
  my $generalDir = $collectionDir->child('generals');
  my @suffixlist = ('.yaml', '.yml');
  my @files = $generalDir->list->grep(sub { $_ =~ /\.ya?ml$/ && -f -r $_ })
    ->sort->map(sub { return $_->basename(@suffixlist) })->each;
  my @returnlist = List::UtilsBy::uniq_by { lc($self->normalize($_)) } @files;
  return \@returnlist;
}

##############################################################################
# Data access methods - Builtin Books
##############################################################################

sub add_builtin_book ($self, $book) {
  my $name = $book->name;

  # Write to persistence layer
  $self->persistence->store_builtin_book($name, $book->to_wire_hash());

  # Update memcached
  my $key = $name =~ s/ /_/gr;
  $key = $self->normalize($key);
  $self->builtin_book_cache->set($key, $book->to_wire_hash());

  return 1;
}

sub get_builtin_book ($self, $name) {
  require Game::EvonyTKR::Model::Factory;

  state $builtin_books = {};

  $self->logger->debug("get_builtin_book called for: $name");

  my $key = $name =~ s/ /_/gr;
  $key = $self->normalize($key);

  # Check state cache
  if (exists $builtin_books->{$key}) {
    $self->logger->debug("Returning builtin book $name from state cache");
    return $builtin_books->{$key};
  }

  # Try memcached
  my $wire_data = $self->builtin_book_cache->get($key);

  # Fall back to persistence
  unless (defined($wire_data)) {
    $self->logger->debug("Not in memcached, checking persistence");
    $wire_data = $self->persistence->get_builtin_book($name);

    if (defined($wire_data)) {
      $self->builtin_book_cache->set($key, $wire_data);
    }
  }

  unless (defined($wire_data)) {
    $self->logger->warn("No wire_data found for key: $key");
    return;
  }

  my $book = Game::EvonyTKR::Model::Factory->build_from_wire('Book', $wire_data);

  unless (defined($book)) {
    $self->logger->error("Factory failed to build book from wire_data");
    return;
  }

  $self->logger->debug("Successfully built book: " . $book->name);
  $builtin_books->{$key} = $book;
  return $book;
}

sub list_builtin_books ($self) {
  require Mojo::File;
  require Mojo::Home;
  require List::UtilsBy;

  my $mh = Mojo::File->new(Mojo::Home->new->detect('Game::EvonyTKR')->to_string());
  my $collectionDir = $mh->child('share/collections/data');
  my $bbdir = $collectionDir->child('skill books');
  my @suffixlist = ('.yaml', '.yml');
  my @files = $bbdir->list->grep(sub { $_ =~ /\.ya?ml$/ && -f -r $_ })
    ->sort->map(sub { return $_->basename(@suffixlist) })->each;
  return [List::UtilsBy::uniq_by { lc($self->normalize($_)) } @files];
}

##############################################################################
# Data access methods - Generic Books
##############################################################################

sub add_generic_book ($self, $book) {
  my $name = $book->name;
  my $level = $book->level;

  # Write to persistence layer
  $self->persistence->store_generic_book($name, $level, $book->to_wire_hash());

  # Update memcached
  my $nn = $name =~ s/ /_/gr;
  $nn = $self->normalize($nn);
  my $key = sprintf('%s_level_%s', $nn, $level);
  $self->generic_book_cache->set($key, $book->to_wire_hash());

  return 1;
}

sub get_generic_book ($self, $name, $level) {
  require Game::EvonyTKR::Model::Factory;

  state $generic_books = {};

  my $key = $name =~ s/ /_/gr;
  $key = $self->normalize($key);
  $key = sprintf('%s_level_%s', $key, $level);

  # Check state cache
  if (exists $generic_books->{$key}) {
    $self->logger->debug("Returning generic book $name from state cache");
    return $generic_books->{$key};
  }

  # Try memcached
  my $wire_data = $self->generic_book_cache->get($key);

  # Fall back to persistence
  unless (defined($wire_data)) {
    $self->logger->debug("Not in memcached, checking persistence");
    $wire_data = $self->persistence->get_generic_book($name, $level);

    if (defined($wire_data)) {
      $self->generic_book_cache->set($key, $wire_data);
    }
  }

  return unless defined($wire_data);

  my $book = Game::EvonyTKR::Model::Factory->build_from_wire('Book', $wire_data);
  $generic_books->{$key} = $book if defined($book);
  return $book;
}

sub list_generic_books ($self) {
  require Mojo::File;
  require Mojo::Home;
  require List::UtilsBy;

  my $mh = Mojo::File->new(Mojo::Home->new->detect('Game::EvonyTKR')->to_string());
  my $collectionDir = $mh->child('share/collections/data');
  my $gbdir = $collectionDir->child('generic books');
  my @suffixlist = ('.yaml', '.yml');
  my @files = $gbdir->list->grep(sub { $_ =~ /\.ya?ml$/ && -f -r $_ })
    ->sort->map(sub { return $_->basename(@suffixlist) })->each;
  return [List::UtilsBy::uniq_by { lc($self->normalize($_)) } @files];
}

##############################################################################
# Data access methods - Covenants
##############################################################################

sub add_covenant ($self, $covenant) {
  my $name = $covenant->primary->name;

  # Write to persistence layer
  $self->persistence->store_covenant($name, $covenant->to_wire_hash());

  # Update memcached
  my $key = lc($self->normalize($name));
  $key =~ s/ /_/g;
  $self->covenant_cache->set($key, $covenant->to_wire_hash());

  return 1;
}

sub get_covenant ($self, $name) {
  require Game::EvonyTKR::Model::Factory;

  state $covenants = {};

  my $normalized_name = lc($self->normalize($name));
  $normalized_name =~ s/ /_/g;

  # Check state cache
  if (exists $covenants->{$normalized_name}) {
    $self->logger->debug("Returning covenant $name from state cache");
    return $covenants->{$normalized_name};
  }

  # Try memcached
  my $wire_data = $self->covenant_cache->get($normalized_name);

  # Fall back to persistence
  unless (defined($wire_data)) {
    $self->logger->debug("Not in memcached, checking persistence");
    $wire_data = $self->persistence->get_covenant($name);

    if (defined($wire_data)) {
      $self->covenant_cache->set($normalized_name, $wire_data);
    }
  }

  return unless defined($wire_data);

  my $covenant = Game::EvonyTKR::Model::Factory->build_from_wire('Covenant', $wire_data);
  $covenants->{$normalized_name} = $covenant if defined($covenant);
  return $covenant;
}

sub list_covenants ($self) {
  require Mojo::File;
  require Mojo::Home;
  require List::UtilsBy;

  my $mh = Mojo::File->new(Mojo::Home->new->detect('Game::EvonyTKR')->to_string());
  my $collectionDir = $mh->child('share/collections/data');
  my $cdir = $collectionDir->child('covenants');
  my @suffixlist = ('.yaml', '.yml');
  my @files = $cdir->list->grep(sub { $_ =~ /\.ya?ml$/ && -f -r $_ })
    ->sort->map(sub { return $_->basename(@suffixlist) })->each;
  return [List::UtilsBy::uniq_by { lc($self->normalize($_)) } @files];
}

##############################################################################
# Data access methods - Specialties
##############################################################################

sub add_specialty ($self, $specialty) {
  my $name = $specialty->name;

  # Write to persistence layer
  $self->persistence->store_specialty($name, $specialty->to_wire_hash());

  # Update memcached
  my $key = lc($self->normalize($name));
  $key =~ s/ /_/g;
  $self->specialty_cache->set($key, $specialty->to_wire_hash());

  return 1;
}

sub get_specialty ($self, $name) {
  require Game::EvonyTKR::Model::Factory;

  state $specialties = {};

  my $normalized_name = lc($self->normalize($name));
  $normalized_name =~ s/ /_/g;

  # Check state cache
  if (exists $specialties->{$normalized_name}) {
    $self->logger->debug("Returning specialty $name from state cache");
    return $specialties->{$normalized_name};
  }

  # Try memcached
  my $wire_data = $self->specialty_cache->get($normalized_name);

  # Fall back to persistence
  unless (defined($wire_data)) {
    $self->logger->debug("Not in memcached, checking persistence");
    $wire_data = $self->persistence->get_specialty($name);

    if (defined($wire_data)) {
      $self->specialty_cache->set($normalized_name, $wire_data);
    }
  }

  return unless defined($wire_data);

  my $specialty = Game::EvonyTKR::Model::Factory->build_from_wire('Specialty', $wire_data);
  $specialties->{$normalized_name} = $specialty if defined($specialty);
  return $specialty;
}

sub list_specialties ($self) {
  require Mojo::File;
  require Mojo::Home;
  require List::UtilsBy;

  my $mh = Mojo::File->new(Mojo::Home->new->detect('Game::EvonyTKR')->to_string());
  my $collectionDir = $mh->child('share/collections/data');
  my $sdir = $collectionDir->child('specialties');
  my @suffixlist = ('.yaml', '.yml');
  my @files = $sdir->list->grep(sub { $_ =~ /\.ya?ml$/ && -f -r $_ })
    ->sort->map(sub { return $_->basename(@suffixlist) })->each;
  return [List::UtilsBy::uniq_by { lc($self->normalize($_)) } @files];
}

##############################################################################
# Data access methods - Ascending Attributes
##############################################################################

sub add_ascending_attribute ($self, $ascendingAttribute) {
  my $name = $self->normalize($ascendingAttribute->general);

  # Write to persistence layer
  $self->persistence->store_ascending_attribute($name, $ascendingAttribute->to_wire_hash());

  # Update memcached
  my $key = lc($self->normalize($ascendingAttribute->general));
  $key =~ s/ /_/g;
  $self->ascending_attribute_cache->set($key, $ascendingAttribute->to_wire_hash());

  return 1;
}

sub get_ascending_attributes ($self, $name) {
  require Game::EvonyTKR::Model::Factory;

  state $AscendingAttributes = {};

  $self->logger->debug("get_ascending_attribute called for: $name");

  my $normalized_name = lc($self->normalize($name));
  $normalized_name =~ s/ /_/g;

  # Check state cache
  if (exists $AscendingAttributes->{$normalized_name}) {
    $self->logger->debug("Returning Ascending Attributes $name from state cache");
    return $AscendingAttributes->{$normalized_name};
  }

  # Try memcached
  my $wire_data = $self->ascending_attribute_cache->get($normalized_name);

  # Fall back to persistence
  unless (defined($wire_data)) {
    $self->logger->debug("Not in memcached, checking persistence");
    $wire_data = $self->persistence->get_ascending_attribute($name);

    if (defined($wire_data)) {
      $self->ascending_attribute_cache->set($normalized_name, $wire_data);
    }
  }

  return unless defined($wire_data);

  $self->logger->debug("Found wire_data, attempting to build AscendingAttributes");
  my $ascendingAttribute = Game::EvonyTKR::Model::Factory->build_from_wire('AscendingAttributes', $wire_data);

  unless (defined($ascendingAttribute)) {
    $self->logger->error("Factory failed to build AscendingAttributes from wire_data");
    return;
  }

  $self->logger->debug("Successfully built ascendingAttributes: " . $ascendingAttribute->general);
  $AscendingAttributes->{$normalized_name} = $ascendingAttribute;
  return $ascendingAttribute;
}

sub list_ascending_attributes ($self) {
  require Mojo::File;
  require Mojo::Home;
  require List::UtilsBy;

  my $mh = Mojo::File->new(Mojo::Home->new->detect('Game::EvonyTKR')->to_string());
  my $collectionDir = $mh->child('share/collections/data');
  my $AscendingAttributesDir = $collectionDir->child('ascending attributes');
  my @suffixlist = ('.yaml', '.yml');
  my @files = $AscendingAttributesDir->list->grep(sub { $_ =~ /\.ya?ml$/ && -f -r $_ })
    ->sort->map(sub { return $_->basename(@suffixlist) })->each;
  return [List::UtilsBy::uniq_by { lc($self->normalize($_)) } @files];
}

1;
__END__

=head1 NAME

Game::EvonyTKR::Role::Persistence - Role providing persistence interface

=head1 SYNOPSIS

  package Game::EvonyTKR::Controller::Generals;
  use Mojo::Base 'Mojolicious::Controller', -signatures;
  use Mojo::Base 'Game::EvonyTKR::Role::Persistence', -role;

  sub list ($self) {
    # Get from cache or persistence
    my $generals = $self->list_generals_with_cache($self->cache);

    $self->render(json => $generals);
  }

=head1 DESCRIPTION

This role provides a convenient interface to the persistence layer for
both Controllers and External (Minion) packages. It includes:

- Direct access to the persistence service
- Convenience methods for job tracking
- Cache-aware data retrieval (checks memcached first, falls back to SQLite)

=head1 METHODS

=head2 Job Tracking

=over 4

=item mark_task_completed($task_name, $notes)

Mark a task as completed in the current lifecycle.

=item is_task_completed($task_name)

Check if a task has completed in the current lifecycle.

=back

=head2 Data Retrieval

All data retrieval methods follow the pattern:

  get_X_with_cache($name, $cache)
  list_X_with_cache($cache)

These methods:
1. Check memcached if cache object provided
2. Fall back to SQLite persistence
3. Populate memcached for future requests

=cut
