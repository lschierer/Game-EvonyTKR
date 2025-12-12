use v5.40;
use experimental qw(class);
use utf8::all;

package Game::EvonyTKR::Service::RedisPersistence {
  use Mojo::Base -base, -signatures;
  with 'Game::EvonyTKR::Role::JSON';
  use Mojo::Redis;
  use Carp;
  use Time::HiRes 'time';

  has 'redis_url' => sub {
    return $ENV{REDIS_URL} || 'redis://127.0.0.1:6379/0';
  };

  has 'redis' => sub ($self) {
    my $redis = Mojo::Redis->new($self->redis_url);
    return $redis;
  };

  has 'db' => sub ($self) {
    return $self->redis->db;
  };

  has encoder => sub ($self) {
    return $self->JSON;
  };

  has 'lifecycle_id' => sub ($self) {
    my $stored = $self->get_metadata('lifecycle_id');
    unless ($stored) {
      $stored = time . '_' . $$;
      $self->set_metadata('lifecycle_id', $stored);
    }
    return $stored;
  };

  # Metadata operations
  sub get_metadata ($self, $key) {
    my $result = $self->db->hget('metadata', $key);
    return $result;
  }

  sub set_metadata ($self, $key, $value) {
    return $self->db->hset('metadata', $key, $value);
  }

  # Job completion tracking
  sub mark_job_completed ($self, $job_name) {
    my $key = "job_completed:$job_name";
    return $self->db->setex($key, 86400, time);    # Expire after 24 hours
  }

  sub is_job_completed ($self, $job_name) {
    my $key = "job_completed:$job_name";
    return $self->db->exists($key);
  }

  # Generic storage operations
  sub store_data ($self, $table, $key, $data) {
    my $json = $self->encode($data);
    return $self->db->hset($table, $key, $json);
  }

  sub get_data ($self, $table, $key) {
    my $json = $self->db->hget($table, $key);
    return unless $json;
    return $self->decode($json);
  }

  sub get_all_data ($self, $table) {
    my $hash   = $self->db->hgetall($table);
    my $result = {};
    for my $key (keys %$hash) {
      $result->{$key} = $self->decode($hash->{$key});
    }
    return $result;
  }

  sub delete_data ($self, $table, $key) {
    return $self->db->hdel($table, $key);
  }

  sub clear_table ($self, $table) {
    return $self->db->del($table);
  }

  # List operations
  sub store_list ($self, $key, $items) {
    $self->db->del($key);    # Clear existing
    return unless @$items;
    my @json_items = map { $self->encode($_) } @$items;
    return $self->db->lpush($key, @json_items);
  }

  sub get_list ($self, $key) {
    my @json_items = $self->db->lrange($key, 0, -1);
    return [map { $self->decode($_) } @json_items];
  }

  # Specific data type methods (matching SQLite interface)

  # Generals
  sub store_general ($self, $key, $general_data) {
    return $self->store_data('generals', $key, $general_data);
  }

  sub get_general ($self, $name) {
    return $self->get_data('generals', $name);
  }

  sub get_all_generals ($self) {
    return $self->get_all_data('generals');
  }

  # Ascending Attributes
  sub store_ascending_attribute ($self, $name_or_data, $data = undef) {
    # Handle both old (name, data) and new (data) signatures
    if (defined $data) {
      # Old signature: store_ascending_attribute($name, $data)
      $data->{name} = $name_or_data unless exists $data->{name};
      return $self->store_data('ascending_attributes', $name_or_data, $data);
    }
    else {
      # New signature: store_ascending_attribute($data)
      my $attr_data = $name_or_data;
      my $name      = $attr_data->{name} or croak "Attribute must have name";
      return $self->store_data('ascending_attributes', $name, $attr_data);
    }
  }

  sub get_ascending_attribute ($self, $name) {
    return $self->get_data('ascending_attributes', $name);
  }

  sub get_all_ascending_attributes ($self) {
    return $self->get_all_data('ascending_attributes');
  }

  # Books
  sub store_book ($self, $book_data) {
    my $name = $book_data->{name} or croak "Book must have name";
    my $table =
      $book_data->{type} eq 'builtin' ? 'builtin_books' : 'generic_books';
    return $self->store_data($table, $name, $book_data);
  }

  sub get_book ($self, $name, $type = 'generic') {
    my $table = $type eq 'builtin' ? 'builtin_books' : 'generic_books';
    return $self->get_data($table, $name);
  }

  sub get_all_books ($self, $type = 'generic') {
    my $table = $type eq 'builtin' ? 'builtin_books' : 'generic_books';
    return $self->get_all_data($table);
  }

  # Legacy method names for compatibility
  sub get_generic_book ($self, $name, $level = undef) {
    return $self->get_book($name, 'generic');
  }

  sub get_builtin_book ($self, $name, $level = undef) {
    return $self->get_book($name, 'builtin');
  }

  sub get_all_generic_books ($self) {
    return $self->get_all_books('generic');
  }

  sub get_all_builtin_books ($self) {
    return $self->get_all_books('builtin');
  }

  sub store_generic_book ($self, $name, $level, $data) {
    $data->{type} = 'generic';
    return $self->store_book($data);
  }

  sub store_builtin_book ($self, $name, $data) {
    $data->{type} = 'builtin';
    return $self->store_book($data);
  }

  # Glossary terms
  sub store_glossary_term ($self, $name, $data) {
    return $self->store_data('glossary_terms', $name, $data);
  }

  sub get_glossary_term ($self, $name) {
    return $self->get_data('glossary_terms', $name);
  }

  sub get_all_glossary_terms ($self) {
    return $self->get_all_data('glossary_terms');
  }

  # Pairs - additional methods
  sub store_pair ($self, $key, $data) {
    return $self->store_data('pairs_individual', $key, $data);
  }

  sub get_pair ($self, $key) {
    return $self->get_data('pairs_individual', $key);
  }

  sub get_all_pair_types ($self) {
    my @keys  = $self->db->keys('pairs:*');
    my @types = map { s/^pairs://; $_ } @keys;
    return \@types;
  }

  sub list_pairs_by_type ($self, $type) {
    return $self->get_pairs_by_type($type);
  }

  # Count methods
  sub count_generals ($self) {
    return $self->db->hlen('generals');
  }

  sub count_ascending_attributes ($self) {
    return $self->db->hlen('ascending_attributes');
  }

  sub count_specialties ($self) {
    return $self->db->hlen('specialties');
  }

  sub count_generic_books ($self) {
    return $self->db->hlen('generic_books');
  }

  sub count_builtin_books ($self) {
    return $self->db->hlen('builtin_books');
  }

  sub count_glossary_terms ($self) {
    return $self->db->hlen('glossary_terms');
  }

  sub count_covenants ($self) {
    return $self->db->hlen('covenants');
  }

  # Specialties
  sub store_specialty ($self, $name_or_data, $data = undef) {
    # Handle both old (name, data) and new (data) signatures
    if (defined $data) {
      # Old signature: store_specialty($name, $data)
      $data->{name} = $name_or_data unless exists $data->{name};
      return $self->store_data('specialties', $name_or_data, $data);
    }
    else {
      # New signature: store_specialty($data)
      my $specialty_data = $name_or_data;
      my $name = $specialty_data->{name} or croak "Specialty must have name";
      return $self->store_data('specialties', $name, $specialty_data);
    }
  }

  sub get_specialty ($self, $name) {
    return $self->get_data('specialties', $name);
  }

  sub get_all_specialties ($self) {
    return $self->get_all_data('specialties');
  }

  # Covenants
  sub store_covenant ($self, $covenant_data) {
    my $name = $covenant_data->{name} or croak "Covenant must have name";
    return $self->store_data('covenants', $name, $covenant_data);
  }

  sub get_covenant ($self, $name) {
    return $self->get_data('covenants', $name);
  }

  sub get_all_covenants ($self) {
    return $self->get_all_data('covenants');
  }

  # Conflicts
  sub store_conflict ($self, $g1, $g2, $conflicts) {
    # Ensure consistent ordering
    ($g1, $g2) = sort ($g1, $g2);
    my $key = "$g1:$g2";
    return $self->db->hset('general_conflicts', $key, $conflicts ? 1 : 0);
  }

  sub get_conflict ($self, $g1, $g2) {
    ($g1, $g2) = sort ($g1, $g2);
    my $key    = "$g1:$g2";
    my $result = $self->db->hget('general_conflicts', $key);
    return unless defined $result;
    return $result ? 1 : 0;
  }

  # Pairs
  sub store_pairs ($self, $type, $pairs) {
    my $key = "pairs:$type";
    return $self->store_list($key, $pairs);
  }

  sub get_pairs_by_type ($self, $type) {
    my $key = "pairs:$type";
    return $self->get_list($key);
  }

  # Clear all data (for rebuilds)
  sub clear_all_data ($self) {
    my @keys = qw(
      metadata generals ascending_attributes builtin_books generic_books
      specialties covenants general_conflicts glossary_terms pairs_individual
    );

    # Clear pair keys
    my @pair_keys = $self->db->keys('pairs:*');
    push @keys, @pair_keys;

    # Clear job completion keys
    my @job_keys = $self->db->keys('job_completed:*');
    push @keys, @job_keys;

    return $self->db->del(@keys) if @keys;
  }
}

1;
__END__
