package Game::EvonyTKR::Service::DynamoDBPersistence;
use v5.42.0;
use utf8::all;
use Mojo::Base -base,                           -signatures;
use Mojo::Base 'Game::EvonyTKR::Role::JSON',    -role;
use Mojo::Base 'Game::EvonyTKR::Role::Common',  -role;
use Mojo::Base 'Game::EvonyTKR::Role::Logging', -role;

use Carp;
use Time::HiRes 'time';

has 'config';        # Persistence config from NotYAMLConfig
has 'aws_config';    # AWS config from NotYAMLConfig

has 'table_name' => sub ($self) {
  my $config = $self->config || {};
  return $config->{dynamodb_table} || $ENV{DYNAMODB_TABLE} || 'evonytkr-data';
};

has 'region' => sub ($self) {
  my $aws_config = $self->aws_config     || {};
  my $region     = $aws_config->{region} || $ENV{AWS_REGION} || 'us-east-1';

  # Debug: Log which source provided the region
  if ($aws_config->{region}) {
    warn sprintf("[DynamoDB] Using region from config: %s\n", $region);
  }
  elsif ($ENV{AWS_REGION}) {
    warn sprintf("[DynamoDB] Using region from ENV: %s\n", $region);
  }
  else {
    warn sprintf("[DynamoDB] Using fallback region: %s (check config!)\n",
      $region);
  }

  return $region;
};

has 'dynamodb' => sub ($self) {
  require Paws;
  return Paws->service('DynamoDB', region => $self->region);
};

has 'lifecycle_id' => sub ($self) {
  my $stored = $self->get_metadata('lifecycle_id');
  unless ($stored) {
    $stored = time . '_' . $$;
    $self->set_metadata('lifecycle_id', $stored);
  }
  return $stored;
};

# Core DynamoDB operations
sub _put_item ($self, $pk, $sk, $data, $entity_type = undef) {
  $entity_type //= $pk;

  $self->log_info(sprintf(
    "[DynamoDB] _put_item called: pk=%s, sk=%s, table=%s",
    $pk, $sk, $self->table_name
  ));

  my $item = {
    pk          => { S => $pk },
    sk          => { S => $sk },
    entity_type => { S => $entity_type },
    data        => { S => $self->encode($data) },
    updated_at  => { N => sprintf("%.6f", time()) }
  };

  eval {
    my $result = $self->dynamodb->PutItem(
      TableName => $self->table_name,
      Item      => $item
    );
    1;
  } or do {
    my $error = $@ || 'unknown error';
    $self->log_error(
      sprintf(
        "[DynamoDB] PutItem FAILED for pk=%s, sk=%s: %s",
        $pk, $sk, $error
      )
    );
    warn sprintf("[DynamoDB] PutItem FAILED for pk=%s, sk=%s: %s\n",
      $pk, $sk, $error);
    return 0;
  };

  $self->log_info(
    sprintf("[DynamoDB] PutItem SUCCESS for pk=%s, sk=%s", $pk, $sk));
  return 1;
}

sub _get_item ($self, $pk, $sk) {
  my $result = eval {
    $self->dynamodb->GetItem(
      TableName => $self->table_name,
      Key       => {
        pk => { S => $pk },
        sk => { S => $sk }
      }
    );
  };

  if ($@) {
    $self->log_error(
      sprintf("[DynamoDB] GetItem failed for pk=%s, sk=%s: %s\n", $pk, $sk, $@)
    );
    return;
  }

  unless ($result && $result->Item) {
    $self->log_debug(
      sprintf("[DynamoDB] No Item returned for pk=%s, sk=%s\n", $pk, $sk));
    return;
  }

  # Paws returns a Paws::DynamoDB::AttributeMap object
  # Access the underlying hash via ->Map
  my $item_hash =
    ref($result->Item) eq 'HASH' ? $result->Item : $result->Item->Map;

  my $data_str = $item_hash->{data}->{S};
  unless (defined $data_str && length($data_str) > 0) {
    $self->log_error(
      sprintf("[DynamoDB] Empty/undef data for pk=%s, sk=%s\n", $pk, $sk));
    return;
  }

  my $decoded = eval { $self->decode($data_str) };
  if ($@) {
    $self->log_error(sprintf(
      "[DynamoDB] JSON decode failed for pk=%s, sk=%s: %s\n",
      $pk, $sk, $@
    ));
    $self->log_error(sprintf("[DynamoDB] Raw data (first 200 chars): %s\n",
      substr($data_str, 0, 200)));
    return;
  }

  return $decoded;
}

sub _query_raw_items ($self, $pk, $sk_prefix = undef) {
  my $key_condition = 'pk = :pk';
  my $attr_values   = { ':pk' => { S => $pk } };

  if ($sk_prefix) {
    $key_condition .= ' AND begins_with(sk, :sk_prefix)';
    $attr_values->{':sk_prefix'} = { S => $sk_prefix };
  }

  my $result = eval {
    $self->dynamodb->Query(
      TableName                 => $self->table_name,
      KeyConditionExpression    => $key_condition,
      ExpressionAttributeValues => $attr_values
    );
  };

  if ($@) {
    $self->log_error(
      sprintf("[DynamoDB] Query failed for pk=%s: %s\n", $pk, $@));
    return [];
  }

  return [] unless $result && $result->Items;

  # Return raw items (as hashes) with Paws::DynamoDB::AttributeMap converted
  my @items;
  for my $item (@{ $result->Items }) {
    my $item_hash = ref($item) eq 'HASH' ? $item : $item->Map;
    push @items, $item_hash;
  }

  return \@items;
}

sub _query_items ($self, $pk, $sk_prefix = undef) {
  my $raw_items = $self->_query_raw_items($pk, $sk_prefix);

  my @decoded_items;
  for my $item_hash (@$raw_items) {
    my $data_str = $item_hash->{data}->{S};
    if (defined $data_str && length($data_str) > 0) {
      push @decoded_items, $self->decode($data_str);
    }
  }

  return \@decoded_items;
}

# Metadata operations
sub get_metadata ($self, $key) {
  return $self->_get_item('metadata', $key);
}

sub set_metadata ($self, $key, $value) {
  return $self->_put_item('metadata', $key, $value);
}

sub get_current_prebuild_run_id ($self) {
  my $metadata = $self->get_metadata('current_prebuild_run_id');
  return $metadata ? $metadata->{run_id} : undef;
}

# Job completion tracking
sub mark_job_completed ($self, $job_name, $run_id = undef) {
  my $sk = $run_id ? "${run_id}:${job_name}" : $job_name;
  return $self->_put_item('job_completed', $sk,
    { completed_at => time(), run_id => $run_id // 'legacy' });
}

sub is_job_completed ($self, $job_name, $run_id = undef) {
  my $sk = $run_id ? "${run_id}:${job_name}" : $job_name;

  $self->log_debug(
    sprintf("[DynamoDB] Checking job_completed: pk=job_completed, sk=%s", $sk));
  my $result = $self->_get_item('job_completed', $sk);

  # If run-scoped lookup failed, try legacy key for backward compatibility
  if (!defined $result && $run_id) {
    $self->log_debug(
      sprintf("[DynamoDB] Run-scoped key not found, trying legacy: sk=%s",
        $job_name)
    );
    $result = $self->_get_item('job_completed', $job_name);
  }

  my $found = defined $result ? 1 : 0;
  $self->log_debug(
    sprintf(
      "[DynamoDB] Job %s completion check: %s",
      $job_name, $found ? 'FOUND' : 'NOT FOUND'
    )
  );
  return $found;
}

# Harvest (clean up) job completion records from previous runs
sub harvest_job_completions ($self, $current_run_id) {
  unless ($current_run_id) {
    $self->log_warn("[DynamoDB] Cannot harvest without current_run_id");
    return 0;
  }

  my $harvested = 0;

  # Query all job_completed items (raw format with sk, pk, data fields)
  my $items = $self->_query_raw_items('job_completed');

  foreach my $item (@$items) {
    my $sk       = $item->{sk}->{S};
    my $data_str = $item->{data}->{S}                // '';
    my $data     = eval { $self->decode($data_str) } // {};

    # Skip items from current run
    next if $sk =~ /^\Q${current_run_id}\E:/;

    # Skip if this is current run_id in data
    next if $data->{run_id} && $data->{run_id} eq $current_run_id;

    # Delete stale record
    eval {
      $self->dynamodb->DeleteItem(
        TableName => $self->table_name,
        Key       => {
          pk => { S => 'job_completed' },
          sk => { S => $sk }
        }
      );
      $harvested++;
      $self->log_debug("[DynamoDB] Harvested stale job_completed: $sk");
    };

    if ($@) {
      $self->log_warn("[DynamoDB] Failed to harvest $sk: $@");
    }
  }

  $self->log_info(
    "[DynamoDB] Harvested $harvested stale job_completed records");
  return $harvested;
}

# Data versioning - track which git-commit the data was built from
sub get_data_version ($self) {
  return $self->get_metadata('data_version');
}

sub set_data_version ($self, $version) {
  return $self->set_metadata('data_version', $version);
}

# Generic storage operations
sub store_data ($self, $table, $key, $data) {
  return $self->_put_item($table, $key, $data);
}

sub get_data ($self, $table, $key) {
  return $self->_get_item($table, $key);
}

sub get_all_data ($self, $table) {
  my $items  = $self->_query_items($table);
  my $result = {};
  for my $item (@$items) {
    # Assume the key is stored in the data or derive from sk
    my $key = $item->{name} || $item->{id} || $item->{term} || 'unknown';
    $result->{$key} = $item;
  }
  return $result;
}

# Specific data type methods
sub store_general ($self, $key, $general_data) {
  return $self->_put_item('generals', $key, $general_data);
}

sub get_general ($self, $name) {
  return $self->_get_item('generals', $name);
}

sub get_all_generals ($self) {
  return $self->get_all_data('generals');
}

sub count_generals ($self) {
  my $items = $self->_query_items('generals');
  return scalar @$items;
}

# Ascending Attributes
sub store_ascending_attribute ($self, $key, $data) {
  return $self->_put_item('ascending_attributes', $key, $data);
}

sub get_ascending_attribute ($self, $name) {
  return $self->_get_item('ascending_attributes', $name);
}

sub get_all_ascending_attributes ($self) {
  return $self->get_all_data('ascending_attributes');
}

sub count_ascending_attributes ($self) {
  my $items = $self->_query_items('ascending_attributes');
  return scalar @$items;
}

# Books
sub store_book ($self, $key, $book_data) {
  my $name  = $book_data->{name} or croak "Book must have name";
  my $type  = $book_data->{type} || 'generic';
  my $table = $type eq 'builtin' ? 'builtin_books' : 'generic_books';
  return $self->_put_item($table, $key, $book_data);
}

sub get_book ($self, $name, $type = 'generic') {
  my $table = $type eq 'builtin' ? 'builtin_books' : 'generic_books';
  return $self->_get_item($table, $name);
}

sub get_all_books ($self, $type = 'generic') {
  my $table = $type eq 'builtin' ? 'builtin_books' : 'generic_books';
  return $self->get_all_data($table);
}

# Legacy book methods
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

sub store_generic_book ($self, $key, $data) {
  $data->{type} = 'generic';
  return $self->store_book($key, $data);
}

sub store_builtin_book ($self, $key, $data) {
  $data->{type} = 'builtin';
  return $self->store_book($key, $data);
}

sub count_generic_books ($self) {
  my $items = $self->_query_items('generic_books');
  return scalar @$items;
}

sub count_builtin_books ($self) {
  my $items = $self->_query_items('builtin_books');
  return scalar @$items;
}

# Specialties
sub store_specialty ($self, $key, $data) {
  return $self->_put_item('specialties', $key, $data);
}

sub get_specialty ($self, $name) {
  return $self->_get_item('specialties', $name);
}

sub get_all_specialties ($self) {
  return $self->get_all_data('specialties');
}

sub count_specialties ($self) {
  my $items = $self->_query_items('specialties');
  return scalar @$items;
}

# Covenants
sub store_covenant ($self, $key, $covenant_data) {
  return $self->_put_item('covenants', $key, $covenant_data);
}

sub get_covenant ($self, $name) {
  return $self->_get_item('covenants', $name);
}

sub get_all_covenants ($self) {
  return $self->get_all_data('covenants');
}

sub count_covenants ($self) {
  my $items = $self->_query_items('covenants');
  return scalar @$items;
}

# Glossary terms
sub store_glossary_term ($self, $name, $data) {
  return $self->_put_item('glossary_terms', $name, $data);
}

sub get_glossary_term ($self, $name) {
  return $self->_get_item('glossary_terms', $name);
}

sub get_all_glossary_terms ($self) {
  return $self->get_all_data('glossary_terms');
}

sub list_glossary_terms ($self) {
  return $self->get_all_glossary_terms();
}

sub count_glossary_terms ($self) {
  my $items = $self->_query_items('glossary_terms');
  return scalar @$items;
}

# Conflicts (simplified for DynamoDB)
sub store_conflict ($self, $g1, $g2, $conflicts) {
  ($g1, $g2) = sort ($g1, $g2);
  my $key = "$g1:$g2";
  return $self->_put_item('general_conflicts', $key,
    { conflicts => $conflicts ? 1 : 0 });
}

# Batch write conflicts - much more efficient for bulk updates
sub store_conflicts_batch ($self, $conflicts_hash) {
  my @items;

  # Convert hash structure to list of items
  foreach my $g1 (keys %$conflicts_hash) {
    foreach my $g2 (keys %{ $conflicts_hash->{$g1} }) {
      my ($sorted_g1, $sorted_g2) = sort ($g1, $g2);
      my $sk        = "$sorted_g1:$sorted_g2";
      my $conflicts = $conflicts_hash->{$g1}{$g2} ? 1 : 0;

      push @items,
        {
        pk          => { S => 'general_conflicts' },
        sk          => { S => $sk },
        entity_type => { S => 'general_conflicts' },
        data        => { S => $self->encode({ conflicts => $conflicts }) },
        updated_at  => { N => sprintf("%.6f", time()) }
        };
    }
  }

  my $total_items = scalar(@items);
  return 0 unless $total_items;

  $self->log_info(
    sprintf("[DynamoDB] Batch writing %d conflict items", $total_items));

  my $written = 0;
  my $failed  = 0;

  # DynamoDB BatchWriteItem limit is 25 items per request
  while (@items) {
    my @batch = splice(@items, 0, 25);

    my $request_items =
      { $self->table_name => [map { { PutRequest => { Item => $_ } } } @batch]
      };

    eval {
      my $result =
        $self->dynamodb->BatchWriteItem(RequestItems => $request_items);

      # Handle unprocessed items (throttling)
      if ($result->UnprocessedItems && %{ $result->UnprocessedItems }) {
        my $unprocessed =
          $result->UnprocessedItems->{ $self->table_name } || [];
        my $unprocessed_count = scalar(@$unprocessed);
        $self->log_warn(sprintf(
          "[DynamoDB] %d items unprocessed due to throttling, retrying...",
          $unprocessed_count));

        # Re-add unprocessed items to the queue
        push @items, map { $_->{PutRequest}->{Item} } @$unprocessed;
        $failed += $unprocessed_count;
      }

      $written += scalar(@batch);
      1;
    } or do {
      my $error = $@ || 'unknown error';
      $self->log_error(sprintf("[DynamoDB] BatchWriteItem failed: %s", $error));
      $failed += scalar(@batch);
    };

    # Small delay between batches to avoid throttling
    select(undef, undef, undef, 0.1) if @items;
  }

  $self->log_info(sprintf(
    "[DynamoDB] Batch write complete: %d/%d items written (%d failed)",
    $written, $total_items, $failed
  ));

  return $written;
}

sub get_conflict ($self, $g1, $g2) {
  ($g1, $g2) = sort ($g1, $g2);
  my $key    = "$g1:$g2";
  my $result = $self->_get_item('general_conflicts', $key);
  return unless $result;
  return $result->{conflicts} ? 1 : 0;
}

sub load_all_conflicts ($self) {
  $self->log_debug(
    sprintf("[DynamoDB] Loading conflicts using Query on pk=general_conflicts")
  );

  # Use _query_raw_items which efficiently queries by pk
  my $items = $self->_query_raw_items('general_conflicts');

  my $item_count = ref($items) eq 'ARRAY' ? scalar(@$items) : 0;
  $self->log_info(
    sprintf("[DynamoDB] Query returned %d conflict items", $item_count));

  if ($item_count == 0) {
    $self->log_warn(
      "[DynamoDB] No conflict items found - may not be written yet");
    return {};
  }

  my $conflicts     = {};
  my $decode_errors = 0;

  for my $item_hash (@$items) {
    # Extract sk and validate
    my $sk = $item_hash->{sk}->{S} or next;
    my ($g1, $g2) = split ':', $sk;
    next unless ($g1 && $g2);

    # Extract and decode data
    my $data_str = $item_hash->{data}->{S};
    unless (defined $data_str && length($data_str) > 0) {
      $self->log_warn(sprintf("[DynamoDB] Empty data for conflict %s", $sk));
      next;
    }

    my $data = eval { $self->decode($data_str) };
    if ($@) {
      $self->log_error(
        sprintf(
          "[DynamoDB] Failed to decode conflict data for %s: %s", $sk, $@
        )
      );
      $decode_errors++;
      next;
    }

    my $has_conflict = $data->{conflicts} ? 1 : 0;

    $conflicts->{$g1}{$g2} = $has_conflict;
    $conflicts->{$g2}{$g1} = $has_conflict;
  }

  my $general_count = scalar(keys %$conflicts);
  $self->log_info(sprintf(
"[DynamoDB] Loaded %d conflict items covering %d generals (%d decode errors)",
    $item_count, $general_count, $decode_errors
  ));

  return $conflicts;
}

# Pairs (simplified - store as JSON)
sub store_pairs ($self, $type, $pairs) {
  return $self->_put_item('pairs', $type, { pairs => $pairs });
}

sub get_pairs_by_type ($self, $type) {
  my $result = $self->_get_item('pairs', $type);
  return $result ? $result->{pairs} : [];
}

sub store_pair ($self, $key, $data) {
  return $self->_put_item('pairs_individual', $key, $data);
}

sub get_pair ($self, $key) {
  return $self->_get_item('pairs_individual', $key);
}

sub get_all_pair_types ($self) {
  # Query pairs_individual to find all unique types
  my $items = eval { $self->_query_raw_items('pairs_individual') };

  if ($@) {
    $self->log_error("Failed to query pairs_individual for types: $@");
    return [];
  }

  my %types_seen;
  foreach my $item_hash (@$items) {
    my $sk = $item_hash->{sk}->{S} or next;
    # sk format is "type/primary/secondary"
    if ($sk =~ /^([^\/]+)\//) {
      $types_seen{$1} = 1;
    }
  }

  return [sort keys %types_seen];
}

sub list_pairs_by_type ($self, $type) {
  # Query pairs_individual table for all pairs of this type
  # Format: pk='pairs_individual', sk starts with 'type/'
  my $items = eval {
    $self->_query_raw_items('pairs_individual', "$type/");
  };

  if ($@) {
    $self->log_error("Failed to query pairs_individual: $@");
    return [];
  }

  my @type_pairs;
  foreach my $item_hash (@$items) {
    my $data_str = $item_hash->{data}->{S};
    next unless $data_str;

    my $wire_pair = eval { $self->decode($data_str) };
    if ($@) {
      my $sk = $item_hash->{sk}->{S} || 'unknown';
      $self->log_warn("Failed to decode pair $sk: $@");
      next;
    }

    push @type_pairs, $wire_pair;
  }

  return \@type_pairs;
}

# Clear all data (for rebuilds) - WARNING: This will be expensive in DynamoDB
sub clear_all_data ($self) {
  # In production, you might want to recreate the table instead
  # This is a simplified version that would need pagination for large datasets
  warn "clear_all_data is expensive in DynamoDB - consider table recreation";

  # For now, just clear metadata to indicate a rebuild is needed
  $self->set_metadata('cleared_at', time());
  return 1;
}

1;
__END__

=head1 NAME

Game::EvonyTKR::Service::DynamoDBPersistence - DynamoDB persistence backend

=head1 DESCRIPTION

Single-table DynamoDB implementation optimized for cost-effectiveness.
Uses partition key (pk) for entity type and sort key (sk) for entity name.

=head1 ENVIRONMENT VARIABLES

=over 4

=item * DYNAMODB_TABLE - Table name (default: evonytkr-data)

=item * AWS_REGION - AWS region (default: us-east-1)

=back

=cut
