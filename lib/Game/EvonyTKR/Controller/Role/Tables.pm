package Game::EvonyTKR::Controller::Role::Tables;
use v5.42.0;
use utf8::all;
use Moo::Role;
use Future::AsyncAwait;

require UUID;
require MIME::Base64;

=head1 NAME

Game::EvonyTKR::Controller::Role::Tables - Shared SSE streaming infrastructure for table controllers (Thunderhorse/PAGI version)

=head1 DESCRIPTION

This role provides common Server-Sent Events (SSE) streaming functionality used by both
the Pairs and Single Generals table controllers. It uses Thunderhorse::SSE (via $ctx->sse)
for async streaming. Routes using this role must be registered with action => 'sse.get'.

=head1 ATTRIBUTES

=head2 table_batch_size

Standard batch size for processing items. Default: 50

=head2 table_keepalive_interval

Keepalive interval in seconds for proxy compatibility. Default: 25

=cut

has table_batch_size => (
  is      => 'ro',
  default => sub {50},
);

has table_keepalive_interval => (
  is      => 'ro',
  default => sub {25},
);

=head1 METHODS

=head2 create_sse

Gets the Thunderhorse::SSE object from the context.
The context must have been created with an SSE scope (action => 'sse.get').

  my $sse = $self->create_sse($ctx);

Arguments:
  $ctx - The request context (Thunderhorse::Context with SSE scope)

Returns:
  Thunderhorse::SSE object (extends PAGI::SSE)

=cut

sub create_sse ($self, $ctx) {
  # Thunderhorse::Context provides a lazy 'sse' field that creates
  # Thunderhorse::SSE automatically when the scope type is 'sse'
  return $ctx->sse;
}

=head2 generate_table_session_id

Generates a unique session ID using UUID5 + UUID7 pattern.

  my $session_id = $c->generate_table_session_id(\@requested_items);

Arguments:
  $requested_items - ArrayRef of item identifiers (optional, defaults to [])

Returns:
  $session_id - UUID string

=cut

sub generate_table_session_id ($self, $requested_items = []) {
  my $uidseed = join(', ', @$requested_items) . ' ' . UUID::uuid7();
  $self->logger->debug("uidseed is '$uidseed'");

  my $session_id = UUID::uuid5($self->UUID5_base, $uidseed);
  $self->logger->debug("final session_id is '$session_id'");

  return $session_id;
}

=head2 send_row_event

Sends an SSE event with the given data.

  await $self->send_row_event($sse, $data);
  await $self->send_row_event($sse, $data, 'pair');  # custom event type

Arguments:
  $sse        - PAGI::SSE object
  $data       - Data to send (will be JSON-encoded)
  $event_type - Optional event type (default: 'row')

=cut

async sub send_row_event ($self, $sse, $data, $event_type = 'row') {
  $self->logger->debug("send_row_event: sending '$event_type' event");
  await $sse->send_event(
    event => $event_type,
    data  => $data,
  );
  $self->logger->debug('send_row_event: event sent successfully');
}

=head2 send_complete_event

Sends a 'complete' SSE event to signal end of stream.

  await $self->send_complete_event($sse, $run_id, $total_items, $item_type);

Arguments:
  $sse         - PAGI::SSE object
  $run_id      - Run ID for this streaming session
  $total_items - Total number of items that were processed
  $item_type   - Optional descriptive name for items (default: 'items')

=cut

async sub send_complete_event ($self, $sse, $run_id, $total_items, $item_type = 'items') {
  $self->logger->debug(sprintf(
    'All %d %s computed, sending complete event',
    $total_items, $item_type
  ));

  await $sse->send_event(
    event => 'complete',
    data  => { runId => 0+ $run_id },
  );

  $self->logger->debug('Complete event sent');
}

=head2 validate_session_id

Validates that a session ID is present and non-empty.
If validation fails, sends a complete event and returns false.

  return unless await $c->validate_session_id($sse, $session_id, $run_id);

Arguments:
  $sse        - PAGI::SSE object
  $session_id - Session ID to validate
  $run_id     - Run ID for error response

Returns:
  Boolean - true if valid, false if invalid (also sends complete event on failure)

=cut

async sub validate_session_id ($self, $sse, $session_id, $run_id) {
  unless (defined($session_id) && length($session_id)) {
    $self->logger->error('Session ID must be present!');
    await $self->send_complete_event($sse, $run_id, 0);
    return 0;
  }
  return 1;
}

=head2 process_items_streaming

Processes items in batches and streams results via SSE.

  await $self->process_items_streaming($sse, {
    items        => \@items,
    run_id       => $run_id,
    process_item => async sub ($item, $index) {
      # Process one item, return result hashref
      return { ... };
    },
    item_type    => 'generals',  # Optional, for logging
    event_type   => 'pair',      # Optional, SSE event type (default: 'row')
  });

Arguments (hashref):
  items        - ArrayRef of items to process (required)
  run_id       - Run ID for this session (required)
  process_item - Async CodeRef($item, $index) returning result hashref (required)
  item_type    - Optional string for logging (defaults to 'items')
  event_type   - Optional SSE event type (defaults to 'row')

=cut

async sub process_items_streaming ($self, $sse, $opts = {}) {
  my $items        = $opts->{items}        // [];
  my $run_id       = $opts->{run_id};
  my $process_item = $opts->{process_item};
  my $item_type    = $opts->{item_type}    // 'items';
  my $event_type   = $opts->{event_type}   // 'row';
  my $batch_size   = $opts->{batch_size}   // $self->table_batch_size;

  my $total_items = scalar(@$items);
  my $processed   = 0;

  $self->logger->debug(sprintf(
    'Starting to process %d %s in batches of %d (event_type: %s)',
    $total_items, $item_type, $batch_size, $event_type
  ));

  # Process items - PAGI::SSE handles connection state
  for my $i (0 .. $#$items) {
    last if $sse->is_closed;

    my $item = $items->[$i];

    eval {
      my $result = await $process_item->($item, $i);

      if ($result) {
        await $self->send_row_event($sse, $result, $event_type);
        $processed++;
      }
    };
    if ($@) {
      $self->logger->error(sprintf(
        'Error processing %s %d: %s', $item_type, $i, $@
      ));
    }

    # Log batch progress
    if (($i + 1) % $batch_size == 0) {
      $self->logger->debug(sprintf(
        'Processed %s %d-%d of %d',
        $item_type, $i - $batch_size + 2, $i + 1, $total_items
      ));
    }
  }

  # Send complete event unless connection closed
  unless ($sse->is_closed) {
    await $self->send_complete_event($sse, $run_id, $processed, $item_type);
  }

  return $processed;
}

=head2 run_sse_stream

High-level method to run an SSE stream with standard setup.

  await $self->run_sse_stream($ctx, async sub ($sse) {
    # Your streaming logic here
    await $sse->send_event(event => 'row', data => { ... });
  });

This handles:
- Creating the SSE object from context
- Starting the stream
- Enabling keepalive
- Running the provided callback
- Waiting for disconnect
- Marking context as consumed (critical for Thunderhorse)

=cut

async sub run_sse_stream ($self, $ctx, $callback) {
  my $sse = $self->create_sse($ctx);

  # Mark context as consumed IMMEDIATELY to prevent Thunderhorse from trying
  # to send HTTP response if ANY error occurs during SSE handling.
  # This is critical for SSE endpoints.
  $ctx->consume;

  # Start SSE stream
  await $sse->start;

  # Enable keepalive for proxy compatibility
  await $sse->keepalive($self->table_keepalive_interval);

  # Register cleanup callback
  $sse->on_close(sub {
    my ($sse, $reason) = @_;
    $self->logger->debug("SSE connection closed: $reason");
  });

  # Run the user's streaming logic
  await $callback->($sse);

  # Wait for client disconnect (if not already closed)
  await $sse->run unless $sse->is_closed;

  return;
}

1;

=head1 AUTHOR

Game::EvonyTKR Development Team

=head1 LICENSE

This software is copyright (c) 2024-2026.

=cut
