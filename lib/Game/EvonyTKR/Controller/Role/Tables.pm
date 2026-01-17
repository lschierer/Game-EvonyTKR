package Game::EvonyTKR::Controller::Role::Tables;
use v5.42.0;
use utf8::all;
use Moo::Role;

require UUID;
require MIME::Base64;
require Mojo::IOLoop;
use List::Util qw(min);

=head1 NAME

Game::EvonyTKR::Controller::Role::Tables - Shared SSE streaming infrastructure for table controllers

=head1 DESCRIPTION

This role provides common Server-Sent Events (SSE) streaming functionality used by both
the Pairs and Single Generals table controllers. It centralizes session management,
batch processing, and event streaming patterns.

=head1 ATTRIBUTES

=head2 table_batch_size

Standard batch size for processing items. Default: 50

=head2 table_loop_delay

Delay in seconds between batch processing loops. Default: 0.01 (10ms)

=head2 table_complete_flush_delay

Delay in seconds before sending complete event to ensure last batch is flushed. Default: 0.1 (100ms)

=cut

has table_batch_size => (
  is      => 'ro',
  default => sub {50},
);

has table_loop_delay => (
  is      => 'ro',
  default => sub {0.01},
);

has table_complete_flush_delay => (
  is      => 'ro',
  default => sub {0.1},
);

=head1 METHODS

=head2 setup_sse_headers

Sets up the HTTP headers required for Server-Sent Events streaming:
- content-type: text/event-stream
- content-encoding: utf-8
- Cache-Control: no-cache

  $c->setup_sse_headers();

=cut

sub setup_sse_headers ($self) {
  $self->res->headers->content_type('text/event-stream');
  $self->res->headers->content_encoding('utf-8');
  $self->res->headers->add('Cache-Control', 'no-cache');
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

=head2 encode_sse_payload

Encodes a data structure as Base64-encoded JSON for SSE transmission.
This ensures UTF-8 characters are preserved correctly in SSE events.

  my $encoded = $c->encode_sse_payload($data);

Arguments:
  $data - Data structure to encode (HashRef, ArrayRef, etc.)

Returns:
  $encoded - Base64-encoded JSON string

=cut

sub encode_sse_payload ($self, $data) {
  my $json_result = $self->encode($data);
  return MIME::Base64::encode_base64($json_result, '');
}

=head2 write_table_sse

Writes an SSE event, optionally with a callback.
Handles encoding of payload if it's a reference.

  $c->write_table_sse($event_type, $payload);
  $c->write_table_sse($event_type, $payload, sub { $c->finish });

Arguments:
  $event_type - String event type (e.g., 'row', 'pair', 'complete')
  $payload    - Data to send (will be encoded if it's a reference)
  $callback   - Optional callback to execute after write (CodeRef)

=cut

sub write_table_sse ($self, $event_type, $payload, $callback = undef) {
  my $encoded = ref($payload) ? $self->encode_sse_payload($payload) : $payload;

  if ($callback) {
    $self->write_sse({ type => $event_type, text => $encoded } => $callback);
  }
  else {
    $self->write_sse({ type => $event_type, text => $encoded });
  }
}

=head2 send_complete_event

Sends a 'complete' SSE event with a finish callback to close the connection.
Ensures the complete event is written before the connection closes.

  $c->send_complete_event($run_id, $total_items);
  $c->send_complete_event($run_id, $total_items, 'pairs');

Arguments:
  $run_id      - Run ID for this streaming session
  $total_items - Total number of items that were processed
  $item_type   - Optional descriptive name for items (default: 'items')

=cut

sub send_complete_event ($self, $run_id, $total_items, $item_type = 'items') {
  $self->logger->debug(sprintf(
    'All %d %s computed and flushed, sending complete event',
    $total_items, $item_type
  ));

  my $payload = $self->encode({ runId => $run_id });

  # Use callback to ensure complete event is written before closing
  $self->write_sse(
    { type => 'complete', text => $payload } => sub {
      $self->finish;
    }
  );

  $self->logger->debug('Complete event queued with finish callback');
}

=head2 validate_session_id

Validates that a session ID is present and non-empty.
If validation fails, sends a complete event and returns false.

  return unless $c->validate_session_id($session_id, $run_id);

Arguments:
  $session_id - Session ID to validate
  $run_id     - Run ID for error response

Returns:
  Boolean - true if valid, false if invalid (also sends complete event on failure)

=cut

sub validate_session_id ($self, $session_id, $run_id) {
  unless (defined($session_id) && length($session_id)) {
    $self->logger->error('Session ID must be present!');
    my $payload = $self->encode({ runId => 0+ $run_id });
    $self->write_table_sse('complete', $payload);
    return 0;
  }
  return 1;
}

=head2 create_batch_processor

Creates a batch processor subroutine for processing items in chunks.
This is a high-level abstraction over the recurring batch pattern.

  my $process_batch = $c->create_batch_processor({
    items        => \@items,
    run_id       => $run_id,
    process_item => sub ($item, $index) {
      # Process one item
      my $result = compute($item);
      $c->write_table_sse('row', $result);
    },
    on_complete  => sub {
      # Optional cleanup before complete event
    },
    batch_size   => 50,        # Optional, uses $c->table_batch_size if not set
    item_type    => 'pairs',   # Optional, for logging
  });

  # Execute immediately and set up recurring
  $process_batch->();
  my $recurring_id = Mojo::IOLoop->recurring($c->table_loop_delay => $process_batch);

Arguments (hashref):
  items        - ArrayRef of items to process (required)
  run_id       - Run ID for this session (required)
  process_item - CodeRef($item, $index) to process each item (required)
  on_complete  - Optional CodeRef to call before complete event
  batch_size   - Optional batch size (defaults to $self->table_batch_size)
  item_type    - Optional string for logging (defaults to 'items')

Returns:
  CodeRef - Subroutine to call for processing next batch

=cut

sub create_batch_processor ($self, $opts = {}) {
  my $items        = $opts->{items}      // [];
  my $batch_size   = $opts->{batch_size} // $self->table_batch_size;
  my $run_id       = $opts->{run_id};
  my $process_item = $opts->{process_item};    # Callback: sub($item, $index)
  my $on_complete  = $opts->{on_complete};     # Callback: sub()
  my $item_type    = $opts->{item_type} // 'items';

  my $current_idx   = 0;
  my $total_items   = scalar(@$items);
  my $complete_sent = 0;

  return sub {
    my $loop = shift;

    # Already completed
    return if $complete_sent;

    # Process next batch
    my $batch_end = min($current_idx + $batch_size, $total_items);

    $self->logger->debug(sprintf(
      'Processing %s %d-%d of %d',
      $item_type, $current_idx + 1,
      $batch_end, $total_items
    ));

    for my $i ($current_idx .. $batch_end - 1) {
      eval { $process_item->($items->[$i], $i); };
      if ($@) {
        $self->logger->error(
          sprintf('Error processing %s %d: %s', $item_type, $i, $@));
      }
    }

    $current_idx = $batch_end;

    # Check if complete
    if ($current_idx >= $total_items && !$complete_sent) {
      $complete_sent = 1;

      # Small delay to ensure last batch is written before complete event
      Mojo::IOLoop->timer(
        $self->table_complete_flush_delay => sub {
          $on_complete->() if $on_complete;
          $self->send_complete_event($run_id, $total_items, $item_type);
        }
      );
    }
  };
}

1;

=head1 AUTHOR

Game::EvonyTKR Development Team

=head1 LICENSE

This software is copyright (c) 2024.

=cut
