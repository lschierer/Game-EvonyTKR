#!/usr/bin/env perl
use v5.42.0;
use FindBin;
use lib "$FindBin::Bin/../lib";

use Mojolicious::Commands;
use Game::EvonyTKR;

my $app = Game::EvonyTKR->new;
my $minion = $app->minion;

say "Checking for ghost workers...";

my $workers = $minion->backend->list_workers(0, 1000);
my $removed = 0;
my $alive = 0;

while (ref($workers) && blessed($workers) && (my $worker = $workers->next)) {
  my $pid = $worker->{pid};
  my $id = $worker->{id};
  my $host = $worker->{host};

  # Check if process exists
  my $exists = kill 0, $pid;

  unless ($exists) {
    say "Removing ghost worker $id (PID $pid on $host - process not found)";
    eval {
      $minion->backend->unregister_worker($id);
      $removed++;
    };
    if ($@) {
      warn "Failed to remove worker $id: $@";
    }
  } else {
    say "Worker $id is alive (PID $pid)";
    $alive++;
  }
}

say "\n=== Summary ===";
say "Removed: $removed ghost workers";
say "Active: $alive live workers";

exit 0;
