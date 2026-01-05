use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require X500::DN;
require X500::RDN;
require Unicode::CaseFold;

package Game::EvonyTKR::Role::Common {
  use Moo::Role;
  use Carp;
  use UUID               qw(uuid5);
  use Unicode::CaseFold  qw(fc);
  use Unicode::Normalize qw(NFKD);
  use List::AllUtils     qw(min max uniq none all );

  has collection_dir => (
    is => 'ro',
    lazy => 1,
    default => sub {
      my $home = Mojo::Home->new->detect('Game::EvonyTKR');
      return $home->child('share/collections/data');
    }
  );

  sub normalize ($self, $name) {
    my $dn = Encode::is_utf8($name) ? $name : Encode::decode_utf8($name);
    my $nn = fc(NFKD($dn));
    $nn =~ s/[’''‛`´]/'/g;
    $nn =~ s/[""‟]/"/g;      # Quotes
    return lc($nn);
  }

  # Generic hydrator:
  #  - $list_cb:   sub ($app) -> arrayref of expected names
  #  - $fetch_cb:  sub ($name) -> object or undef
  #  - $state_ref: hashref-like storage (use 'state %cache' in the caller)
  #  - returns hashref of hydrated objects keyed by normalized name
  sub _hydrate_from_list ($c, $app, $list_cb, $fetch_cb, $state_ref,
    $sig_state_ref) {

    my $_norm = sub ($c, $name) {
      my $k = lc($c->normalize($name) // '');
      return $k;
    };

    my $names = $list_cb->($app) // [];
    # Build a cheap signature of "what should exist"
    my $sig = join "\0", sort map { $_norm->($c, $_) } @$names;

    # If signature unchanged, we’re fully up to date
    return $state_ref if defined $$sig_state_ref && $$sig_state_ref eq $sig;

    # Otherwise, only fetch missing ones
    for my $name (@$names) {
      my $key = $_norm->($c, $name);

      next if exists $state_ref->{$key};
      if (my $obj = $fetch_cb->($name)) {
        $state_ref->{$key} = $obj;
      }
    }

    # If we now cover the full set, bump signature
    my $have_all =
      (@$names == scalar grep { exists $state_ref->{ $_norm->($c, $_) } }
        @$names);
    $$sig_state_ref = $sig if $have_all;

    return $state_ref;
  }

  has globalDN => (
    is => 'ro',
    lazy => 1,
    default => sub {
      return X500::DN->new(
        X500::RDN->new('OU' => 'EvonyTKR'),
        X500::RDN->new('OU' => 'Game'),
        X500::RDN->new('OU' => 'module'),
        X500::RDN->new('dc' => 'Perl'),
        X500::RDN->new('dc' => 'org'),
      );
    }
  );

  has UUID5_base => (
    is => 'ro',
    lazy => 1,
    default => sub {
      my ($self) = @_;
      my $ns_base = uuid5(dns => 'perl.org');
      return uuid5($ns_base, $self->globalDN->getX500String());
    }
  );

  # Helper to retry operations that may encounter transient SQLite locking
  sub _minion_retry ($self, $operation, $max_attempts = 3) {
    my $attempt = 0;
    while ($attempt < $max_attempts) {
      $attempt++;
      my $result = eval { $operation->() };

      if ($@) {
        my $error = $@;
        # Check if it's a transient database error
        if ($error =~
          /database is locked|database disk image is malformed|SQLITE_BUSY/i) {
          if ($attempt < $max_attempts) {
            $self->log_debug(sprintf(
'Minion operation failed with transient error (attempt %d/%d): %s',
              $attempt, $max_attempts, $error
            ));
            # Exponential backoff: 100ms, 200ms, 400ms
            select(undef, undef, undef, 0.1 * (2**($attempt - 1)));
            next;
          }
          # Max attempts reached
          $self->log_error(sprintf(
            'Minion operation failed after %d attempts: %s',
            $max_attempts, $error
          ));
          die $error;
        }
        # Not a transient error, rethrow immediately
        die $error;
      }

      # Success
      return $result;
    }
  }

  # Generic prerequisite checker for Minion jobs and controllers
  # Uses DynamoDB work unit tracker to check completion
  # $prereq_tasks: arrayref of work unit names that must be complete
  # Returns: 0 if all prereqs met, 1 if outstanding (controllers)
  #          calls retry/fail for Minion jobs
  sub are_prereqs_outstanding ($self, $minion, $prereq_tasks) {
    my $is_minion_job =
      $self->can('retry') && $self->can('fail') && $self->can('note');

    unless ($minion) {
      $self->log_error(
        'must provide a minion process in which to search for jobs.');
      return 1;
    }

    if (scalar(@{$prereq_tasks}) == 0) {
      $self->log_error('prereq tasks must be defined.');
      return 1;
    }

    # Get work unit tracker
    require Game::EvonyTKR::WorkUnit::Tracker;
    my $tracker = Game::EvonyTKR::WorkUnit::Tracker->new(
      persistence => $self->can('persistence')
      ? $self->persistence
      : do {
        require Game::EvonyTKR::Service::Persistence;
        Game::EvonyTKR::Service::Persistence->new;
      }
    );

    my $prereqs      = {};
    my @outstanding  = ();
    my @failed_tasks = ();

    foreach my $prereq (@$prereq_tasks) {
      # Check DDB for work unit completion
      my $is_completed = $tracker->is_complete($prereq);

      if ($is_completed) {
        $prereqs->{$prereq} = 'completed';
        next;
      }

# For controllers, we don't need to distinguish between pending/failed/not-started
# They just show a wait page regardless
# Only check Minion for failed jobs if caller is a Minion job (to fail fast)
      if ($is_minion_job) {
     # Check if prereq failed - wrap in retry logic for transient SQLite locking
        my $prereqFailedCount = $self->_minion_retry(sub {
          $minion->jobs({
            tasks  => [$prereq],
            states => ['failed'],
          })->total // 0;
        });

        if ($prereqFailedCount > 0) {
          push @failed_tasks, $prereq;
          $prereqs->{$prereq} = 'failed';
          next;
        }
      }

# Not completed and (for controllers) not checking Minion, or (for jobs) not failed
      push @outstanding, $prereq;
      $prereqs->{$prereq} = 'pending';
    }

    # Log prereq states
    $self->log_debug(
      sprintf('prereqs are in states %s',
        Data::Printer::np($prereqs, multiline => 0))
    );

    # Handle failed prereqs
    if (@failed_tasks) {
      my $errmessage = sprintf('Cannot proceed: prerequisite job(s) failed: %s',
        join(', ', @failed_tasks));
      $self->log_error($errmessage);
      return $is_minion_job ? $self->fail($errmessage) : 1;
    }

    # Handle outstanding prereqs
    if (@outstanding) {
      if ($is_minion_job) {
        # Note which prereqs are outstanding
        $self->note(outstanding_prereqs => \@outstanding);

        # Calculate retry delay based on number of outstanding prereqs
        my $delay =
          max(min(5 * scalar(@outstanding), 30), $self->standard_delay);
        $self->log_debug(sprintf(
          'Retrying with delay %s due to outstanding prereqs: %s',
          $delay, join(', ', @outstanding)
        ));
        return $self->retry({ delay => $delay });
      }
      else {
        return 1;    # Outstanding prereqs for controller
      }
    }

    # All prereqs met - clear any previous outstanding_prereqs note
    if ($is_minion_job) {
      $self->note(outstanding_prereqs => undef);
    }

    return 0;
  }

  sub normalizeSpecialtyLevels ($self, @specialties) {
    my @normalized = @specialties;

    # Ensure we have exactly 4 specialties
    if (scalar @normalized != 4) {
      $self->logger->warn("Expected 4 specialties, got "
          . scalar @normalized
          . ". Padding with defaults.");
      while (scalar @normalized < 4) {
        push @normalized, 'gold';
      }
      @normalized = @normalized[0 .. 3] if scalar @normalized > 4;
    }

    # Validate and normalize each specialty level
    foreach my $index (0 .. 3) {
      if (none { $_ eq $normalized[$index] } $self->SpecialtyLevelValues->@*) {
        $self->logger->warn(
"Invalid specialty level at index $index: $normalized[$index], using default"
        );
        $normalized[$index] = 'gold';
      }
    }

    # Apply the specialty 4 rule
    my $all_gold = all { $_ eq 'gold' } @normalized[0 .. 2];

    if ($all_gold && $normalized[3] eq 'none') {
      $self->logger->warn(
"When specialties 1-3 are all gold, specialty 4 cannot be 'none'. Setting to gold."
      );
      $normalized[3] = 'gold';
    }

    if (!$all_gold && $normalized[3] ne 'none') {
      $self->logger->warn(
"When specialties 1-3 are not all gold, specialty 4 must be 'none'. Setting to none."
      );
      $normalized[3] = 'none';
    }

    return @normalized;
  }

}
1;
__END__
