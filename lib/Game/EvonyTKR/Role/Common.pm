use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require JSON::PP;
require Mojo::JSON;
require X500::DN;
require X500::RDN;
require Unicode::CaseFold;

package Game::EvonyTKR::Role::Common {
  use Mojo::Base -role, -signatures;
  use Carp;
  use UUID               qw(uuid5);
  use Unicode::CaseFold  qw(fc);
  use Unicode::Normalize qw(NFKD);
  use List::AllUtils     qw(min uniq );

  has 'collection_dir' => sub {
    my $home = Mojo::Home->new->detect('Game::EvonyTKR');
    return $home->child('share/collections/data');
  };

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
      $k =~ s/ /_/g;
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

  has 'globalDN' => sub {
    return X500::DN->new(
      X500::RDN->new('OU' => 'EvonyTKR'),
      X500::RDN->new('OU' => 'Game'),
      X500::RDN->new('OU' => 'module'),
      X500::RDN->new('dc' => 'Perl'),
      X500::RDN->new('dc' => 'org'),
    );
  };

  has 'UUID5_base' => sub ($self) {
    my $ns_base = uuid5(dns => 'perl.org');
    return uuid5($ns_base, $self->globalDN->getX500String());
  };

  # Generic prerequisite checker for Minion jobs and controllers
  # Uses persistence layer to check job completion across hypnotoad restarts
  # $prereq_tasks: arrayref of task names that must be finished
  # Returns: 0 if all prereqs met, 1 if outstanding (controllers)
  #          calls retry/fail for Minion jobs
  sub are_prereqs_outstanding ($self, $minion, $prereq_tasks) {
    my $is_minion_job =
      $self->can('retry') && $self->can('fail') && $self->can('note');

    unless ($minion) {
      $self->logger->error(
        'must provide a minion process in which to search for jobs.');
      return 1;
    }

    if (scalar(@{$prereq_tasks}) == 0) {
      $self->logger->error('prereq tasks must be defined.');
      return 1;
    }

    # Get persistence service (assuming we have it via Role::Persistence)
    my $persistence;
    if ($self->can('persistence')) {
      $persistence = $self->persistence;
    }
    else {
      # Fallback: create a new instance
      require Game::EvonyTKR::Service::Persistence;
      $persistence = Game::EvonyTKR::Service::Persistence->new;
    }

    my $prereqs         = {};
    my @outstanding     = ();
    my @failed_tasks    = ();

    foreach my $prereq (@$prereq_tasks) {
      # Check persistence layer for completion
      my $is_completed = $persistence->is_job_completed($prereq);

      if ($is_completed) {
        $prereqs->{$prereq} = 'completed';
        next;
      }

      # Not completed in persistence - check Minion for active/failed jobs
      my $prereqPendingCount = $minion->jobs({
        tasks  => [$prereq],
        states => ['active', 'inactive'],
      })->total // 0;

      my $prereqFailedCount = $minion->jobs({
        tasks  => [$prereq],
        states => ['failed'],
      })->total // 0;

      if ($prereqFailedCount > 0) {
        push @failed_tasks, $prereq;
        $prereqs->{$prereq} = 'failed';
      }
      elsif ($prereqPendingCount > 0) {
        push @outstanding, $prereq;
        $prereqs->{$prereq} = 'pending';
      }
      else {
        # Not completed, not pending, not failed - not started yet
        push @outstanding, $prereq;
        $prereqs->{$prereq} = 'not_started';
      }
    }

    # Log prereq states
    $self->logger->debug(
      sprintf('prereqs are in states %s',
        Data::Printer::np($prereqs, multiline => 0))
    );

    # Handle failed prereqs
    if (@failed_tasks) {
      my $errmessage = sprintf('Cannot proceed: prerequisite job(s) failed: %s',
        join(', ', @failed_tasks));
      $self->logger->error($errmessage);
      return $is_minion_job ? $self->fail($errmessage) : 1;
    }

    # Handle outstanding prereqs
    if (@outstanding) {
      if ($is_minion_job) {
        # Note which prereqs are outstanding
        $self->note(outstanding_prereqs => \@outstanding);

        # Calculate retry delay based on number of outstanding prereqs
        my $delay = min(2 * scalar(@outstanding), 30);
        $self->logger->debug(sprintf(
          'Retrying with delay %s due to outstanding prereqs: %s',
          $delay, join(', ', @outstanding)
        ));
        return $self->retry({ delay => $delay });
      }
      else {
        return 1;    # Outstanding prereqs for controller
      }
    }

    # All prereqs met
    return 0;
  }
}
1;
__END__
