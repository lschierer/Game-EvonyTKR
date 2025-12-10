use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require HTTP::Tiny;
require HTML::LinkExtor;
require URI;

package LinkChecker::Command;
use Mojo::Base -base,                           -signatures;
use Mojo::Base 'Game::EvonyTKR::Role::Logging', -role;
use List::AllUtils qw( any none );
use namespace::autoclean;
use Carp;
our $VERSION = 'v0.30.0';

has debug => 0;
has delay => 0.1;    # Delay between requests in seconds (100ms default)

has 'startUrl'       => '';
has checked_urls     => sub { {} };
has urls_to_check    => sub { [] };
has 'start_hostname' => '';

BEGIN {
  Game::EvonyTKR::Role::Logging::get_logger(__PACKAGE__);
}

sub init {
  my $self = shift;
  unless (defined($self->startUrl) && length($self->startUrl)) {
    croak('startUrl is required');
  }
  push @{ $self->urls_to_check }, $self->startUrl;
  $self->start_hostname(URI->new($self->startUrl)->host);
}

sub execute {
  my $self = shift;
  $self->log_info(sprintf('Starting checking at "%s"', $self->startUrl));

  # Process queue until empty
  while (@{ $self->urls_to_check }) {
    my $url = shift @{ $self->urls_to_check };    # FIFO: take from front
    $self->check_url($url);

    # Add delay between requests to avoid overwhelming the server
    if ($self->delay && @{ $self->urls_to_check }) {
      select(undef, undef, undef, $self->delay);
    }
  }

  $self->log_info("Url Checking complete");

  # Update children statuses now that all URLs are processed
  $self->update_children_statuses();

  foreach my $checked (sort keys %{ $self->checked_urls }) {
    if ($self->checked_urls->{$checked}->{status} !~ /^2/) {
      say "Found Broken Link to $checked";
    }
    elsif (exists $self->checked_urls->{$checked}->{children}) {
      foreach
        my $child (sort keys %{ $self->checked_urls->{$checked}->{children} }) {
        if ($self->checked_urls->{$checked}->{children}->{$child} !~ /^2/) {
          say "Page $checked contains Broken Link to $child.";
        }
      }
    }
  }
}

sub check_url ($self, $url, $recurse = 1) {
  # Remove fragment for checking purposes
  my $uri = URI->new($url);

  if (exists $self->checked_urls->{$url}) {
    $self->log_debug("$url has already been checked. Skipping.");
    return $self->checked_urls->{$url}->{status};
  }

  $self->log_info("Checking $url");

  my $http = HTTP::Tiny->new(
    timeout => 30,
    agent   => 'EvonyTKR-LinkChecker/1.0',
  );

  # Retry up to 3 times for transient failures
  my $response;
  my $max_retries = 3;
  for my $attempt (1 .. $max_retries) {
    $response = $http->get($url);

    # Success or permanent failure (4xx) - don't retry
    if ($response->{success}
      || ($response->{status} >= 400 && $response->{status} < 500)) {
      last;
    }

    # Transient failure (5xx, timeout, etc) - retry with backoff
    if ($attempt < $max_retries) {
      my $backoff = 0.5 * $attempt;    # 0.5s, 1s, 1.5s
      $self->log_debug(
"Attempt $attempt failed with $response->{status}, retrying after ${backoff}s"
      );
      select(undef, undef, undef, $backoff);
    }
  }

  $self->checked_urls->{$url}->{status} = $response->{status};

  unless ($response->{success}) {
    $self->log_warn(sprintf(
      'Detected Broken page %s via status %s - %s.',
      $url, $response->{status}, $response->{reason}
    ));
    return $response->{status};
  }

  $self->log_debug(
    sprintf('Page %s returned status %s.', $url, $response->{status}));

  if ($response->{content} && length($response->{content}) && $recurse) {
    my $content = $response->{content};
    if (length($uri->fragment)) {
      my $frag           = $uri->fragment;
      my $fragment_found = 0;

      # Check for id attributes on any element
      if ($content =~ /<[^>]+id=[\'\"]$frag[\'\"][^>]*>/i) {
        $fragment_found = 1;
      }
      # Check for name attributes (older anchor style)
      elsif ($content =~ /<a[^>]+name=[\'\"]$frag[\'\"][^>]*>/i) {
        $fragment_found = 1;
      }

      unless ($fragment_found) {
        $self->log_warn("Fragment #$frag NOT found on page $url");
        $self->checked_urls->{$url}->{status} =
          404;    # Override the successful page status
        return 404;
      }
    }
    if ($uri->path !~ /\.(css|js|png|jpg|gif|pdf)$/i)
    {    # cannot find links to check in these files.
      my $extractor = HTML::LinkExtor->new(undef, $url);
      $extractor->parse($response->{content});
      my @links = $extractor->links;

      my $hostname = $uri->host;
      $self->log_info("extracted hostname $hostname");

      foreach my $link_array (sort @links) {
        my ($tag, %attrs) = @$link_array;
        my $href = $attrs{href} || $attrs{src};

        if ($href) {
          my $abs_uri = URI->new($href)->abs($url);
          $self->log_debug("found url to check: $abs_uri");

          unless ($abs_uri->scheme eq 'mailto') {    # Avoid email links
            my $abs_url_str = $abs_uri->as_string;

            # Check if URL is already processed
            unless (exists $self->checked_urls->{$abs_url_str}) {
              # Check if already in queue to avoid duplicates
              unless (grep { $_ eq $abs_url_str } @{ $self->urls_to_check }) {
              # Only add to queue for recursive checking if it's the same domain
                if ($self->start_hostname eq $abs_uri->host) {
                  push @{ $self->urls_to_check },
                    $abs_url_str;   # Add to end of queue for recursive checking
                  $self->log_debug(sprintf(
                    'Added internal URL "%s" to queue for recursive checking',
                    $abs_url_str));
                }
                else {
                  # External URL - check it directly but don't recurse
                  $self->check_single_url($abs_url_str);
                  $self->log_debug(sprintf(
                    'Checked external URL "%s" directly (no recursion).',
                    $abs_url_str));
                }
              }
            }

            # Mark the relationship for later status update
            $self->checked_urls->{$url}->{children}->{$href} = 'pending';
          }
        }
      }
    }

  }

  return $response->{status};
}

sub check_single_url ($self, $url) {
  # This method checks a single URL without recursion (for external links)
  if (exists $self->checked_urls->{$url}) {
    $self->log_debug("$url has already been checked. Skipping.");
    return $self->checked_urls->{$url}->{status};
  }

  $self->log_info("Checking external URL $url (no recursion)");

  my $http = HTTP::Tiny->new(
    timeout => 30,
    agent   => 'EvonyTKR-LinkChecker/1.0',
  );

  # Retry up to 3 times for transient failures
  my $response;
  my $max_retries = 3;
  for my $attempt (1 .. $max_retries) {
    $response = $http->get($url);

    # Success or permanent failure (4xx) - don't retry
    if ($response->{success}
      || ($response->{status} >= 400 && $response->{status} < 500)) {
      last;
    }

    # Transient failure (5xx, timeout, etc) - retry with backoff
    if ($attempt < $max_retries) {
      my $backoff = 0.5 * $attempt;    # 0.5s, 1s, 1.5s
      $self->log_debug(
"Attempt $attempt failed with $response->{status}, retrying after ${backoff}s"
      );
      select(undef, undef, undef, $backoff);
    }
  }

  $self->checked_urls->{$url}->{status} = $response->{status};

  unless ($response->{success}) {
    $self->log_warn(sprintf(
      'Detected Broken external page %s via status %s - %s.',
      $url, $response->{status}, $response->{reason}
    ));
  }
  else {
    $self->log_debug(sprintf(
      'External page %s returned status %s.',
      $url, $response->{status}
    ));
  }

  return $response->{status};
}

sub update_children_statuses ($self) {
  foreach my $parent_url (keys %{ $self->checked_urls }) {
    next unless exists $self->checked_urls->{$parent_url}->{children};

    foreach
      my $child_href (keys %{ $self->checked_urls->{$parent_url}->{children} })
    {
      next
        unless $self->checked_urls->{$parent_url}->{children}->{$child_href} eq
        'pending';

      # Convert relative href to absolute URL to find in checked_urls
      my $abs_uri     = URI->new($child_href)->abs($parent_url);
      my $abs_url_str = $abs_uri->as_string;

      if (exists $self->checked_urls->{$abs_url_str}) {
        $self->checked_urls->{$parent_url}->{children}->{$child_href} =
          $self->checked_urls->{$abs_url_str}->{status};
      }
      else {
        $self->log_warn("Could not find status for child URL: $abs_url_str");
        $self->checked_urls->{$parent_url}->{children}->{$child_href} =
          'unknown';
      }
    }
  }
}

1;
__END__
