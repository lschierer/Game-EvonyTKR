use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require HTTP::Tiny;
require HTML::LinkExtor;
require URI;

class LinkChecker::Command {
  use List::AllUtils qw( any none );
  use Log::Log4perl  qw(:easy);
  use Log::Log4perl::Config;
  use Log::Log4perl qw(:levels);
  use namespace::autoclean;
  use Carp;
  our $VERSION = 'v0.30.0';
  Log::Log4perl::Config->utf8(1);

  field $debug : param //= 0;

  field $startUrl : param;
  field %checked_urls;
  field @urls_to_check;
  field $logger;

  ADJUST {
    push @urls_to_check, $startUrl;
  }

  ADJUST {
    if ($debug) {
      Log::Log4perl->easy_init($DEBUG);
    }
    else {
      Log::Log4perl->easy_init($WARN);
    }

    $logger = Log::Log4perl->get_logger(__CLASS__);
  }

  method execute {
    $logger->info("Starting checking at $startUrl");
    
    # Process queue until empty
    while (@urls_to_check) {
      my $url = shift @urls_to_check;  # FIFO: take from front
      $self->check_url($url);
    }
    
    $logger->info("Url Checking complete");
    
    # Update children statuses now that all URLs are processed
    $self->update_children_statuses();
    
    foreach my $checked (sort keys %checked_urls) {
      if ($checked_urls{$checked}->{status} !~ /^2/) {
        say "Found Broken Link to $checked";
      }
      elsif (exists $checked_urls{$checked}->{children}) {
        foreach my $child (sort keys %{ $checked_urls{$checked}->{children} }) {
          if ($checked_urls{$checked}->{children}->{$child} !~ /^2/) {
            say "Page $checked contains Broken Link to $child.";
          }
        }
      }
    }
  }

  method check_url ($url, $recurse = 1) {
    # Remove fragment for checking purposes
    my $uri = URI->new($url);

    if (exists $checked_urls{$url}) {
      $logger->debug("$url has already been checked. Skipping.");
      return $checked_urls{$url}->{status};
    }

    $logger->info("Checking $url");

    my $response = HTTP::Tiny->new->get($url);
    $checked_urls{$url}->{status} = $response->{status};

    unless ($response->{success}) {
      $logger->warn(sprintf(
        'Detected Broken page %s via status %s - %s.',
        $url, $response->{status}, $response->{reason}
      ));
      return $response->{status};
    }

    $logger->debug(
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
          $logger->warn("Fragment #$frag NOT found on page $url");
          $checked_urls{$url}->{status} =
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
        $logger->info("extracted hostname $hostname");

        foreach my $link_array (sort @links) {
          my ($tag, %attrs) = @$link_array;
          my $href = $attrs{href} || $attrs{src};

          if ($href) {
            my $abs_uri = URI->new($href)->abs($url);
            $logger->info("found url to check: $abs_uri");

            unless ($abs_uri->scheme eq 'mailto') {    # Avoid email links
              my $abs_url_str = $abs_uri->as_string;
              
              # Add to queue if not already checked or queued
              unless (exists $checked_urls{$abs_url_str}) {
                # Check if already in queue to avoid duplicates
                unless (grep { $_ eq $abs_url_str } @urls_to_check) {
                  push @urls_to_check, $abs_url_str;  # Add to end of queue
                  $logger->debug("Added $abs_url_str to queue");
                }
              }
              
              # Mark the relationship for later status update
              $checked_urls{$url}->{children}->{$href} = 'pending';
            }
          }
        }
      }

    }

    return $response->{status};
  }

  method update_children_statuses {
    foreach my $parent_url (keys %checked_urls) {
      next unless exists $checked_urls{$parent_url}->{children};
      
      foreach my $child_href (keys %{ $checked_urls{$parent_url}->{children} }) {
        next unless $checked_urls{$parent_url}->{children}->{$child_href} eq 'pending';
        
        # Convert relative href to absolute URL to find in checked_urls
        my $abs_uri = URI->new($child_href)->abs($parent_url);
        my $abs_url_str = $abs_uri->as_string;
        
        if (exists $checked_urls{$abs_url_str}) {
          $checked_urls{$parent_url}->{children}->{$child_href} = 
            $checked_urls{$abs_url_str}->{status};
        } else {
          $logger->warn("Could not find status for child URL: $abs_url_str");
          $checked_urls{$parent_url}->{children}->{$child_href} = 'unknown';
        }
      }
    }
  }

}
1;
__END__
