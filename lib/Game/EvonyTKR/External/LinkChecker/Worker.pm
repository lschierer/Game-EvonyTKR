use v5.42.0;
use experimental qw(class);
use utf8::all;

package Game::EvonyTKR::External::LinkChecker::Worker;
use Mojo::Base 'Game::EvonyTKR::External::JobBase', -signatures;
use HTTP::Tiny;
use HTML::LinkExtor;
use URI;
use List::AllUtils qw(uniq);

sub task_name { 'check_links_section' }

sub register_task ($class, $app) {
  $app->minion->add_task($class->task_name => __PACKAGE__);
}

sub run ($job, $start_url, $checked_urls = {}, $max_depth = 2, $current_depth = 0) {
  my $results = {
    checked => {},
    broken => {},
    external_refs => []
  };
  
  my @queue = ($start_url);
  my $base_host = URI->new($start_url)->host;
  
  while (@queue && $current_depth <= $max_depth) {
    my $url = shift @queue;
    next if $checked_urls->{$url} || $results->{checked}{$url};
    
    my $status = $job->_check_single_url($url);
    $results->{checked}{$url} = $status;
    
    if ($status !~ /^2/) {
      $results->{broken}{$url} = $status;
      next;
    }
    
    # Extract links if successful and within depth limit
    if ($current_depth < $max_depth) {
      my $links = $job->_extract_links($url, $base_host);
      
      for my $link (@$links) {
        my $link_host = URI->new($link)->host;
        
        if ($link_host eq $base_host) {
          push @queue, $link unless $checked_urls->{$link} || $results->{checked}{$link};
        } else {
          push @{$results->{external_refs}}, $link;
        }
      }
    }
    
    $current_depth++ if @queue == 0; # Increment depth when level complete
  }
  
  $job->finish($results);
}

sub _check_single_url ($job, $url) {
  my $http = HTTP::Tiny->new(timeout => 30, agent => 'EvonyTKR-LinkChecker/2.0');
  
  for my $attempt (1..3) {
    my $response = $http->get($url);
    return $response->{status} if $response->{success} || $response->{status} < 500;
    
    select(undef, undef, undef, 0.5 * $attempt) if $attempt < 3;
  }
  
  return 500; # Failed after retries
}

sub _extract_links ($job, $url, $base_host) {
  my $http = HTTP::Tiny->new(timeout => 30);
  my $response = $http->get($url);
  
  return [] unless $response->{success} && $response->{content};
  
  my $extractor = HTML::LinkExtor->new(undef, $url);
  $extractor->parse($response->{content});
  
  my @links;
  for my $link_array ($extractor->links) {
    my ($tag, %attrs) = @$link_array;
    my $href = $attrs{href} || $attrs{src};
    
    next unless $href && $href !~ /^mailto:/;
    
    my $abs_url = URI->new($href)->abs($url)->as_string;
    push @links, $abs_url;
  }
  
  return [uniq @links];
}

1;
