use v5.42.0;
use utf8::all;
use File::FindLib 'lib';
require Storable;

package Game::EvonyTKR::Role::Cache {
  use Mojo::Base -role, -signatures;
  use Cache::Memcached::Fast;
  use List::AllUtils qw(uniq);
  use Carp;

  our %default_options = (
    servers            => ['127.0.0.1:11211'],
    hash_namespace     => 1,
    compress_threshold => 10_000,
    compress_ratio     => 0.9,
    compress_methods   =>
      [\&IO::Compress::Gzip::gzip, \&IO::Uncompress::Gunzip::gunzip],
    utf8              => 1,
    nowait            => 1,
  );

  sub default_client($self) {
    state $client = Cache::Memcached::Fast->new({
      namespace => 'EvonyTKR:',
      %default_options,
    });
    return $client;
  }

  sub create_cache($self, $options = {}) {
    unless (exists $options->{namespace}) {
      $self->logger->logcroak(
        'create_cache requires a namespace as part of its options');
      return;
    }

    return Cache::Memcached::Fast->new({ %default_options, %$options });
  }

  sub add_item($self, $key, $data, $client = undef) {
    my $logkey = rand(1000);
    $client //= $self->default_client;

    # First, store the actual data
    my $dr = $client->set($key, $data);
    unless (defined($dr) && $dr == 1) {
      $self->logger->error("$logkey: failed to set key $key");
      return $dr;
    }

    # Now safely update _all_keys using CAS
    my $max_retries = 10;
    for my $attempt (1 .. $max_retries) {
      # gets() returns [value, cas_token] or undef if key doesn't exist
      my $gets_result = $client->gets('_all_keys');

      my ($current_keys, $cas_token);
      my $new_keys;

      if (defined $gets_result) {
        ($cas_token, $current_keys,) = @$gets_result;
        $self->logger->debug(sprintf(
        '%s: found current keys "%s" for cas_token "%s"',
        $logkey, $current_keys, $cas_token));
        $current_keys = '' unless defined($current_keys);
        # Add our key if it's not already there
        my @keys = grep {length} split ',', $current_keys;
        $self->logger->debug("$logkey: check if key '$key' is present");
        unless (grep { $_ eq $key } @keys) {
          # sort the keys for easier debugging.
          push @keys, $key;
          $new_keys = join ',', sort { "$a" cmp "$b" } List::AllUtils::uniq @keys;
        }
        else {
          $self->logger->warn(
            "$logkey: attempting to add duplicate key '$key'");
          return 1;    # Key already exists, we're done
        }

        # Use CAS since the key exists
        my $cas_result = $client->cas('_all_keys', $cas_token, $new_keys,);
        if (defined($cas_result) && $cas_result eq "1") {
          $self->logger->debug(
            "$logkey: Success for key '$key' on attempt $attempt");
          return 1;    # Success!
        }
        else {
          $self->logger->debug(sprintf(
            '%s: got cas_result "%s", %s retries left',
            $logkey, $cas_result, $max_retries - $attempt
          ));
        }
        # CAS failed, retry

      }
      else {
        # Key doesn't exist, use regular set()
        $self->logger->debug("$logkey: setting key '$key' using client->set");
        my $set_result = $client->set('_all_keys', $key);
        if (defined($set_result) && $set_result == 1) {
          return 1;    # Success!
        }
        # set() failed, maybe someone else created it, retry with gets()
      }

      $self->logger->debug(
        "$logkey: CAS/set conflict on attempt $attempt, retrying");
    }

    $self->logger->error(
      "$logkey: Failed to update _all_keys after $max_retries attempts");
    return 0;
  }

  sub get_all_items($self, $client = undef) {
    $client //= $self->default_client;

    my $casr = $client->gets('_all_keys') // [undef, ''];
    $self->logger->debug(
      sprintf(
        'get_all_items casr %s', Data::Printer::np($casr, multiline => 0)
      )
    );
    my @keys = uniq split ',', $casr->[1];
    @keys = grep {length} @keys;
    $self->logger->debug(sprintf('get_all_items found %s keys', scalar @keys));
    my $gmr = $client->get_multi(@keys);
    $self->logger->debug(
      sprintf('get_all_items get_multi returned %s items',
        scalar keys $gmr->%*)
    );

    if (scalar keys $gmr->%* == 0 && scalar @keys > 0) {
      my $key  = $keys[0];
      my $test = $client->get($key);
      $self->logger->debug(
        sprintf(
          'get_all_items test get returned %s for key %s',
          Data::Printer::np($test, multiline => 0), $key
        )
      );
    }
    return $gmr;
  }

  sub set_value($self, $key, $value, $client = undef, $expiration = 0) {
    $client //= $self->default_client;
    my $result = $client->set($key, $value, $expiration);
    if (not defined $result) {
      $result = $client->server_versions;
      if (defined $result) {
        $result = Data::Printer::np($result, multiline => 0);
      }
      else {
        $result = 'cannot connect to get versions';
      }
    }
    return $result;
  }

  sub get_value($self, $key, $client = undef) {
    $client //= $self->default_client;
    return $client->get($key);
  }

  sub delete_value($self, $key, $client = undef) {
    $client //= $self->default_client;
    $client->delete($key);
    my @keys = uniq split ',', ($client->get('_all_keys') // '');
    @keys = sort grep { length($_) && $_ ne $key } @keys;
    $client->set('_all_keys', join(',', @keys), 0);
  }
}
1;
__END__
