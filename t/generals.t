use v5.42.0;
use utf8::all;
use experimental qw(class);
use Test::More;
use List::AllUtils qw(all);
use Data::Printer;
use Mojo::File;
use File::FindLib 'lib';
use Scalar::Util 'blessed';
use Storable qw(freeze thaw);
use Cache::Memcached;
use IO::Compress::Gzip;
use IO::Uncompress::Gunzip;
use Unicode::CaseFold;

our %default_options = (
  utf8              => 1,
  serialize_methods =>
    [\&Sereal::Encoder::encode_sereal, \&Sereal::Decoder::decode_sereal],
  servers            => ['127.0.0.1:11211'],
  compress_threshold => 1000_000,
);

require Game::EvonyTKR;
require Game::EvonyTKR::Log::Config;
require Game::EvonyTKR::Shared::Constants;
require Game::EvonyTKR::External::Common;

# Setup logger
my $logger        = Game::EvonyTKR::Log::Config->get_logger('Test::Package');
my $memcachClient = Cache::Memcached->new({
  debug     => 0,
  namespace => 'generals__',
  %default_options,
});
# Locate data
my $dist_dir       = Mojo::File->new('./share');
my $collection_dir = $dist_dir->child("collections/data");

$logger->info("Importing generals from $collection_dir");
my $importTool = Game::EvonyTKR::External::Common->new(app => undef);

my $generals = {};

my @yaml_files = ($collection_dir->child('generals'))->list->grep(sub {
  if ($_ =~ m/\.ya?ml$/) {
    return 1;
  }
  return 0;
})->each;


foreach my $index (0 .. $#yaml_files) {
  my $yf      = $yaml_files[$index];
  my $general = $importTool->load_single_general($yf, $index);
  if ($general) {
    $generals->{ $general->normalize($general->name) } = $general;
    $logger->info(sprintf('successfully imported "%s"',
      $general->normalize($general->name)));
    eval {
      my $frozen = freeze($general);
      my $thawed = thaw($frozen);
      if ($thawed->isa('Game::EvonyTKR::Model::General')) {
        if ($thawed->builtInBook->isa('Game::EvonyTKR::Model::Book')) {
          $logger->info("Mojo::Base serialization successful");
        }
        else {
          $logger->logcroak(
            '$thawed->builtInBook is not a Game::EvonyTKR::Model::Book');
        }
      }
      else {
        $logger->logcroak('$thawed is not a Game::EvonyTKR::Model::General');
      }
    } or do {
      $logger->logcroak("Mojo::Base serialization failed: $@");
    };

    $memcachClient->delete($test_key);

    # Simple test first
    my $simple_test = $memcachClient->set('simple_test', 'hello world');
    $logger->debug(sprintf('Simple test result: %s', $simple_test // 'undef'));

    my $simple_get = $memcachClient->get('simple_test');
    $logger->debug(sprintf('Simple get result: %s', $simple_get // 'undef'));

    eval {
      my $frozen = freeze($general);
      $logger->debug(sprintf('Frozen data length: %d bytes', length($frozen)));
      my $key = 'general_' . $general->name;
      $key =~ s/[^a-zA-Z0-9_-]/_/g;    # Simple key sanitization
      $logger->debug(sprintf('Using key: "%s"', $key));
      my $setResult = $memcachClient->set($key, $frozen);
      $logger->debug(sprintf(
        'setResult for %s: %s (defined: %s)',
        $key,
        $setResult // 'undef',
        defined($setResult) ? 'yes' : 'no'
      ));
      unless ($setResult) {
        die sprintf('failed to set general "%s" in Memcached', $key);
      }
      $logger->debug(sprintf('set for %s complete', $key));
      my $getResult = $memcachClient->get($key);
      unless (defined($getResult)) {
        die sprintf('failed to get general %s from Memcached', $key);
      }
      my $thawed = thaw($getResult);
      $logger->info(sprintf(
        'setResult: %s; retrieved and thawed general: %s',
        $setResult, ref($thawed)
      ));
      1;    # Ensure eval returns true on success
    } or do {
      my $error = $@ || 'unknown error';
      $logger->logcroak("Memcached serialization failed: $error");
    };
  }
}

$logger->info('All memcached tests completed successfully');

is(
  scalar keys $generals->%*,
  scalar @yaml_files,
  "All general YAML files imported: "
    . scalar(keys $generals->%*) . " of "
    . scalar(@yaml_files)
);
# Validate structure
my @bad;
for my $general (values $generals->%*) {
  my $id = $general->id // '(unknown)';

  # Basic checks matching your Zod expectations
  my $problems = [];

  push @$problems, 'not blessed'  unless blessed($general);
  push @$problems, 'missing id'   unless defined $general->id;
  push @$problems, 'missing name' unless defined $general->name;

  my $attr = $general->basicAttributes;
  if (!ref($attr) || ref($attr) ne 'Game::EvonyTKR::Model::BasicAttributes') {
    push @$problems, 'missing basicAttributes: ' . ref($attr);
  }
  else {
    my %h = %{ $attr->to_hash() };
    foreach my $key (qw(attack defense leadership politics)) {
      my $val = $h{$key};
      if (ref($val) ne 'Game::EvonyTKR::Model::BasicAttribute') {
        push @$problems, "bad attribute $key";
      }
      else {
        unless ($val->base >= 0 && $val->increment >= 0) {
          push @$problems, "bad values in attribute $key";
        }
      }
    }
  }

  push @$problems, 'missing builtInBookName'
    unless defined $general->builtInBookName;
  push @$problems, 'missing specialtyNames'
    unless ref($general->specialtyNames) eq 'ARRAY';
  push @$problems, 'missing type array' unless ref($general->type) eq 'ARRAY';

  if (@$problems) {
    push @bad,
      {
      name     => $general->name // '(unnamed)',
      id       => $id,
      problems => $problems
      };
  }
}

if (@bad) {
  diag "Malformed generals:";
  for my $b (@bad) {
    diag "  $b->{name} (ID: $b->{id}) has issues: "
      . join(', ', $b->{problems}->@*);
  }
}

is(scalar @bad, 0, "All generals passed structure checks");
done_testing;

sub default_client($self) {
  state $client = Cache::Memcached::Fast->new({
    namespace => 'EvonyTKR:',
    %default_options,
  });
  return $client;
}

sub create_cache($self, $options = {}) {
  unless (exists $options->{namespace}) {
    $logger->logcroak(
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
    $logger->error("$logkey: failed to set key $key");
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
      $logger->info(sprintf(
        '%s: found current keys "%s" for cas_token "%s"',
        $logkey, $current_keys, $cas_token
      ));
      $current_keys = '' unless defined($current_keys);
      # Add our key if it's not already there
      my @keys = grep {length} split ',', $current_keys;
      $logger->info("$logkey: check if key '$key' is present");
      unless (grep { $_ eq $key } @keys) {
        $new_keys = $current_keys ? "$current_keys,$key" : $key;
      }
      else {
        $logger->warn("$logkey: attempting to add duplicate key '$key'");
        return 1;    # Key already exists, we're done
      }

      # Use CAS since the key exists
      my $cas_result = $client->cas('_all_keys', $cas_token, $new_keys,);
      if (defined($cas_result) && $cas_result eq "1") {
        $logger->info("$logkey: Success for key '$key' on attempt $attempt");
        return 1;    # Success!
      }
      else {
        $logger->info(sprintf(
          '%s: got cas_result "%s", %s retries left',
          $logkey, $cas_result, $max_retries - $attempt
        ));
      }
      # CAS failed, retry

    }
    else {
      # Key doesn't exist, use regular set()
      $logger->info("$logkey: setting key '$key' using client->set");
      my $set_result = $client->set('_all_keys', $key);
      if (defined($set_result) && $set_result == 1) {
        return 1;    # Success!
      }
      # set() failed, maybe someone else created it, retry with gets()
    }

    $logger->info("$logkey: CAS/set conflict on attempt $attempt, retrying");
  }

  $logger->error(
    "$logkey: Failed to update _all_keys after $max_retries attempts");
  return 0;
}

sub get_all_items($self, $client = undef) {
  $client //= $self->default_client;
  my $keys_data = $client->get('_all_keys');
  $logger->info("get_all_items _all_keys retrieval: "
      . (defined $keys_data ? 'SUCCESS' : 'FAILED'));

  if (defined $keys_data) {
    my @keys     = List::AllUtils::uniq split ',', $keys_data;
    my $test_key = $keys[0];    # Try first key

    $logger->info("get_all_items Testing key: '$test_key'");

    # Try individual get
    my $individual = $client->get($test_key);
    $logger->info("get_all_items Individual get result: "
        . (defined $individual ? 'SUCCESS' : 'FAILED'));

    if (defined $individual) {
      $logger->info("get_all_items Individual type: " . ref($individual));
      $logger->info(
        "get_all_items Individual blessed: " . (blessed($individual) // 'NO'));
    }

    # Try get_multi with just one key
    my $multi_result = $client->get_multi($test_key);
    $logger->info("get_all_items get_multi single key: "
        . (keys %$multi_result ? 'SUCCESS' : 'FAILED'));
  }
  else {
    $logger->warn('get_all_items No key data found');
  }
  my $casr = $client->gets('_all_keys') // [undef, ''];
  $logger->info(sprintf(
    'get_all_items casr %s', Data::Printer::np($casr, multiline => 0)));
  my @keys = List::AllUtils::uniq split ',', $casr->[1];
  @keys = grep {length} @keys;
  $logger->info(sprintf('get_all_items found %s keys', scalar @keys));
  my $gmr = $client->get_multi(@keys);
  $logger->info(
    sprintf('get_all_items get_multi returned %s',
      Data::Printer::np($gmr, multiline => 0))
  );

  if (scalar keys $gmr->%* == 0 && scalar @keys > 0) {
    my $key  = $keys[0];
    my $test = $client->get($key);
    $logger->info(sprintf(
      'get_all_items test get returned %s for key %s',
      Data::Printer::np($test, multiline => 0), $key
    ));
  }
  return $gmr;
}
