use v5.42.0;
use utf8::all;
use Test2::V0;
use Scalar::Util qw(blessed);
use File::FindLib 'lib';

use Game::EvonyTKR::Loader::Generals;
use Game::EvonyTKR::Loader::Covenants;

# Test that the Covenants loader works correctly
# Note: Covenants loader requires generals to be loaded first

# First load generals (required dependency)
my $generals_loader = Game::EvonyTKR::Loader::Generals->new(
  data_dir => 'share/collections/data/generals',
);
my $generals_count = $generals_loader->load_all();
ok($generals_count > 0, "Loaded $generals_count generals (dependency for covenants)");

# Now load covenants
my $loader = Game::EvonyTKR::Loader::Covenants->new(
  data_dir        => 'share/collections/data/covenants',
  generals_loader => $generals_loader,
);

my $count = $loader->load_all();
ok($count > 0, "Loaded $count covenants");

# Test list_covenants
my $covenant_keys = $loader->list_covenants();
is(scalar(@$covenant_keys), $count, "list_covenants returns correct count");

# Test get_covenant for each loaded covenant
my @failed_loads;
my @structure_errors;

for my $key (@$covenant_keys) {
  my $covenant = $loader->get_covenant($key);

  unless ($covenant) {
    push @failed_loads, $key;
    next;
  }

  # Validate structure
  my @problems;

  push @problems, 'not blessed' unless blessed($covenant);
  push @problems, 'missing name' unless defined $covenant->name;

  # Check members array
  my $members = $covenant->members;
  if (!$members || ref($members) ne 'ARRAY') {
    push @problems, 'members not an array';
  }
  elsif (scalar(@$members) == 0) {
    push @problems, 'empty members array';
  }

  # Check levels if present
  if ($covenant->can('levels')) {
    my $levels = $covenant->levels;
    if ($levels && ref($levels) ne 'ARRAY') {
      push @problems, 'levels not an array';
    }
  }

  if (@problems) {
    push @structure_errors, {
      key      => $key,
      name     => $covenant->name // '(unnamed)',
      problems => \@problems,
    };
  }
}

is(scalar(@failed_loads), 0, "All covenants loaded successfully")
  or diag("Failed to load: " . join(', ', @failed_loads));

is(scalar(@structure_errors), 0, "All covenants passed structure validation")
  or do {
    for my $err (@structure_errors) {
      diag(sprintf("  %s (%s): %s",
        $err->{name}, $err->{key}, join(', ', @{$err->{problems}})));
    }
  };

# Test covenant_count
is($loader->covenant_count(), $count, "covenant_count matches loaded count");

# Test that covenant members reference valid generals
my @invalid_members;
for my $key (@$covenant_keys) {
  my $covenant = $loader->get_covenant($key);
  next unless $covenant;

  for my $member (@{$covenant->members}) {
    my $general_name = ref($member) ? $member->name : $member;
    my $normalized = $generals_loader->normalize($general_name);
    my $general = $generals_loader->get_general($normalized);
    unless ($general) {
      push @invalid_members, {
        covenant => $covenant->name,
        member   => $general_name,
      };
    }
  }
}

is(scalar(@invalid_members), 0, "All covenant members reference valid generals")
  or do {
    for my $err (@invalid_members) {
      diag(sprintf("  Covenant '%s' has invalid member: %s",
        $err->{covenant}, $err->{member}));
    }
  };

done_testing;
