use v5.42.0;
use utf8::all;
use Test2::V0;
use Scalar::Util qw(blessed);
use File::FindLib 'lib';

use Game::EvonyTKR::Loader::Generals;

# Test that the Generals loader works correctly

my $loader = Game::EvonyTKR::Loader::Generals->new(
  data_dir => 'share/collections/data/generals',);

# Test loading
my $count = $loader->load_all();
ok($count > 0, "Loaded $count generals");

# Test list_generals
my $general_keys = $loader->list_generals();
is(scalar(@$general_keys), $count, "list_generals returns correct count");

# Test get_general for each loaded general
my @failed_loads;
my @structure_errors;

for my $key (@$general_keys) {
  my $general = $loader->get_general($key);

  unless ($general) {
    push @failed_loads, $key;
    next;
  }

  # Validate structure matches expected schema
  my @problems;

  push @problems, 'not blessed'  unless blessed($general);
  push @problems, 'missing id'   unless defined $general->id;
  push @problems, 'missing name' unless defined $general->name;

  # Check basicAttributes
  my $attr = $general->basicAttributes;
  if ( !$attr
    || !blessed($attr)
    || !$attr->isa('Game::EvonyTKR::Model::BasicAttributes')) {
    push @problems, 'missing or invalid basicAttributes';
  }
  else {
    for my $field (qw(attack defense leadership politics)) {
      my $val = $attr->$field;
      unless ($val
        && blessed($val)
        && $val->can('base')
        && $val->can('increment')) {
        push @problems, "invalid $field attribute";
      }
      elsif ($val->base < 0 || $val->increment < 0) {
        push @problems, "negative values in $field attribute";
      }
    }
  }

  push @problems, 'missing builtInBookName'
    unless defined $general->builtInBookName;
  push @problems, 'specialtyNames not an array'
    unless ref($general->specialtyNames) eq 'ARRAY';
  push @problems, 'type not an array' unless ref($general->type) eq 'ARRAY';

  if (@problems) {
    push @structure_errors,
      {
      key      => $key,
      name     => $general->name // '(unnamed)',
      problems => \@problems,
      };
  }
}

is(scalar(@failed_loads), 0, "All generals loaded successfully")
  or diag("Failed to load: " . join(', ', @failed_loads));

is(scalar(@structure_errors), 0, "All generals passed structure validation")
  or do {
  for my $err (@structure_errors) {
    diag(sprintf("  %s (%s): %s",
      $err->{name}, $err->{key}, join(', ', @{ $err->{problems} })));
  }
  };

# Test that we can look up generals by normalized name
my $test_general = $loader->get_general($general_keys->[0]);
if ($test_general) {
  my $name       = $test_general->name;
  my $normalized = $loader->normalize($name);
  my $lookup     = $loader->get_general($normalized);
  ok($lookup, "Can look up general by normalized name: $normalized");
  is($lookup->name, $name, "Lookup returns correct general");
}

# Test general_count
is($loader->general_count(), $count, "general_count matches loaded count");

done_testing;
