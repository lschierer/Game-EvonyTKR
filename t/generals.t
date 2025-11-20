use v5.42.0;
use utf8::all;
use experimental qw(class);
use Test::More;
use List::AllUtils qw(all);
use Data::Printer;
use Path::Tiny;
use File::FindLib 'lib';
use Scalar::Util 'blessed';

require Game::EvonyTKR;
require Game::EvonyTKR::Logger::Config;
require Game::EvonyTKR::Shared::Constants;
require Game::EvonyTKR::Model::EvonyTKR::Manager;

# Setup logger
my $logger_config = Game::EvonyTKR::Logger::Config->new('test');
my $log_config = Path::Tiny->cwd->child('share/log4perl.test.conf');
Log::Log4perl::Config->utf8(1);
Log::Log4perl::init($log_config->canonpath());
my $logger = Log::Log4perl->get_logger('Game.EvonyTKR');

# Locate data
my $dist_dir = Path::Tiny::path('./share');
my $collection_dir = $dist_dir->child("collections/data");

# Manager setup
my $RootManager = Game::EvonyTKR::Model::EvonyTKR::Manager->new(SourceDir => $dist_dir);
$logger->info("Importing generals from $collection_dir");

$RootManager->generalManager->importAll($collection_dir->child("generals"));
my @generals = values %{ $RootManager->generalManager->get_all_generals() };

my @yaml_files = Path::Tiny::path($collection_dir->child('generals'))
  ->children(qr/\.ya?ml$/);

is(scalar @generals, scalar @yaml_files,
   "All general YAML files imported: " . scalar(@generals) . " of " . scalar(@yaml_files));
# Validate structure
my @bad;
for my $general (@generals) {
  my $id = $general->id // '(unknown)';

  # Basic checks matching your Zod expectations
  my $problems = [];

  push @$problems, 'not blessed' unless blessed($general);
  push @$problems, 'missing id' unless defined $general->id;
  push @$problems, 'missing name' unless defined $general->name;

  my $attr = $general->basicAttributes;
  if (!ref($attr) || ref($attr) ne 'Game::EvonyTKR::Model::BasicAttributes') {
    push @$problems, 'missing basicAttributes: ' . ref($attr);
  } else {
    my %h = %{$attr->to_hash()};
    foreach my $key (qw(attack defense leadership politics)) {
      my $val = $h{$key};
      if (ref($val) ne 'Game::EvonyTKR::Model::BasicAttribute') {
        push @$problems, "bad attribute $key";
      } else {
        unless ($val->base >= 0 && $val->increment >= 0) {
          push @$problems, "bad values in attribute $key";
        }
      }
    }
  }

  push @$problems, 'missing builtInBookName' unless defined $general->builtInBookName;
  push @$problems, 'missing specialtyNames' unless ref($general->specialtyNames) eq 'ARRAY';
  push @$problems, 'missing type array' unless ref($general->type) eq 'ARRAY';

  if (@$problems) {
    push @bad, { name => $general->name // '(unnamed)', id => $id, problems => $problems };
  }
}

if (@bad) {
  diag "Malformed generals:";
  for my $b (@bad) {
    diag "  $b->{name} (ID: $b->{id}) has issues: " . join(', ', $b->{problems}->@*);
  }
}

is(scalar @bad, 0, "All generals passed structure checks");
done_testing;
