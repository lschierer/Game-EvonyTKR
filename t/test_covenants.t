#!/usr/bin/env perl
use v5.42.0;
use experimental qw(class);
use Test2::V0;
use lib 'lib';

# File::Share requires the main module first
require Game::EvonyTKR;
require Game::EvonyTKR::Log::Config;
require Game::EvonyTKR::Service::Cache;
require YAML::PP;
require Path::Tiny;

my $logger = Game::EvonyTKR::Log::Config->logger('Test::Package');
$logger->info(sprintf('Test script logging configured with log level %s',
  Log::Log4perl::Level::to_level($logger->level()),));

require Game::EvonyTKR::Role::Logger;
require Game::EvonyTKR::Model::General;
require Game::EvonyTKR::Model::Covenant;

my $distDir = Mojo::Home->new->detect('Game::EvonyTKR')->to_string;
my $CovenantsDir =
  Mojo::File->new($distDir)->child('share/collections/data/covenants');
my $GeneralsDir =
  Mojo::File->new($distDir)->child('share/collections/data/generals');

ok(-r -d $CovenantsDir, "using CovenantsDir $CovenantsDir");

my $cc = Game::EvonyTKR::Service::Cache->new();

my $test_key = 'memcached_test_' . time();

unless ($cc->set($test_key, 'test')) {
  $logger->info('Memcached not available, skipping memcached tests');
  exit 1;
}

package Test::Package {
  use Mojo::Base -base,                                         -signatures;
  use Mojo::Base 'Game::EvonyTKR::Role::Logger',                -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Common',                -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Persistence',  -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Persistence', -role;

  sub loadAllGenerals($self) {
    my @yaml_files = $GeneralsDir->list->grep(sub {
      if ($_ =~ m/\.ya?ml$/) {
        return 1;
      }
      return 0;
    })->each;
    my @suffixList = ('.yaml', '.yml');
    foreach my $filename (@yaml_files) {

      my ($generalFile) = $GeneralsDir->list->grep(sub {
        my $nn = $self->normalize($filename);
        $nn = Mojo::File->new($nn)->basename(@suffixList);
        my $cf = $self->normalize($_->basename(@suffixList));
        if ($nn eq $cf) {
          return 1;
        }
        return 0;
      })->head(1)->each;

      unless (-f $generalFile && -r $generalFile) {
        $self->logger->error("Cannot read general file: $generalFile");
        return 0;
      }

      my $data       = $generalFile->slurp('UTF-8');
      my $hashObject = YAML::PP->new(
        schema       => [qw/ + Perl /],
        yaml_version => ['1.2', '1.1'],
      )->load_string($data);

      my $general = Game::EvonyTKR::Model::General->from_hash($hashObject);
      my $result  = $self->add_general($general);
      unless ($general && $result) {
        $self->logger->error("Failed to import General from $generalFile");
        return 0;
      }
    }
    return 1;
  }

  sub loadCovenant ($self, $filename) {
    my $CovenantFile = Mojo::File->new(Encode::decode_utf8($filename));
    my $data         = $CovenantFile->slurp('UTF-8');
    my $hashObject   = YAML::PP->new(
      schema       => [qw/ + Perl /],
      yaml_version => ['1.2', '1.1'],
    )->load_string($data);
    my $primary = $self->get_general($hashObject->{name});
    unless ($primary) {
      my $errmessage = sprintf('cannot find general "%s"', $hashObject->{name});
      $self->logger->error($errmessage);
      return 0;
    }

    my $covenant =
      Game::EvonyTKR::Model::Covenant->from_hash($hashObject, $primary);
    return $covenant;
  }

  sub loadAllCovenants($self) {
    my @files =
      $CovenantsDir->list->grep(sub { $_ =~ /\.ya?ml$/ && -f -r $_ })->each;
    my @covenants;
    my $failures = [];
    foreach my $filename (@files) {
      my $covenant = $self->loadCovenant($filename);
      unless ($covenant) {
        $self->logger->error(
          sprintf('failed to create covenant from file %s', $filename));
        push @{$failures}, $filename;
        next;
      }
      push @covenants, $covenant;
    }
    if (scalar(@{$failures})) {
      return 0;
    }
    if (scalar(@covenants) == 0) {
      $self->logger->error('no covenants loaded without any errors generated.');
      return 0;
    }
    return \@covenants;
  }

  sub findCovenants ($self) {
    my $names = [];
    foreach my $cn ($self->list_covenants->@*) {
      push @{$names}, $cn unless (!$cn || !length($cn));
    }
    return $names;
  }
}

my $test = Test::Package->new();
unless ($test->loadAllGenerals()) {
  $logger->logcroak('failed to load the prerequisite generals');
}

subtest 'Load Covenants' => sub {
  my $helper = Test::Package->new();
  my $names  = $test->findCovenants();
  $logger->debug(sprintf('found %s names in findCovenants', scalar(@{$names})));
  ok(scalar(@{$names}), 'find covenants');

  my @files =
    $CovenantsDir->list->grep(sub { $_ =~ /\.ya?ml$/ && -f -r $_ })->each;
  ok(scalar(@files),                      'covenant files identified');
  ok(scalar(@files) == scalar(@{$names}), 'files found equals names found');
  $logger->info(sprintf(
    'found %s covenant files: %s',
    scalar(@files), join ', ', map { sprintf('"%s"', $_) } @files,
  ));
  my $all = $helper->loadAllCovenants();
  ok($all && ref($all) && ref($all) eq 'ARRAY' && scalar(@{$all}),
    'load all covenants from files');
  ok(
    scalar(@{$all}) == scalar(@{$names}),
    'loaded covenants equals names found'
  );
  done_testing();
};

done_testing();
