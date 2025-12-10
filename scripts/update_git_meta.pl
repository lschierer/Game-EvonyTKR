#!/usr/bin/env perl
use v5.40;
use experimental qw(class);
use utf8::all;

use File::FindLib 'lib';
require Game::EvonyTKR;
use File::Share qw(dist_dir );
require Path::Tiny;

use GitRepo::Reader;
require YAML::PP;
use DateTime;
require Game::EvonyTKR::Role::Logging;
require Data::Printer;

# Setup logging
my $logger = Game::EvonyTKR::Role::Logging::get_logger(__PACKAGE__);

# 1. Read git metadata
my $distDir = Path::Tiny::path(dist_dir('Game::EvonyTKR'));
$logger->info("distDir is $distDir");
my $reader = GitRepo::Reader->new(source_dir => $distDir->parent());

# Get current commit and branch info (fast operations)
my $git_commit = $reader->get_current_commit();
my $git_branch = $reader->get_current_branch();
my $build_time = DateTime->now->iso8601();

$logger->info("Git commit: $git_commit");
$logger->info("Git branch: $git_branch");
$logger->info("Build time: $build_time");

# 2. Load config file
my $yaml        = YAML::PP->new;
my $config_path = $distDir->parent()->child('game-evony_t_k_r.yml');

$logger->info("Reading config from $config_path");
my $config_data = {};
if ($config_path->is_file) {
  $config_data = $yaml->load_string($config_path->slurp_utf8);
}
else {
  $logger->error("config file $config_path is not found.");
}

# 3. Inject version metadata (preserve git_meta if it exists)
# Note: git_meta (copyright/authors) should be manually maintained in the config file
#       This script only updates the version info (commit, branch, build time)

$config_data->{version} = {
  'git-commit' => $git_commit,
  'git-branch' => $git_branch,
  'build-time' => $build_time,
};

# 4. Write back
$logger->info(
  "Updating version section with commit $git_commit on branch $git_branch");
$config_path->spew_utf8($yaml->dump_string($config_data));

$logger->info("Config updated successfully.");
