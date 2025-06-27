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
use Log::Log4perl;
require Data::Printer;

# Setup logging
Log::Log4perl::init(\<<'EOT');
log4perl.rootLogger              = INFO, Screen
log4perl.appender.Screen         = Log::Log4perl::Appender::Screen
log4perl.appender.Screen.stderr  = 1
log4perl.appender.Screen.layout  = Log::Log4perl::Layout::PatternLayout
log4perl.appender.Screen.layout.ConversionPattern = %d [%p] %m%n
EOT

my $logger = Log::Log4perl->get_logger;

# 1. Read git metadata
my $distDir = Path::Tiny::path(dist_dir('Game::EvonyTKR'));
$logger->info("distDir is $distDir");
my $reader = GitRepo::Reader->new( source_dir => $distDir->parent() );

my $oldest_dt = $reader->find_copyright_range();
my $year_range;
if ($oldest_dt) {
    my $this_year = DateTime->now->year;
    my $start_year = $oldest_dt->year;
    $year_range = ($start_year == $this_year)
        ? "$this_year"
        : "$start_year-$this_year";
} else {
    $year_range = DateTime->now->year;
}

my $authors = $reader->find_authors();
$logger->info("Found authors " . Data::Printer::np($authors));

# 2. Load config file
my $yaml   = YAML::PP->new;
my $config_path = $distDir->parent()->child('game-evony_t_k_r.yml');

$logger->info("Reading config from $config_path");
my $config_data = {};
if ($config_path->is_file) {
    $config_data = $yaml->load_string($config_path->slurp_utf8);
} else {
  $logger->error("config file $config_path is not found.");
}

# 3. Inject metadata
$config_data->{git_meta} = {
    copyright => $year_range,
    authors   => $authors,
};

# 4. Write back
$logger->info("Updating git_meta section with year '$year_range' and authors");
$config_path->spew_utf8($yaml->dump_string($config_data));

$logger->info("Config updated.");
