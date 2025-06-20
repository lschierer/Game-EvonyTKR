#!/usr/bin/env perl
use v5.40;
use utf8;
use Path::Tiny;
use Text::CSV;
use Data::Printer;
use List::Util qw(uniq);

my $generals_file = path('EvansFullGeneralInfo.csv');
my $groups_file   = path('EvAnsConflictGroupDefinitions.csv');

# -----------------------------
# Load generals and their BSC groups
# -----------------------------
my %group_members;
my $gen_csv = Text::CSV->new({ binary => 1, auto_diag => 1 });

open my $gen_fh, '<:encoding(UTF-8)', $generals_file or die "Cannot open generals file: $!";
my $gen_header = $gen_csv->getline($gen_fh);

say "header is " . Data::Printer::np($gen_header);
$gen_csv->column_names(@$gen_header);

while (my $row = $gen_csv->getline_hr($gen_fh)) {
  my $name = $row->{Name} // next;

  my $bsc_codes = $row->{'BSC Codes'};
  my $group = $bsc_codes =~ s/\s+//gr;
  next unless $group =~ /\w/;
  push @{ $group_members{$group} }, $name;
}
close $gen_fh;

# Normalize member lists
$_ = [uniq @{$_}] for values %group_members;

# -----------------------------
# Load group link definitions
# -----------------------------
my %group_links;
my $grp_csv = Text::CSV->new({ binary => 1, auto_diag => 1 });

open my $grp_fh, '<:encoding(UTF-8)', $groups_file or die "Cannot open group definitions file: $!";
my $grp_header = $grp_csv->getline($grp_fh);
say "second header: " . Data::Printer::np($grp_header);
$grp_csv->column_names(@$grp_header);

while (my $row = $grp_csv->getline_hr($grp_fh)) {
  my $combo = $row->{'Troop Combination Code'} // next;
  next if $combo =~ /^\s*$/;
  my @groups = split /[-&]/, $combo;
  for my $g1 (@groups) {
    for my $g2 (@groups) {
      next if $g1 eq $g2;
      push @{ $group_links{$g1} }, $g2;
    }
  }
}
close $grp_fh;

# Normalize linked group lists
$_ = [uniq @{$_}] for values %group_links;

# -----------------------------
# Output results
# -----------------------------
p %group_members;
p %group_links;
