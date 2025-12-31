#!/usr/bin/env perl
use v5.42.0;
use lib 'lib';
use YAML::XS;
use JSON::MaybeXS;

# This script generates a static conflict lookup table
# After training an ML model, use it to predict ALL general pairs
# and create a fast lookup hash

# For now, use our current rule-based system to generate the lookup
# Later: replace with ML model predictions

say "Loading generals...";
my @general_files = glob("share/collections/data/generals/*.yaml");
my @general_names = map { 
  my $name = $_;
  $name =~ s{.*/}{};
  $name =~ s{\.yaml$}{};
  $name;
} @general_files;

say "Found " . scalar(@general_names) . " generals";

# Generate conflict lookup
# Structure: { "General1|General2" => 1 } for conflicts
# Compatible pairs are omitted (absence = compatible)

my %conflicts;
my $total_pairs = 0;
my $conflict_count = 0;

say "\nGenerating conflict lookup...";
say "(This would use ML model predictions in production)";

for my $i (0..$#general_names) {
  for my $j ($i+1..$#general_names) {
    my $g1 = $general_names[$i];
    my $g2 = $general_names[$j];
    $total_pairs++;
    
    # TODO: Replace this with ML model prediction
    # For now, use placeholder - you'd call:
    # my $conflicts = predict_conflict($g1, $g2);
    # if ($conflicts) { ... }
    
    # Placeholder: mark as conflict if in conflicting_pairs file
    # (This is just for demonstration)
  }
  
  if (($i + 1) % 50 == 0) {
    say "  Processed " . ($i + 1) . " generals...";
  }
}

say "\nTotal pairs evaluated: $total_pairs";
say "Conflicts found: $conflict_count";
say "Compatible pairs: " . ($total_pairs - $conflict_count);

# Save as JSON for fast loading
my $output = {
  generated_at => scalar(localtime),
  total_generals => scalar(@general_names),
  total_pairs => $total_pairs,
  conflicts => \%conflicts,
};

say "\nSaving to share/conflict_lookup.json...";
open my $fh, '>', 'share/conflict_lookup.json' or die $!;
print $fh JSON::MaybeXS->new(pretty => 1, canonical => 1)->encode($output);
close $fh;

say "Done!";
say "\nUsage in code:";
say '  my $lookup = decode_json(path("share/conflict_lookup.json")->slurp);';
say '  my $key = join("|", sort($g1->name, $g2->name));';
say '  my $conflicts = $lookup->{conflicts}{$key} // 0;';
