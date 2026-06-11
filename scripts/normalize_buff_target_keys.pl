#!/usr/bin/env perl
# Normalize buff troop-targeting keys in share/collections/data to the
# canonical `targetedType:` spelling. Historical data used `troop:` and
# `class:` interchangeably; consumers (Model::Buff::from_hash,
# Service::PDL::Compiler::_buff_targeted_type) normalize all spellings,
# but raw-YAML consumers have mis-bucketed buffs because of the variants.
#
# Safety model:
#   1. Parse each file and assert every hash containing `troop`/`class`
#      is buff-shaped (has `attribute`) and lacks `targetedType` already.
#   2. Rewrite only key lines textually, preserving formatting.
#   3. Re-parse and deep-compare against the original structure with the
#      same rename applied programmatically; refuse to write on mismatch.
use v5.42.0;
use utf8;
use experimental 'signatures';
use File::Find;
use YAML::XS qw(Load);
use Data::Dumper;

my $data_dir = shift // 'share/collections/data';
die "no such dir: $data_dir\n" unless -d $data_dir;

my @keys_to_rename = qw(troop class targetedTroops);
my $key_alt = join '|', @keys_to_rename;

sub slurp ($path) {
  open my $fh, '<:raw', $path or die "read $path: $!";
  local $/;
  return scalar <$fh>;
}

# Walk a parsed structure; rename troop/class keys to targetedType in
# place, dying unless every such hash looks like a buff entry.
sub rename_in_structure ($node, $path) {
  if (ref $node eq 'HASH') {
    for my $key (@keys_to_rename) {
      next unless exists $node->{$key};
      die "$path: hash with '$key' lacks 'attribute' (not a buff?): "
        . Dumper($node)
        unless exists $node->{attribute};
      die "$path: hash has both '$key' and 'targetedType': " . Dumper($node)
        if exists $node->{targetedType};
      $node->{targetedType} = delete $node->{$key};
    }
    rename_in_structure($_, $path) for values %$node;
  }
  elsif (ref $node eq 'ARRAY') {
    rename_in_structure($_, $path) for @$node;
  }
}

sub canonical ($data) {
  local $Data::Dumper::Sortkeys = 1;
  local $Data::Dumper::Indent   = 1;
  return Dumper($data);
}

my (@changed, @failed);
find({
  no_chdir => 1,
  wanted   => sub {
    my $path = $File::Find::name;
    return unless -f $path && $path =~ /\.ya?ml$/;
    my $text = slurp($path);
    return unless $text =~ /^(\s*(?:-\s+)?)(?:$key_alt):(\s|$)/m;

    my $orig = eval { Load($text) };
    if ($@) { push @failed, "$path: parse error: $@"; return; }

    my $expected = eval { rename_in_structure($orig, $path); $orig };
    if ($@) { push @failed, $@; return; }

    (my $new_text = $text)
      =~ s/^(\s*(?:-\s+)?)(?:$key_alt):(\s|$)/$1targetedType:$2/mg;

    my $reparsed = eval { Load($new_text) };
    if ($@) { push @failed, "$path: rewritten text fails to parse: $@"; return; }
    if (canonical($reparsed) ne canonical($expected)) {
      push @failed, "$path: rewrite changed semantics beyond key rename";
      return;
    }

    open my $out, '>:raw', $path or die "write $path: $!";
    print {$out} $new_text;
    close $out;
    push @changed, $path;
  },
}, $data_dir);

say "rewrote: $_" for @changed;
say scalar(@changed), " file(s) rewritten";
if (@failed) {
  say STDERR "FAILED (left untouched):";
  say STDERR "  $_" for @failed;
  exit 1;
}
