use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require Path::Tiny;
require Readonly;
require JSON::PP;
require YAML::PP;
require Game::EvonyTKR::Model::Buff;
require Game::EvonyTKR::Model::Buff::Value;

class Game::EvonyTKR::Shared::Parser : isa(Game::EvonyTKR::Shared::Constants) {
  use List::AllUtils qw( first all any none uniq );
  use IPC::Open3;
  use open qw(:std :utf8);
  use Symbol 'gensym';
  use namespace::autoclean;

  # Simplified normalize_buff - parses Prolog output format
  method normalize_buff ($buff_hash) {
    $self->logger->debug(
      "Processing Prolog fragment: " . Data::Printer::np($buff_hash));

    my @result;
    my $is_debuff = 0;

    unless (ref $buff_hash eq 'HASH') {
      $self->logger->warn("Expected buff as hash, got: $buff_hash");
      return;
    }

    my $attribute_atom  = $buff_hash->{attribute};
    my $troop_atom      = $buff_hash->{troop};
    my $value           = $buff_hash->{value};
    my $conditions_list = $buff_hash->{conditions} // [];

# Prolog cannot parse the negative out of the numbers, we must do so here.
# that also means that it sometimes does not set the debuff conditions correctly.
    if ($value < 0) {
      $self->logger->info("detected a debuff to convert");
      $is_debuff = 1;
      $value     = abs($value);
      @{$conditions_list} = grep { length($_) > 0 } @{$conditions_list};
      if (scalar @{$conditions_list}) {
        my $is_PvM = 0;

        foreach my $c (@{$conditions_list}) {

          if (exists $self->BuffConditionValues->{$c}) {
            if ( $self->BuffConditionValues->{$c}->{PvM}
              && (not $self->BuffConditionValues->{$c}->{Attacking})
              && (not $self->BuffConditionValues->{$c}->{Defense})
              && (not $self->BuffConditionValues->{$c}->{Mayor})
              && (not $self->BuffConditionValues->{$c}->{Wall})) {
              # this particular disunion, while not exhaustive,
              # is sufficient to ensure that we have eliminated
              # the possibility this is a PvP buff. While
              # there are conditions that overlap, only a
              # definitively PvM buff can pass here.
              $c =~ s/against[_ ]monsters/monsters/i;
              # a duplicate won't hurt, missing the value will.
              # there is a possibility that at some future
              # point a second value other than 'against monsters'
              # might pass this test.
              push @{$conditions_list}, 'Monsters';
              $is_PvM = 1;
              last;
            }
          }
        }
        if (not $is_PvM) {

        }
      }
      else {
        # if there are NO conditions already,
        # then it *cannot* be a PvM buff.
        push @{$conditions_list}, 'Enemy';
      }
    }

    # Decode atom into full names
    my $attribute = $self->atom_to_attribute($attribute_atom);

    # Create the buff object
    my $buff_value = Game::EvonyTKR::Model::Buff::Value->new(
      number => $value,
      unit   => 'percentage'
    );

    my $buff = Game::EvonyTKR::Model::Buff->new(
      attribute => $attribute,
      value     => $buff_value,
      passive   => 0,
    );

    if (ref($troop_atom) eq 'ARRAY') {
      $troop_atom = $troop_atom->[0];
    }
    if (length($troop_atom)) {
      $self->logger->debug("troop_atom is '$troop_atom'");
      my $troop_type = $self->atom_to_troop($troop_atom);
      $buff->set_target($troop_type);
    }

    # Handle condition normalization
    foreach my $cond (@$conditions_list) {
      if (ref $cond eq 'ARRAY') {
        $cond = join(' ', @{$cond});
      }
      my $r;
      if ($cond eq 'enemy') {
        $is_debuff = 1;
        $r         = $buff->set_condition('Enemy');
        $self->logger->debug(
          "Added debuff condition: 'Enemy' ; set_condition result: $r");
      }
      elsif ($cond eq 'monsters') {
        $is_debuff = 1;
        $r         = $buff->set_condition('Monsters');
        $self->logger->debug(
          "Added debuff condition: 'Monsters' ; set_condition result: $r");
      }
      elsif ($cond =~ /against[_ ]monsters/i && $is_debuff) {
        $cond = 'Monsters';
        $r    = $buff->set_condition($cond);
        $self->logger->debug(
          "Added buff condition: $cond ; set_condition result: $r");
      }
      else {
        $self->logger->debug("Processing condition: '$cond'");
        if (length($cond) == 0) {
          next;
        }
        my $normalized = $self->normalize_condition_case($cond);
        if (defined $normalized) {
          $r = $buff->set_condition($normalized);
          $self->logger->debug(
            "Added buff condition: $normalized ; set_condition result: $r");
        }
        else {
          $self->logger->warn("Could not normalize condition: '$cond'");
        }
      }
    }

    return $buff;
  }

  method normalize_condition_case($prolog_condition) {

    $self->logger->debug(sprintf(
      'normalize_condition_case called with: "%s"', $prolog_condition));

    # Handle the new underscore-based condition format
    # Convert underscore atoms back to display format
    if ($prolog_condition =~ /^[a-z_]+$/ && $prolog_condition =~ /_/) {
      # This looks like an underscore-based atom from Prolog
      my $display_condition = $prolog_condition;
      $display_condition =~ s/_/ /g;    # Convert underscores to spaces

      $self->logger->debug(
        "Converted underscore atom: '$prolog_condition' -> '$display_condition'"
      );

      # Try to map to proper case using existing constants
      my $mapped = $self->string_to_condition($display_condition);
      if ($mapped) {
        $self->logger->debug(
          "string_to_condition mapped: '$display_condition' -> '$mapped'");
        return $mapped;
      }

      # Fallback: capitalize each word
      my $capitalized =
        join(' ', map { ucfirst($_) } split(/ /, $display_condition));
      $self->logger->debug(
        "Using capitalized fallback: '$display_condition' -> '$capitalized'");
      return $capitalized;
    }

    # Legacy handling for old format (space-separated strings)
    # Create a mapping from lowercase to proper case
    my %condition_map;

    # Build map from existing constants (both buff and debuff)
    foreach my $const (
      keys %{ $self->BuffConditionValues },
      @{ $self->DebuffConditionValues }
    ) {
      $condition_map{ lc($const) } = $const;
    }

    $self->logger->debug(
      "Built condition map with " . scalar(keys %condition_map) . " entries");

    # Handle multi-word conditions that might have different formatting
    my $lower_condition = lc($prolog_condition);
    $self->logger->debug("Lowercase condition: '$lower_condition'");

    # Direct lookup first
    if (exists $condition_map{$lower_condition}) {
      my $result = $condition_map{$lower_condition};
      $self->logger->debug(
        "Direct lookup found: '$lower_condition' -> '$result'");
      return $result;
    }

    # Fallback: try string_to_condition for mapping
    my $mapped = $self->string_to_condition($prolog_condition);
    if ($mapped) {
      $self->logger->debug(
        "string_to_condition mapped: '$prolog_condition' -> '$mapped'");
      return $mapped;
    }

    # Last resort: return original with first letter capitalized
    my $capitalized = ucfirst($prolog_condition);
    $self->logger->debug(
      "Using capitalized fallback: '$prolog_condition' -> '$capitalized'");
    return $capitalized;
  }

  # Helper methods to convert atoms back to display format
  method atom_to_attribute($atom) {
    my $attr = $atom;
    $attr =~ s/_/ /g;    # Convert underscores to spaces
    return $self->string_to_attribute($attr) || ucfirst($attr);
  }

  method atom_to_troop($atom) {
    my $troop = $atom;
    $troop =~ s/_/ /g;    # Convert underscores to spaces
    return $self->string_to_trooptype($troop) || ucfirst($troop);
  }

  method generate_grammar {
    my $grammar = Path::Tiny::path(
      'share/prolog/Game/EvonyTKR/Shared/EvonyBuffDictionary.pl');
    my @rules;

    # Generate DCG rules for troops
    push @rules, map {
      my $t    = lc($_);
      my @l    = split(/ /, $t);
      my $base = $l[1] =~ s/s$//r;
      my @s;
      foreach my $index (0 .. $#l) {
        $s[$index] = $index ? $base : $l[$index];
      }
      my $atom   = join('_',    @l);
      my $tokens = join('], [', @l);

      my $stokens = join('], [', @s);
      my @r2;
      push @r2, sprintf('troop(%s) --> [%s].', $atom, $tokens);
      push @r2, sprintf('troop(%s) --> [%s].', $atom, $stokens);
      @r2;
    } values $self->TroopTypeValues->%*;

    # Generate DCG rules for attributes
    push @rules, map {
      my $t = lc($_);
      if ($_ =~ /^SubCity (.+)$/i) {
        my $base_attr    = lc($1);
        my @l            = split(/ /, $base_attr);
        my $base_atom    = join('_', @l);
        my $subcity_atom = "subcity_" . $base_atom;

        sprintf('subcity_attribute(%s) --> [%s].',
          $subcity_atom, join('], [', @l));
      }
      else {
        my @l      = split(/ /, $t);
        my $atom   = join('_',    @l);
        my $tokens = join('], [', @l);
        my @r2;
        push @r2, sprintf('attribute(%s) --> [%s].', $atom, $tokens);

        if (scalar @l > 1 && $l[1] eq 'to') {
          $l[1] = 'into';

          $tokens = join('], [', @l);
          push @r2, sprintf('attribute(%s) --> [%s].', $atom, $tokens);
        }
        @r2;
      }
    } $self->AttributeValues->@*;

    push @rules, map {
      my $key = $_;
      $self->logger->debug("attribute alias key is '$key'");
      my $attr = $self->MappedAttributeNames->{$key};
      $self->logger->debug("attribute for alias '$key' is '$attr'");

      $attr = lc($attr);
      my @term      = split(/ /, $attr);
      my $base_atom = join('_', @term);
      my $val       = lc($key);
      my @synonym   = split(/ /, $val);
      foreach my $s (@synonym) {
        $s =~ s/(.*)/[$1]/;
        $s =~ s/\’//g;
      }
      if ($key =~ /^(?:sub|mayor)/i) {
        sprintf(
          'subcity_attribute(%s) --> %s.',
          join('_',  @term),
          join(', ', @synonym)
        );
      }
      else {
        sprintf('attribute(%s) --> %s.', join('_', @term),
          join(', ', @synonym));
      }
    } keys %{ $self->MappedAttributeNames };

    # Keep validation predicates for conditions
    push @rules, map {
      my $t      = lc($_);
      my @l      = split(/ /, $t);
      my $atom   = join('_',    @l);
      my $tokens = join('], [', @l);
      my @r2;
      push @r2, sprintf('condition(%s) --> [%s].', $atom, $tokens);

      if (scalar @l > 1 && $l[1] eq 'to') {
        $l[1] = 'into';

        $tokens = join('], [', @l);
        push @r2, sprintf('condition(%s) --> [%s].', $atom, $tokens);
      }
      @r2;
    } keys %{ $self->BuffConditionValues };

    push @rules, map {
      my $t      = lc($_);
      my @l      = split(/ /, $t);
      my $atom   = join('_',    @l);
      my $tokens = join('], [', @l);
      my @r2;
      push @r2, sprintf('condition(%s) --> [%s].', $atom, $tokens);

      if (scalar @l > 1 && $l[1] eq 'to') {
        $l[1] = 'into';

        $tokens = join('], [', @l);
        push @r2, sprintf('condition(%s) --> [%s].', $atom, $tokens);
      }
      @r2;
    } $self->DebuffConditionValues->@*;

    push @rules, map {
      my $key = $_;
      $self->logger->debug("condition alias key is '$key'");
      my $attr = $self->mapped_conditions->{$key};
      $self->logger->debug("condition for alias '$key' is '$attr'");

      $attr = lc($attr);
      my @term      = split(/ /, $attr);
      my $base_atom = join('_', @term);
      my $val       = lc($key);
      my @synonym   = split(/ /, $val);
      foreach my $s (@synonym) {
        $s =~ s/(.*)/[$1]/;
        $s =~ s/[\x{2018}\x{2019}]/'/g;
      }

      sprintf('condition(%s) --> %s.', join('_', @term), join(', ', @synonym));
    } keys %{ $self->mapped_conditions };

    foreach my $r (@rules) {
      $r = lc($r);
    }
    @rules = uniq(sort(@rules));
    # Group by predicate name
    my %grouped;
    foreach my $rule (@rules) {
      my ($head) = $rule =~
        /^(\w+)\(/;    # Extract predicate name (e.g., 'condition', 'attribute')
      push @{ $grouped{$head} }, $rule;
    }

    # Sort each group by reverse line length, then flatten all groups
    @rules = map {
      my $group = $_;
      sort { length($b) <=> length($a) } @$group
    } @grouped{ sort keys %grouped }
      ;    # Maintain alphabetical order of predicate groups
    $grammar->spew_utf8(join("\n", @rules));

  }

  method tokenize_buffs ($text) {

    # remove the following for prolog:
    #\x{0022}    # ASCII double quote "
    #\x{0027}    # ASCII single quote '
    #\x{2018}    # Unicode left single quote '
    #\x{2019}    # Unicode right single quote '
    #\x{201C}    # Unicode left double quote "
    #\x{201D}    # Unicode right double quote "
    #\x{0060}    # ASCII backtick
    # \x{00B4}    # ASCII acute accent ´
    $text =~
      s/[\x{0022}\x{0027}\x{2018}\x{2019}\x{201C}\x{201D}\x{0060}\x{00B4}]//gx
      ;    # remove apostrophies entirely for prolog to parse.
    $text = lc($text);    # prolog requires all lower case.

    # prolog will tokenize hyphens even in words.
    # none of my constants are built around in-word hyphens.
    $text =~ s/\bin-([a-zA-Z]+)\b/in $1/g;

    $self->logger->debug("cleaned text is '$text'");

    # Send the raw string instead of tokenizing
    my $quoted_text = "'" . $text . "'";    # single-quote to create an atom

    my ($in_fh, $out_fh, $err_fh) = (undef, gensym, gensym);
    my $cmd = [
      'swipl', '-q', '-g', 'main', '-s',
      $self->distDir->child('prolog/Game/EvonyTKR/Shared/buff_parser.pl')
    ];
    my $pid = open3($in_fh, $out_fh, $err_fh, @$cmd);
    binmode($in_fh,  ":utf8");              # Write to Prolog's stdin
    binmode($out_fh, ":utf8");              # Read from Prolog's stdout
    binmode($err_fh, ":utf8");              # Read from Prolog's stderr

    print $in_fh "$quoted_text.\n";         # Send quoted string
    close $in_fh;

    my @err = <$err_fh>;
    my @out = <$out_fh>;
    waitpid $pid, 0;

    # Debug logging
    foreach my $error_line (@err) {
      $self->logger->error("STDERR: $error_line");
    }
    # Note, we will output STDOUT after parsing it below.

    my $parsed = join('', @out);
    $parsed =~ s/^\s+|\s+$//g;
    $self->logger->info(sprintf('parsed text is -- %s --', $parsed));

    # Extract just the buff list (last non-debug line)
    my @buff_fragments;
    my @json_lines;
    my @json_buffer;

    my $last_buff_index = -1;
    for (my $i = 0; $i < @out; $i++) {
      if ($out[$i] =~ /^buff\(/) {
        $last_buff_index = $i;
      }
    }

    foreach my $line (@out) {
      chomp $line;
      if ($line =~ /^DEBUG:/) {
        $self->logger->debug("STDOUT: $line");
      }
      elsif ($line =~ /^buff\(/) {
        push @buff_fragments, $line;    # optional: keep legacy Prolog format
      }
    }

    if ($last_buff_index >= 0) {
      for my $line (@out[($last_buff_index + 1) .. $#out]) {
        push @json_buffer, $line;
      }

      # Step 3: Parse buffered JSON lines
      my $json_text = join("\n", @json_buffer);
      $json_text =~ s/\}\s+{/},{/g;
      eval {
        $json_text = "[$json_text]";
        $self->logger->debug("json text is -- $json_text -- ");
        my $decoded = JSON::PP->new(utf8 => 1)->decode($json_text);
        push @json_lines, @$decoded;
      };
      if ($@) {
        $self->logger->warn("Failed to parse JSON block: $@");
      }
    }
    else {
      $self->logger->warn("No buff(...) line found; skipping JSON extraction");
    }

    $self->logger->debug(sprintf(
      'found %d buffs: %s',
      scalar @buff_fragments,
      join(', ', @buff_fragments)
    ));
    if (scalar @json_lines != scalar @buff_fragments) {
      $self->logger->warn(sprintf(
        'uneven output detected: %s versus %s, are there missing output lines?',
        scalar @json_lines,
        scalar @buff_fragments
      ));
    }
    return @json_lines;
  }
}
1;
