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
  use List::AllUtils qw( first all any none );
  use IPC::Open3;
  use Symbol 'gensym';
  use namespace::autoclean;

  # Simplified normalize_buff - parses Prolog output format
  method normalize_buff ($prolog_fragment) {
    $self->logger->debug("Processing Prolog fragment: $prolog_fragment");

    # Parse Prolog buff format: buff(attribute,troop,value,conditions)
    if ($prolog_fragment =~ /^buff\(([^,]+),([^,]+),(\d+),\[([^\]]*)\]\)$/) {
      my ($attr_atom, $troop_atom, $value, $conditions_str) = ($1, $2, $3, $4);

      # Convert atoms back to display format
      my $attribute  = $self->atom_to_attribute($attr_atom);
      my $troop_type = $self->atom_to_troop($troop_atom);

      # Parse conditions list
      my @conditions;
      if ($conditions_str) {
        $self->logger->debug("Raw conditions string: '$conditions_str'");

# Handle mixed quoted and unquoted conditions: attacking,'brings a dragon','brings a spiritual beast'
# Split on commas first, then handle quoted vs unquoted
        my @condition_parts = split(/,/, $conditions_str);

        foreach my $part (@condition_parts) {
          $part =~ s/^\s+|\s+$//g;    # trim whitespace

          if ($part =~ /^'([^']+)'$/) {
            # Quoted condition: 'brings a dragon'
            push @conditions, $1;
            $self->logger->debug("Found quoted condition: '$1'");
          }
          elsif ($part =~ /^[a-z_]+$/) {
            # Unquoted single word condition: attacking
            push @conditions, $part;
            $self->logger->debug("Found unquoted condition: '$part'");
          }
          else {
            $self->logger->warn("Unrecognized condition format: '$part'");
          }
        }

        $self->logger->debug("Final parsed conditions: ["
            . join(', ', map {"'$_'"} @conditions)
            . "]");
      }
      else {
        $self->logger->debug("No conditions string provided");
      }

      # Create the buff object
      my $buff_value = Game::EvonyTKR::Model::Buff::Value->new(
        number => $value,
        unit   => 'percentage'
      );

      my $buff = Game::EvonyTKR::Model::Buff->new(
        attribute    => $attribute,
        value        => $buff_value,
        targetedType => $troop_type || '',
        passive      => 0,
      );

      # Add conditions
      foreach my $condition (@conditions) {
        $self->logger->debug("Processing condition: '$condition'");
        # Map lowercase Prolog output to proper Constants format
        my $normalized_condition = $self->normalize_condition_case($condition);
        $self->logger->debug(
          "Normalized condition: '$condition' -> '$normalized_condition'");
        my $result = $buff->set_condition($normalized_condition);
        $self->logger->debug("set_condition result: $result");
      }

      # Log final buff conditions
      my @final_conditions = $buff->conditions();
      $self->logger->debug("Final buff conditions: ["
          . join(', ', map {"'$_'"} @final_conditions)
          . "]");

      return ($buff);
    }
    else {
      $self->logger->error("Could not parse Prolog fragment: $prolog_fragment");
      return ();
    }
  }

  method normalize_condition_case($prolog_condition) {
    $self->logger->debug(
      "normalize_condition_case called with: '$prolog_condition'");

    # Create a mapping from lowercase to proper case
    my %condition_map;

    # Build map from existing constants (both buff and debuff)
    foreach my $const (@{ $self->BuffConditionValues },
      @{ $self->DebuffConditionValues }) {
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

  method tokenize_buffs ($text) {

    my $grammar = Path::Tiny::path(
      'share/prolog/Game/EvonyTKR/Shared/EvonyBuffDictionary.pl');
    $grammar->spew_utf8(join(
      "\n",
      # Generate DCG rules for troops
      (
        map {
          my $t      = lc($_);
          my @l      = split(/ /, $t);
          my $atom   = join('_',    @l);
          my $tokens = join('], [', @l);
          sprintf('troop(%s) --> [%s].', $atom, $tokens);
        } values $self->TroopTypeValues->%*
      ),

      # Generate DCG rules for attributes
      (
        map {
          my $t      = lc($_);
          my @l      = split(/ /, $t);
          my $atom   = join('_',    @l);
          my $tokens = join('], [', @l);
          sprintf('attribute(%s) --> [%s].', $atom, $tokens);
        } $self->AttributeValues->@*
      ),

      # Keep validation predicates for conditions
      (
        map {
          my $t = lc($_);
          my @l = split(/ /, $t);
          sprintf('condition([%s]).', join(", ", map {"\"$_\""} @l));
        } $self->BuffConditionValues->@*
      ),
      (
        map {
          my $t = lc($_);
          my @l = split(/ /, $t);
          sprintf('condition([%s]).', join(", ", map {"\"$_\""} @l));
        } $self->DebuffConditionValues->@*
      ),
    ));
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
    $self->logger->debug("cleaned text is '$text'");

    # Send the raw string instead of tokenizing
    my $quoted_text = "\"$text\"";    # Quote the string for Prolog

    my ($in_fh, $out_fh, $err_fh) = (undef, gensym, gensym);
    my $cmd = [
      'swipl', '-s',
      $self->distDir->child('prolog/Game/EvonyTKR/Shared/buff_parser.pl')
    ];
    my $pid = open3($in_fh, $out_fh, $err_fh, @$cmd);

    print $in_fh "$quoted_text.\n";    # Send quoted string
    close $in_fh;

    my @err = <$err_fh>;
    my @out = <$out_fh>;
    waitpid $pid, 0;

    # Debug logging
    $self->logger->info("STDERR: " . join('', @err));
    $self->logger->info("STDOUT: " . join('', @out));

    my $parsed = join('', @out);
    $parsed =~ s/^\s+|\s+$//g;
    $self->logger->info(sprintf('parsed text is %s', $parsed));

    # Extract just the buff list (last non-debug line)
    my @output_lines = grep { $_ !~ /^DEBUG:/ } @out;
    my $single       = join('', @output_lines);
    my @buff_fragments;
    if ($single =~ /\[buff/) {
      if($single !~ /^\[(buff\(.+?\))(,)?/) {
        $self->logger->error("buff_fragments does not match regex!!" . $single);
      }
      while ($single =~ s/^\[?(buff\(.+?\))(,)?//) {
        my $f = $2;
        $self->logger->debug("found frag $f");
        push @buff_fragments, $1;
      }
    }
    else {
      push @buff_fragments, $single;
    }

    $self->logger->debug(
      "Split into " . scalar(@buff_fragments) . " buff fragments: " . join('----',@buff_fragments ));
    return @buff_fragments;
  }
}
1;
