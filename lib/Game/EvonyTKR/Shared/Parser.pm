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
  method normalize_buff ($buff_hash) {
    $self->logger->debug("Processing Prolog fragment: " . Data::Printer::np($buff_hash));

    my @result;

    unless (ref $buff_hash eq 'HASH') {
      $self->logger->warn("Expected buff as hash, got: $buff_hash");
      return;
    }

    my $attribute_atom  = $buff_hash->{attribute};
    my $troop_atom      = $buff_hash->{troop};
    my $value           = $buff_hash->{value};
    my $conditions_list = $buff_hash->{conditions} // [];

    # Decode atom into full names
    my $attribute  = $self->atom_to_attribute($attribute_atom);
    my $troop_type = $self->atom_to_troop($troop_atom);

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

    # Handle condition normalization
    foreach my $cond (@$conditions_list) {
      $self->logger->debug("Processing condition: '$cond'");
      my $normalized = $self->normalize_condition_case($cond);
      if (defined $normalized) {
        my $r = $buff->set_condition($normalized);
        $self->logger->debug(
          "Added buff condition: $normalized ; set_condition result: $r");
      } else {
        $self->logger->warn("Could not normalize condition: '$cond'");
      }
    }

    push @result, $buff;

    return @result;
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
    $grammar->spew_utf8(join("\n",sort(

      # Generate DCG rules for troops
      (
        map {

          my $t      = lc($_);
          my @l      = split(/ /, $t);
          my $base = $l[1] =~ s/s$//r;
          my @s;
          foreach my $index ( 0..$#l ) {
            $s[$index] = $index ?  $base : $l[$index] ;
          }
          my $atom   = join('_',    @l);
          my $tokens = join('], [', @l);

          my $stokens = join ('], [', @s);
          my @rules;
          push @rules, sprintf('troop(%s) --> [%s].', $atom, $tokens);
          push @rules, sprintf('troop(%s) --> [%s].', $atom, $stokens);
          @rules;
        } values $self->TroopTypeValues->%*
      ),

      # Generate DCG rules for attributes
      (
        map {
          my $t      = lc($_);
          if($_ =~ /^SubCity (.+)$/i) {
            my $base_attr = lc($1);
            my @l = split(/ /, $base_attr);
            my $base_atom = join('_', @l);
            my $subcity_atom = "subcity_" . $base_atom;

            sprintf('subcity_attribute(%s) --> [%s].', $base_atom, join('], [', @l));
          }else {
            my @l      = split(/ /, $t);
            my $atom   = join('_',    @l);
            my $tokens = join('], [', @l);
            sprintf('attribute(%s) --> [%s].', $atom, $tokens);
          }
        } $self->AttributeValues->@*
      ),

      # Keep validation predicates for conditions
      (
        map {
          my $t = lc($_);
          my @l = split(/ /, $t);
          sprintf('condition([%s]).', join(", ", map {"'$_'"} @l));
        } $self->BuffConditionValues->@*
      ),
      (
        map {
          my $t = lc($_);
          my @l = split(/ /, $t);
          sprintf('condition([%s]).', join(", ", map {"'$_'"} @l));
        } $self->DebuffConditionValues->@*
      ),

     (
       map {
         my $key = $_;
         my $val = $self->mapped_conditions->{$key};
         my @lhs_tokens = map { lc($_) } split(/ /, $key);
         my $rhs_atom   = lc($val);
         sprintf('condition_syn([%s], \'%s\').', join(', ', map { "'$_'" } @lhs_tokens), $rhs_atom);
       } sort {
      		my @aparts = split(/ /, $a);
      		my @bparts = split(/ /, $b);
      		return scalar(@aparts) <=> scalar(@bparts);
		    } keys %{ $self->mapped_conditions }
     ),

     (
       map {
         my $key = $_;
         my $val = $self->mapped_conditions->{$key};
         my @lhs_tokens = map { lc($_) } split(/ /, $key);
         my $rhs_atom   = lc($val);
         sprintf("condition_syn_key([%s]).", join(', ', map { "'$_'" } @lhs_tokens));
       } sort {
      		my @aparts = split(/ /, $a);
      		my @bparts = split(/ /, $b);
      		return scalar(@aparts) <=> scalar(@bparts);
		    } keys %{ $self->mapped_conditions }
     ),

    )));
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
    my $quoted_text = "'" . $text . "'";  # single-quote to create an atom

    my ($in_fh, $out_fh, $err_fh) = (undef, gensym, gensym);
    my $cmd = [
      'swipl', '-q', '-g', 'main', '-s',
      $self->distDir->child('prolog/Game/EvonyTKR/Shared/buff_parser.pl')
    ];
    my $pid = open3($in_fh, $out_fh, $err_fh, @$cmd);

    print $in_fh "$quoted_text.\n";    # Send quoted string
    close $in_fh;

    my @err = <$err_fh>;
    my @out = <$out_fh>;
    waitpid $pid, 0;

    # Debug logging
    foreach my $error_line (@err){
      $self->logger->error("STDERR: $error_line");
    }
    foreach my $stdout_line (@out ) {
      $self->logger->debug("STDOUT: $stdout_line");
    }


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
      } elsif ($line =~ /^buff\(/) {
        push @buff_fragments, $line;  # optional: keep legacy Prolog format
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
    } else {
        $self->logger->warn("No buff(...) line found; skipping JSON extraction");
    }

    $self->logger->debug(sprintf(
      'found %d buffs: %s',
      scalar @buff_fragments,
      join(', ', @buff_fragments)
    ));
    if(scalar@json_lines != scalar @buff_fragments) {
      $self->logger->warn(sprintf('uneven output detected: %s versus %s, are there missing output lines?',
        scalar @json_lines, scalar @buff_fragments
      ));
    }
    return @json_lines;
  }
}
1;
