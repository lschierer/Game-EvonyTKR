use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require Path::Tiny;
require Readonly;
require JSON::PP;
require YAML::PP;
require AI::Prolog;
require Game::EvonyTKR::Model::Buff;
require Game::EvonyTKR::Model::Buff::Value;

class Game::EvonyTKR::Shared::Parser : isa(Game::EvonyTKR::Shared::Constants) {
  use List::AllUtils qw( first all any none );
  use IPC::Open3;
  use Symbol 'gensym';
  use namespace::autoclean;

  method noramlize_attributes ($fragment) {
    $self->logger->debug("noramlize_attributes has fragment '$fragment'");
    # ✅ Attribute types (e.g. "Attack", "Defense", etc.)
    my @attributes;
    foreach my $attr ($self->AttributeValues->@*) {
      if ($fragment =~ /\b\Q$attr\E\b/i) {
        push @attributes, $attr;
      }
    }

    # Special handling for "by X%" patterns created by restructuring
    if ($fragment =~ /(.+?)\s+by\s+\d+%/i) {
      my $before_by = $1;
      # Try to extract the attribute part from the end of the phrase before "by"
      # Look for common attribute patterns
      my @potential_attrs;

      # Try different attribute extraction patterns
      if ($before_by =~ /\b(wounded\s+into\s+death\s+rate)\s*$/i) {
        push @potential_attrs, $1;
      } elsif ($before_by =~ /\b([\w\s]*(?:attack|defense|hp|speed|capacity|size|rate))\s*$/i) {
        push @potential_attrs, $1;
      }

      foreach my $potential_attr (@potential_attrs) {
        $potential_attr =~ s/^\s+|\s+$//g; # trim whitespace
        my $mapped = $self->string_to_attribute($potential_attr);
        if ($mapped && none { $_ eq $mapped } @attributes) {
          push @attributes, $mapped;
          $self->logger->debug("Mapped 'by' pattern attribute '$potential_attr' → '$mapped'");
        }
      }
    }

    # Also try to detect mapped synonyms
    foreach my $phrase ($fragment =~ /\b[\w\s']+\b/g) {
      my $mapped = $self->string_to_attribute($phrase);
      push @attributes, $mapped
        if $mapped && none { $_ eq $mapped } @attributes;
      $self->logger->debug("Mapped attribute '$phrase' → '$mapped'") if $mapped;
    }

    # Fallback for "attack and defense" style combos
    if (!@attributes && $fragment =~ /\b(.+?) and (.+?)$/i) {
      foreach my $maybe_attr ($1, $2) {
        my $attr = $self->string_to_attribute($maybe_attr);
        push @attributes, $attr if $attr;
      }
    }
    $self->logger->debug(
      "I found the following potential attributes: " . join(', ', @attributes));

    my $in_subcity = ($fragment =~ /in Subordinate City/i);

    # keys must be all lower case.
    my %subcity_map = (
      'construction speed' => 'SubCity Construction Speed',
      'training speed'     => 'SubCity Training Speed',
      'training capacity'  => 'SubCity Training Capacity',
      'death to survival'  => 'SubCity Death to Survival',
      'gold production'    => 'SubCity Gold Production',
      'troop capacity'     => 'SubCity Troop Capacity',
    );
    if ($in_subcity) {
      @attributes =
        map { exists $subcity_map{ lc($_) } ? $subcity_map{ lc($_) } : $_ }
        @attributes;
    }
    return @attributes;
  }

  method normalize_conditions ($fragment) {
    my @conditions;
    my %seen_mapped;
    my @words = split ' ', $fragment;
    my %used_index;

    # longest phrases first
    for my $size (reverse 1 .. $self->words_condition_phrase) {
      for (my $i = 0; $i <= $#words - $size + 1; $i++) {

        # skip if any word in this window already used
        next if grep { $used_index{$_} } $i .. $i + $size - 1;

        my $candidate = join ' ', @words[$i .. $i + $size - 1];
        my $mapped    = $self->string_to_condition($candidate);
        $fragment =~ s/$candidate//;

        if ($mapped && !$seen_mapped{$mapped}++) {
          push @conditions, $mapped;
          $used_index{$_} = 1 for $i .. $i + $size - 1;

        }
      }
    }

    # Build the cleaned fragment from unused words
      my $cleaned = join ' ', grep { defined($_) } @words[grep { !$used_index{$_} } 0 .. $#words];

      return (\@conditions, $cleaned);
  }

  method process_normalized_buff ($value, $is_debuff, $desc, $fragment) {
    my @buffs;
    # Start with a working copy of the fragment
    my $working_fragment = $fragment;

    # some text patterns do not have a description section
    # distinct from the condition section of the fragment
    if ($desc eq '' or !$desc) {
      $desc = $fragment;
    }
    $self->logger->debug("process_normalized_buff has desc '$desc' and fragment '$fragment'");

    # Normalize conditions and strip them from the fragment
    my ($condition_ref, $cleaned_fragment) = $self->normalize_conditions($working_fragment);
    $working_fragment = $cleaned_fragment;
    if ($desc =~ /\benemy\b/i) {
      push(@$condition_ref, 'Enemy');
    }
    elsif ($desc =~ /\bReduces\b/i && $desc =~ /\bMonster\b/i) {
      push(@$condition_ref, 'Monsters');
    }
    if ($condition_ref && @$condition_ref) {
      foreach my $phrase (@$condition_ref) {
        $working_fragment =~ s/\b\Q$phrase\E\b//i;
      }
    }
    # Normalize attributes and strip them from the fragment
    my @attributes = $self->noramlize_attributes($working_fragment);
    if (scalar(@attributes) == 0) {
      $self->logger->error("No attributes detected in $fragment");
    }
    foreach my $attr (@attributes) {
      $working_fragment =~ s/\b\Q$attr\E\b//i;
    }

    # Detect troop classes from the cleaned-up fragment
    my @classes = grep {
      $working_fragment =~ /\b\Q$_\E\b/i
    } values $self->TroopTypeValues->%*;

    if (!@classes && $working_fragment =~ /troops[\u2019']/i) {
      $self->logger->debug("Detected generic 'troops’' phrase — buff applies to all troop types.");
    } else {
      $self->logger->debug('detectected classes ' . join(', ', @classes));
    }

    foreach my $attr (@attributes) {
      $self->logger->debug(sprintf(
        'attr: %s; class count: %s; condition count: %s',
        $attr, scalar(@classes), scalar(@$condition_ref)
      ));

      my $v = Game::EvonyTKR::Model::Buff::Value->new(
        number => $value,
        unit   => 'percentage',
      );

      if (scalar(@classes)) {
        foreach my $class (@classes) {
          my $b = Game::EvonyTKR::Model::Buff->new(
            value        => $v,
            targetedType => $class,
            attribute    => $attr,
            passive      => 0,
          );
          push @buffs, $b;
        }
      }
      else {
        my $b = Game::EvonyTKR::Model::Buff->new(
          value     => $v,
          attribute => $attr,
          passive   => 0,
        );
        push @buffs, $b;
      }
    }

    foreach my $c (@{$condition_ref}) {
      foreach my $b (@buffs) {
        $b->set_condition($c);
      }
    }
    return @buffs;
  }
  method normalize_buff ($fragment) {
    my @buffs;
    $self->logger->debug("fragment is $fragment");

    my $troopRE = sprintf("(Troops|%s)", join('|', map { "$_ Troops?" } values %{ $self->TroopTypeValues } ));
    my $attributeRE = sprintf('(%s)', join('|', @{ $self->AttributeValues } ));

    if ($fragment =~ /(?<desc>.+?) by (?<value>-?\d+)%?(?:\s+(?<cond>.+?))?\.?$/i) {
      my $value     = abs($+{value}) + 0;
      my $is_debuff = $+{value} =~ /^-/ ? 1 : 0;
      my $desc      = $+{desc};

      @buffs = $self->process_normalized_buff($value, $is_debuff, $desc, $fragment);

    } elsif ($fragment =~ /([-+])(\d+)/) {
      # Handle patterns with +/- values that don't match the "by X%" format

      # Special case: Complex patterns like "Marching Ground Troop and Mounted Troop Defense +10 and HP +10"
      # This pattern has multiple troops connected by "and"
      if ($fragment =~ /^(\w+)\s+((?:\w+\s+Troops?\s+and\s+)+\w+\s+Troops?)\s+(.+)$/) {
        my ($condition, $troop_list, $attr_part) = ($1, $2, $3);

        # Extract individual troop types
        my @troop_types = split(/\s+and\s+/, $troop_list);

        # Extract attributes with values
        my @attr_parts = split(/\s+and\s+/, $attr_part);
        my @attributes_with_values;

        foreach my $attr_part (@attr_parts) {
          if ($attr_part =~ /(.+?)\s+([-+])(\d+)/) {
            push @attributes_with_values, [$1, $2, $3];
          }
        }

        # Create buffs for each combination of troop type and attribute
        foreach my $troop_type (@troop_types) {
          # Convert to plural form
          $troop_type =~ s/\bGround Troop\b/Ground Troops/g;
          $troop_type =~ s/\bMounted Troop\b/Mounted Troops/g;
          $troop_type =~ s/\bRanged Troop\b/Ranged Troops/g;
          $troop_type =~ s/\bSiege Machine\b/Siege Machines/g;

          foreach my $attr_data (@attributes_with_values) {
            my ($attr, $sign, $value) = @$attr_data;
            my $full_desc = "$troop_type $attr";
            my $restructured_fragment = "$full_desc by $value% when $condition";

            if ($restructured_fragment =~ /(?<desc>.+?) by (?<value>-?\d+)%?(?:\s+(?<cond>.+?))?\.?$/i) {
              my $new_value = abs($+{value}) + 0;
              my $new_is_debuff = $+{value} =~ /^-/ ? 1 : 0;
              my $new_desc = $+{desc};
              push @buffs, $self->process_normalized_buff($new_value, $new_is_debuff, $new_desc, $restructured_fragment);
            }
          }
        }

      } else {
        # Original logic for simpler patterns
        # First, check if there's a leading condition before the first comma
        my $leading_condition = '';
        if ($fragment =~ /^([^,]+),/) {
          my $potential_condition = $1;
          # If this part doesn't contain +/-, it's likely a condition
          if ($potential_condition !~ /([-+])(\d+)/) {
            $leading_condition = $potential_condition;
          }
        }

        # Split on comma or " and " while preserving the original structure
        my @parts = split(/\s*(?:,|(?:\s+and\s+))\s*/, $fragment);

        # Extract troop context from the first part that has a value
        my $troop_context = '';
        my $condition_context = '';
        for my $part (@parts) {
          if ($part =~ /(.+?)\s+([-+])(\d+)/) {
            my $desc = $1;
            if ($desc =~ /(\w+)\s+((?:Ground|Mounted|Ranged|Siege)\s+Troops?)\s+(\w+)/) {
              $condition_context = $1;
              $troop_context = $2;
              last;
            }
          }
        }

        foreach my $part (@parts) {
          if ($part =~ /(.+?)\s+([-+])(\d+)/) {
            my ($desc, $sign, $value) = ($1, $2, $3);
            my $is_debuff = ($sign eq '-') ? 1 : 0;

            # Process this part normally
            $self->process_part($desc, $sign, $value, $leading_condition, \@buffs);

          } elsif ($part =~ /^([A-Za-z\s]+)$/) {
            # This part has no value - check if the next part has a value we can share
            my $attribute_only = $1;

            # Look for a value in the remaining parts
            # Find the current attribute's position first
            my $current_index = -1;
            for my $k (0..$#parts) {
              if ($parts[$k] eq $attribute_only) {
                $current_index = $k;
                last;
              }
            }

            my $processed = 0;

            # Look for the next value-bearing part after the current attribute
            for my $j ($current_index + 1 .. $#parts) {
              if ($parts[$j] =~ /(.+?)\s+([-+])(\d+)/) {
                my ($other_desc, $other_sign, $other_value) = ($1, $2, $3);

                # For shared values, reconstruct the full context using extracted troop context
                my $full_desc = $attribute_only;
                if ($troop_context) {
                  $full_desc = "$condition_context $troop_context $attribute_only";
                } elsif ($leading_condition) {
                  # If no troop context extracted, try to use leading condition context
                  if ($leading_condition =~ /(\w+)\s+((?:Ground|Mounted|Ranged|Siege)\s+Troops?)/) {
                    my ($cond, $troop) = ($1, $2);
                    $full_desc = "$cond $troop $attribute_only";
                  } elsif ($attribute_only =~ /^(Attack|Defense|HP)$/i && $leading_condition) {
                    # For generic attributes like "Troops Defense", don't add specific troop type
                    $full_desc = "$attribute_only";
                  }
                }

                # Share the value with this attribute
                $self->process_part($full_desc, $other_sign, $other_value, $leading_condition, \@buffs);
                $processed = 1;
                last; # Use the first value found after the current attribute
              }
            }

            # If no value found after current attribute, fall back to any available value
            if (!$processed && $current_index >= 0) {
              for my $j (0..$#parts) {
                next if $j == $current_index; # Skip the current attribute itself
                if ($parts[$j] =~ /(.+?)\s+([-+])(\d+)/) {
                  my ($other_desc, $other_sign, $other_value) = ($1, $2, $3);

                  # Only use this as fallback if it's a simple attribute
                  if ($other_desc =~ /^[A-Za-z\s]+$/) {
                    my $full_desc = $attribute_only;
                    if ($troop_context) {
                      $full_desc = "$condition_context $troop_context $attribute_only";
                    } elsif ($leading_condition && $attribute_only =~ /^(Attack|Defense|HP)$/i) {
                      $full_desc = "$attribute_only";
                    }

                    $self->process_part($full_desc, $other_sign, $other_value, $leading_condition, \@buffs);
                    last;
                  }
                }
              }
            }
          }
        }
      }
    }

    return @buffs;
  }

  method process_part ($desc, $sign, $value, $leading_condition, $buffs_ref) {
    my $is_debuff = ($sign eq '-') ? 1 : 0;

    # Restructure to match the "by X%" pattern that the first if clause expects
    my $restructured_fragment;

    # If we have a leading condition and this part doesn't contain a comma, combine them
    if ($leading_condition && $desc !~ /,/) {
      # For shared values, we need to reconstruct the troop type context
      # Check if the desc is just an attribute (like "Defense" or "HP")
      if ($desc =~ /^(Attack|Defense|HP|March Size|Rally Capacity|Training Speed|Construction Speed)$/i) {
        # This is likely a shared value case - find the troop context from the leading condition
        # Extract troop type from leading condition if it exists
        if ($leading_condition =~ /(\w+\s+(?:Ground|Mounted|Ranged|Siege)\s+Troops?)/) {
          my $troop_context = $1;
          $troop_context =~ s/\bGround Troop\b/Ground Troops/g;
          $troop_context =~ s/\bMounted Troop\b/Mounted Troops/g;
          $troop_context =~ s/\bRanged Troop\b/Ranged Troops/g;
          $troop_context =~ s/\bSiege Machine\b/Siege Machines/g;
          $desc = "$troop_context $desc";
        }
      }

      # Determine the appropriate condition based on the target
      my $condition_to_use = $leading_condition;
      if ($desc =~ /\bMonsters?\b/i) {
        # This is a debuff affecting monsters - use "Monsters" condition
        $condition_to_use = "Monsters";
      } elsif ($desc =~ /\bTroops?\b/i || $leading_condition) {
        # This is a buff affecting troops - map the leading condition appropriately
        $condition_to_use = $self->string_to_condition($leading_condition) || $leading_condition;
      }

      # Convert singular troop types to plural, but preserve "Troops" as-is for targetedType handling
      my $processed_desc = $desc;
      $processed_desc =~ s/\bGround Troop\b/Ground Troops/g;
      $processed_desc =~ s/\bMounted Troop\b/Mounted Troops/g;
      $processed_desc =~ s/\bRanged Troop\b/Ranged Troops/g;
      $processed_desc =~ s/\bSiege Machine\b/Siege Machines/g;

      $restructured_fragment = "$processed_desc by $value% $condition_to_use";
    } elsif ($desc =~ /^(.+?),\s*(.+)$/) {
      # Pattern: "condition, troop attribute" -> "troop attribute by value% condition"
      my ($condition, $troop_attr) = ($1, $2);

      # Convert singular troop types to plural in troop_attr
      $troop_attr =~ s/\bGround Troop\b/Ground Troops/g;
      $troop_attr =~ s/\bMounted Troop\b/Mounted Troops/g;
      $troop_attr =~ s/\bRanged Troop\b/Ranged Troops/g;
      $troop_attr =~ s/\bSiege Machine\b/Siege Machines/g;

      $restructured_fragment = "$troop_attr by $value% $condition";
    } elsif ($desc =~ /^(Enemy|Monsters?)\s+(.+)$/) {
      # Pattern: "Enemy/Monster troop attribute" -> special handling for debuffs
      my ($enemy_type, $troop_attr) = ($1, $2);

      # Extract conditions like "In-city" from the troop_attr
      my @conditions = ($enemy_type);
      if ($troop_attr =~ s/^(In-city|Out-city|Marching)\s+//i) {
        push @conditions, $1;
      }

      # Remove "Troop" from enemy patterns since it's not part of the attribute
      $troop_attr =~ s/\bTroops?\s+//g;

      # Convert singular troop types to plural (for any remaining troop references)
      $troop_attr =~ s/\bGround Troop\b/Ground Troops/g;
      $troop_attr =~ s/\bMounted Troop\b/Mounted Troops/g;
      $troop_attr =~ s/\bRanged Troop\b/Ranged Troops/g;
      $troop_attr =~ s/\bSiege Machine\b/Siege Machines/g;

      # For enemy patterns, put conditions first so attribute can be extracted cleanly
      my $condition_str = join(' ', @conditions);
      $restructured_fragment = "$condition_str $troop_attr by $value%";
    } elsif ($desc =~ /^(\w+)\s+(.+)$/) {
      # Pattern: "condition troop attribute" -> "troop attribute by value% condition"
      my ($condition, $troop_attr) = ($1, $2);

      # Convert singular troop types to plural to match Constants
      $troop_attr =~ s/\bGround Troop\b/Ground Troops/g;
      $troop_attr =~ s/\bMounted Troop\b/Mounted Troops/g;
      $troop_attr =~ s/\bRanged Troop\b/Ranged Troops/g;
      $troop_attr =~ s/\bSiege Machine\b/Siege Machines/g;

      $restructured_fragment = "$troop_attr by $value% when $condition";
    } else {
      # Fallback - but try to inherit context from the calling environment
      # This handles cases like "HP" where we need to get troop context from elsewhere

      # Check if this is a simple attribute that needs context inheritance
      if ($desc =~ /^(Attack|Defense|HP|March Size|Rally Capacity|Training Speed|Construction Speed)$/i) {
        # Try to find context from the buffs already processed in this fragment
        # Look for existing buffs that might have the context we need
        if (@$buffs_ref > 0) {
          my $last_buff = $buffs_ref->[-1];
          if ($last_buff->targetedType && $last_buff->conditions) {
            # Inherit context from the previous buff
            my $troop_type = $last_buff->targetedType;
            my @conditions = $last_buff->conditions;
            $restructured_fragment = "$troop_type $desc by $value% " . join(' ', @conditions);
          } else {
            $restructured_fragment = "$desc by $value%";
          }
        } else {
          $restructured_fragment = "$desc by $value%";
        }
      } else {
        $restructured_fragment = "$desc by $value%";
      }
    }

    # Now use the first if clause logic directly
    if ($restructured_fragment =~ /(?<desc>.+?) by (?<value>-?\d+)%?(?:\s+(?<cond>.+?))?\.?$/i) {
      my $new_value = abs($+{value}) + 0;
      my $new_is_debuff = $+{value} =~ /^-/ ? 1 : 0;
      my $new_desc = $+{desc};
      push @$buffs_ref, $self->process_normalized_buff($new_value, $new_is_debuff, $new_desc, $restructured_fragment);
    }
  }

  method tokenize_buffs ($text) {

    my $grammar = Path::Tiny::path('share/prolog/Game/EvonyTKR/Shared/EvonyBuffDictionary.pl');
    $grammar->spew_utf8(join("\n",
        (map {my $t = lc($_); my @l = split(/ /, $t); sprintf('attribute([%s]).', join(", ", map{ "\"$_\"" } @l));  } $self->AttributeValues->@*),
        (map {my $t = lc($_); my @l = split(/ /, $t); sprintf('troop([%s]).', join(", ", map{ "\"$_\"" } @l));  } values $self->TroopTypeValues->%*),
        (map {my $t = lc($_); my @l = split(/ /, $t); sprintf('condition([%s]).', join(", ", map{ "\"$_\"" } @l));  } $self->BuffConditionValues->@*),
        (map {my $t = lc($_); my @l = split(/ /, $t); sprintf('condition([%s]).', join(", ", map{ "\"$_\"" } @l));  } $self->DebuffConditionValues->@*),
    ));

    $text =~ s/[’‘]//g; # remove apostrophies entirely for prolog to parse.
    $text = lc($text); # prolog requires all lower case.
    my @words = split(/\s+/, $text);
    my $token_list = '[' . join(', ', map { "'$_'" } @words) . ']';

    my ($in_fh, $out_fh, $err_fh)= (undef, gensym, gensym);
    my $cmd = ['swipl',  '-s', $self->distDir->child('prolog/Game/EvonyTKR/Shared/buff_parser.pl')];
    my $pid = open3($in_fh, $out_fh, $err_fh, @$cmd);

    print $in_fh "$token_list.\n";
    close $in_fh;

    my @err = <$err_fh>;   # STDERR: compiler messages, banner
    my @out = <$out_fh>;   # STDOUT: actual result
    waitpid $pid, 0;

    my $parsed = join('', @out);
    $parsed =~ s/^\s+|\s+$//g;

    $self->logger->info(sprintf('parsed text is %s', $parsed));
    return $parsed;
    # Parse the Prolog output like: "[buff(attack,mounted,50,leading), buff(defense,mounted,30,dragon), ...]"
  }

#  method tokenize_buffs {
#    my ($text) = @_;
#    $text =~ s/[%]//g;
#    $text =~ s/[’‘]/'/g;    # Replace fancy single quotes with ASCII '
#
#    # First split full input into sentences
#    my @sentences = split /\.\s*/, $text;
#    my @result;
#
#    for my $sentence (@sentences) {
#      next unless $sentence =~ /\S/;
#
#      # Extract trailing "when ..." clause
#      my $condition_str = '';
#      my $extra_context = '';
#
#      if ($sentence =~ s/\s+when\s+(.*)$//i) {
#        $condition_str = $1;
#      }
#
#      if ($sentence =~ s/\s+(in Subordinate City)\b//i) {
#        $extra_context = $1;
#      }
#
#      # construct an attribute regex
#      my $attr_regex = join '|',
#        map { quotemeta($_) } $self->AttributeValues->@*;
#
#      # a verbs regex
#      my $verbs_regex = join '|',
#        qw(Reduces Increases Improves Decreases Boosts Lowers);
#
#      my @subs;
#
## Match any "... by <number>" pattern (covers attack and HP by 40, March Size by 15)
## Match any phrase that ends in 'by <value>', even if it has multiple troop types before it
#      my @by_clauses = $sentence =~ /
#        (
#          (?:$verbs_regex|\band\b)?       # leading keyword like "Increases" or "and"
#          [^\.]*?                          # fragment up to the value
#          \b(?:troops[’']\s+)?             # optional troops' prefix — preserve it!
#          [a-z ]*?                         # attribute area
#          \bby\s+\d+                       # match the value
#        )
#      /gix;
#      @subs = @by_clauses if @by_clauses;
#
#      # Fallback: if nothing matched, keep the full sentence
#      @subs = ($sentence) unless @subs;
#
#      foreach my $s (@subs) {
#        $s .= " $extra_context"      if $extra_context;
#        $s .= " when $condition_str" if $condition_str;
#        push @result, $s;
#      }
#    }
#    $self->logger->debug(sprintf('tokenize_buffs returning %s',
#    join(', ', grep {/\S/} @result ) ));
#    return grep {/\S/} @result;
#  }
}
1;
