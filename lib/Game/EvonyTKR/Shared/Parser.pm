use v5.40.0;
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

  method normalize_buff ($fragment) {
    my @buffs;
    $self->logger->debug("fragment is $fragment");

    if ($fragment =~ /(?<desc>.+?) by (?<value>-?\d+)%?(?:\s+(?<cond>.+?))?\.?$/i) {
      my $value     = abs($+{value}) + 0;
      my $is_debuff = $+{value} =~ /^-/ ? 1 : 0;
      my $desc      = $+{desc};
      my $cond      = $+{cond} // '';

      # Start with a working copy of the fragment
      my $working_fragment = $fragment;

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
    }

    return @buffs;
  }

  method tokenize_buffs {
    my ($text) = @_;
    $text =~ s/[%]//g;
    $text =~ s/[’‘]/'/g;    # Replace fancy single quotes with ASCII '

    # First split full input into sentences
    my @sentences = split /\.\s*/, $text;
    my @result;

    for my $sentence (@sentences) {
      next unless $sentence =~ /\S/;

      # Extract trailing "when ..." clause
      my $condition_str = '';
      my $extra_context = '';

      if ($sentence =~ s/\s+when\s+(.*)$//i) {
        $condition_str = $1;
      }

      if ($sentence =~ s/\s+(in Subordinate City)\b//i) {
        $extra_context = $1;
      }

      # construct an attribute regex
      my $attr_regex = join '|',
        map { quotemeta($_) } $self->AttributeValues->@*;

      # a verbs regex
      my $verbs_regex = join '|',
        qw(Reduces Increases Improves Decreases Boosts Lowers);

      my @subs;

# Match any "... by <number>" pattern (covers attack and HP by 40, March Size by 15)
# Match any phrase that ends in 'by <value>', even if it has multiple troop types before it
      my @by_clauses = $sentence =~ /
        (
          (?:$verbs_regex|\band\b)?       # leading keyword like "Increases" or "and"
          [^\.]*?                          # fragment up to the value
          \b(?:troops[’']\s+)?             # optional troops' prefix — preserve it!
          [a-z ]*?                         # attribute area
          \bby\s+\d+                       # match the value
        )
      /gix;
      @subs = @by_clauses if @by_clauses;

      # Fallback: if nothing matched, keep the full sentence
      @subs = ($sentence) unless @subs;

      foreach my $s (@subs) {
        $s .= " $extra_context"      if $extra_context;
        $s .= " when $condition_str" if $condition_str;
        push @result, $s;
      }
    }

    return grep {/\S/} @result;
  }
}
1;
