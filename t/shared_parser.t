use v5.40.0;
use utf8::all;
use experimental qw(class);
use Test::More;
use List::AllUtils qw( all any none );
use Devel::Local;
use File::FindLib 'lib';
require Game::EvonyTKR;
require Data::Printer;
require Game::EvonyTKR::Shared::Parser;

use Log::Log4perl;

Log::Log4perl->init(\<<'EOT');
log4perl.rootLogger              = ERROR, Screen
log4perl.logger.Game.EvonyTKR.Model.Buff = WARN
log4perl.logger.Game.EvonyTKR.Model.Buff.Summarizer = WARN
log4perl.logger.Game.EvonyTKR.Shared.Parser = DEBUG

log4perl.additivity.Game.EvonyTKR = 0
log4perl.additivity.Mojolicious = 0
log4perl.additivity.MojoX = 0
log4perl.additivity.Web = 0

log4perl.appender.Screen         = Log::Log4perl::Appender::Screen
log4perl.appender.Screen.stderr = 1
log4perl.appender.Screen.layout  = PatternLayout
log4perl.appender.Screen.layout.ConversionPattern = %d [%p] %m%n
EOT

my $parser = Game::EvonyTKR::Shared::Parser->new();
$parser->logger->level('DEBUG');

use List::MoreUtils qw(uniq);



subtest 'fake singlebuff book' => sub {
  my $text =
    'increases mounted troops attack by 45% when general is leading the army.';
  my $parsed = $parser->tokenize_buffs($text);
  diag "parsed is '$parsed'";

  ok(length($parsed) > 0,  'Parsed buffs');

  done_testing;

};

sub match_buff {
  my ($buffs, %args) = @_;

  return any {
    my $buff = $_;

    my $ok = 1;

    if ($buff->attribute ne $args{attribute}) {
      diag sprintf('attribute test failed: %s ne %s.',
        $buff->attribute, $args{attribute});
      $ok = 0;
    }

    if ($buff->value->number != $args{value}) {
      diag sprintf('value test failed: %s ne %s.',
        $buff->value->number, $args{value});
      $ok = 0;
    }

    if (defined $args{class}) {
      if (($buff->targetedType // '') ne $args{class}) {
        diag sprintf(
          'class test failed: %s ne %s.',
          $buff->targetedType // 'undef',
          $args{class}
        );
        $ok = 0;
      }
    }

    if (defined $args{conditions} && @{ $args{conditions} }) {
      if (scalar $buff->conditions() > 0) {
        my @actual   = $buff->conditions();
        my @expected = @{ $args{conditions} };
        my @union    = List::MoreUtils::uniq(@actual, @expected);
        if ( scalar(@union) != scalar(@actual)
          || scalar(@actual) != scalar(@expected)) {
          diag sprintf('conditions mismatch: actual=%s expected=%s union=%s',
            scalar(@actual), scalar(@expected), scalar(@union));
          diag('actual: ' . join(', ', @actual));
          diag('expected: ' . join(', ', @expected));
          $ok = 0;
        }
      }
      else {
        diag('expected conditions but none found in buff');
        $ok = 0;
      }
    }

    return $ok;
  } @$buffs;
}


#subtest 'Dictator Skill Book' => sub {
#  my $text =
#"Increases mounted troops’ attack by 45% when General is leading the army to attack Monsters. Increases ground troops and mounted troops’ attack by 15% when General brings any dragon.";
#
#  # Parser pipeline
#  my @fragments = $parser->tokenize_buffs($text);
#
#  my @hashedBuffs;
#  foreach my $frag (@fragments) {
#    my @nb = $parser->normalize_buff($frag);
#    push(@hashedBuffs, @nb);
#  }
#
#  is scalar(@hashedBuffs), 3, 'Parsed 3 buffs';
#
#  ok(
#    match_buff(
#      \@hashedBuffs,
#      attribute  => 'Attack',
#      value      => 15,
#      class      => 'Mounted Troops',
#      conditions => ['brings a dragon']
#    ),
#    'Mounted Troops 15% attack buff (brings a dragon)'
#  );
#
#  ok(
#    match_buff(
#      \@hashedBuffs,
#      attribute  => 'Attack',
#      value      => 15,
#      class      => 'Ground Troops',
#      conditions => ['brings a dragon']
#    ),
#    'Ground Troops 15% attack buff (brings a dragon)'
#  );
#
#  ok(
#    match_buff(
#      \@hashedBuffs,
#      attribute  => 'Attack',
#      value      => 45,
#      class      => 'Mounted Troops',
#      conditions => ['Against Monsters', 'leading the army']
#    ),
#    'Mounted Troops 45% attack buff (Against Monsters)'
#  );
#  done_testing;
#};
#
#subtest 'Augustus Skill Book' => sub {
#  my $text =
#"Increases mounted troops’ attack by 40% and ranged troops’ attack by 15% when General is leading the army to attack.";
#
#  # Parser pipeline
#  my @fragments = $parser->tokenize_buffs($text);
#  my @hashedBuffs;
#  foreach my $frag (@fragments) {
#    my @nb = $parser->normalize_buff($frag);
#    push(@hashedBuffs, @nb);
#  }
#
#  is scalar(@hashedBuffs), 2, 'Parsed 2 buffs';
#
#  ok(
#    match_buff(
#      \@hashedBuffs,
#      attribute  => 'Attack',
#      value      => 40,
#      class      => 'Mounted Troops',
#      conditions => ['Attacking', 'leading the army']
#    ),
#    'Mounted Troops 40% attack buff (Attacking)'
#  );
#
#  ok(
#    match_buff(
#      \@hashedBuffs,
#      attribute  => 'Attack',
#      value      => 15,
#      class      => 'Ranged Troops',
#      conditions => ['Attacking', 'leading the army']
#    ),
#    'Ranged Troops 15% attack buff (Attacking)'
#  );
#  done_testing;
#};
#
#subtest 'Bloody Leader Skill Book' => sub {
#  my $text =
#"Increases the March Size by 10% and mounted troops’ attack by 15% when General is leading the army to attack";
#
#  # Parser pipeline
#  my @fragments = $parser->tokenize_buffs($text);
#  my @hashedBuffs;
#  foreach my $frag (@fragments) {
#    my @nb = $parser->normalize_buff($frag);
#    push(@hashedBuffs, @nb);
#  }
#
#  is scalar(@hashedBuffs), 2, 'Parsed 2 buffs';
#
#  ok(
#    match_buff(
#      \@hashedBuffs,
#      attribute  => 'Attack',
#      value      => 15,
#      class      => 'Mounted Troops',
#      conditions => ['Attacking', 'leading the army']
#    ),
#    'Mounted Troops 15% attack buff (Attacking)'
#  );
#
#  ok(
#    match_buff(
#      \@hashedBuffs,
#      attribute  => 'March Size',
#      value      => 10,
#      conditions => ['Attacking', 'leading the army']
#    ),
#    'March Size 10% buff (Attacking)'
#  );
#
#  done_testing;
#};
#
#diag 'start of Chivalry Skill Book';
#subtest 'Chivalry Skill Book' => sub {
#  my $text =
#"Increases the Construction Speed by 50% and troops' training speed by 30% in Subordinate City when General is the Mayor.";
#
##  # Parser pipeline
#  my @fragments = $parser->tokenize_buffs($text);
#  my @hashedBuffs;
#  foreach my $frag (@fragments) {
#    diag "frag is $frag";
#    my @nb = $parser->normalize_buff($frag);
#    push(@hashedBuffs, @nb);
#  }
#
#  is scalar(@hashedBuffs), 2, 'Parsed 2 buffs';
#
#  ok(
#    match_buff(
#      \@hashedBuffs,
#      attribute  => 'SubCity Construction Speed',
#      value      => 50,
#      conditions => ['When City Mayor for this SubCity']
#    ),
#    '50% SubCity Construction Speed buff (Mayor)'
#  );
#
#  ok(
#    match_buff(
#      \@hashedBuffs,
#      attribute  => 'SubCity Training Speed',
#      value      => 30,
#      conditions => ['When City Mayor for this SubCity']
#    ),
#    '30% SubCity Training Speed buff (Mayor)'
#  );
#
#  done_testing;
#};
#
#subtest 'Alessandra Red1 - Multiple troops with and' => sub {
#  my $text = "Marching Ground Troop and Mounted Troop Defense +10% and HP +10%";
#
#  my @fragments = $parser->tokenize_buffs($text);
#  my @hashedBuffs;
#  foreach my $frag (@fragments) {
#    diag "frag is $frag";
#    my @nb = $parser->normalize_buff($frag);
#    push(@hashedBuffs, @nb);
#  }
#
#  is scalar(@hashedBuffs), 4, 'Parsed 4 buffs (2 troops × 2 attributes)';
#
#  ok(
#    match_buff(
#      \@hashedBuffs,
#      attribute  => 'Defense',
#      value      => 10,
#      class      => 'Ground Troops',
#      conditions => ['Marching']
#    ),
#    'Ground Troops 10% Defense buff (Marching)'
#  );
#
#  ok(
#    match_buff(
#      \@hashedBuffs,
#      attribute  => 'HP',
#      value      => 10,
#      class      => 'Ground Troops',
#      conditions => ['Marching']
#    ),
#    'Ground Troops 10% HP buff (Marching)'
#  );
#
#  ok(
#    match_buff(
#      \@hashedBuffs,
#      attribute  => 'Defense',
#      value      => 10,
#      class      => 'Mounted Troops',
#      conditions => ['Marching']
#    ),
#    'Mounted Troops 10% Defense buff (Marching)'
#  );
#
#  ok(
#    match_buff(
#      \@hashedBuffs,
#      attribute  => 'HP',
#      value      => 10,
#      class      => 'Mounted Troops',
#      conditions => ['Marching']
#    ),
#    'Mounted Troops 10% HP buff (Marching)'
#  );
#
#  done_testing;
#};
#
#subtest 'Alessandra Red2 - Single debuff' => sub {
#  my $text = "Enemy In-city Troop Wounded into Death rate +10%";
#
#  my @fragments = $parser->tokenize_buffs($text);
#  my @hashedBuffs;
#  foreach my $frag (@fragments) {
#    diag "frag is $frag";
#    my @nb = $parser->normalize_buff($frag);
#    push(@hashedBuffs, @nb);
#  }
#
#  is scalar(@hashedBuffs), 1, 'Parsed 1 buff';
#
#  ok(
#    match_buff(
#      \@hashedBuffs,
#      attribute  => 'Wounded to Death',
#      value      => 10,
#      conditions => ['In City', 'Enemy']
#    ),
#    'Enemy Troops 10% Wounded to Death debuff (In City)'
#  );
#
#  done_testing;
#};
#
#subtest 'Alessandra Red3 - Single buff with condition' => sub {
#  my $text = "When General is launching Alliance War, Rally Capacity +8%";
#
#  my @fragments = $parser->tokenize_buffs($text);
#  my @hashedBuffs;
#  foreach my $frag (@fragments) {
#    diag "frag is $frag";
#    my @nb = $parser->normalize_buff($frag);
#    push(@hashedBuffs, @nb);
#  }
#
#  is scalar(@hashedBuffs), 1, 'Parsed 1 buff';
#
#  ok(
#    match_buff(
#      \@hashedBuffs,
#      attribute  => 'Rally Capacity',
#      value      => 8,
#      conditions => ['When Rallying']
#    ),
#    'Rally Capacity 8% buff (launching Alliance War)'
#  );
#
#  done_testing;
#};
#
#subtest 'Alessandra Red4 - Single buff' => sub {
#  my $text = "Marching Ground Troop Defense +50%";
#
#  my @fragments = $parser->tokenize_buffs($text);
#  my @hashedBuffs;
#  foreach my $frag (@fragments) {
#    diag "frag is $frag";
#    my @nb = $parser->normalize_buff($frag);
#    push(@hashedBuffs, @nb);
#  }
#
#  is scalar(@hashedBuffs), 1, 'Parsed 1 buff';
#
#  ok(
#    match_buff(
#      \@hashedBuffs,
#      attribute  => 'Defense',
#      value      => 50,
#      class      => 'Ground Troops',
#      conditions => ['Marching']
#    ),
#    'Ground Troops 50% Defense buff (Marching)'
#  );
#
#  done_testing;
#};
#
#subtest 'Alessandra Red5 - Mixed comma and and' => sub {
#  my $text = "Marching Ground Troop Attack +20%, Defense and HP +20%";
#
#  my @fragments = $parser->tokenize_buffs($text);
#  my @hashedBuffs;
#  foreach my $frag (@fragments) {
#    diag "frag is $frag";
#    my @nb = $parser->normalize_buff($frag);
#    push(@hashedBuffs, @nb);
#  }
#
#  is scalar(@hashedBuffs), 3, 'Parsed 3 buffs';
#
#  ok(
#    match_buff(
#      \@hashedBuffs,
#      attribute  => 'Attack',
#      value      => 20,
#      class      => 'Ground Troops',
#      conditions => ['Marching']
#    ),
#    'Ground Troops 20% Attack buff (Marching)'
#  );
#
#  ok(
#    match_buff(
#      \@hashedBuffs,
#      attribute  => 'Defense',
#      value      => 20,
#      class      => 'Ground Troops',
#      conditions => ['Marching']
#    ),
#    'Ground Troops 20% Defense buff (Marching)'
#  );
#
#  ok(
#    match_buff(
#      \@hashedBuffs,
#      attribute  => 'HP',
#      value      => 20,
#      class      => 'Ground Troops',
#      conditions => ['Marching']
#    ),
#    'Ground Troops 20% HP buff (Marching)'
#  );
#
#  done_testing;
#};
#
#
#diag "start of Aethelflaed Ascending Attributes";
#
##1 Star When attacking Monsters, Mounted Troop HP +30%, Troops Defense +10%.
#subtest 'Aethelflaed’s Red1' => sub {
#  my $text = "When attacking Monsters, Mounted Troop HP +30%, Troops Defense +10%.";
#
#  my @fragments = $parser->tokenize_buffs($text);
#  my @hashedBuffs;
#  foreach my $frag (@fragments) {
#    diag "frag is $frag";
#    my @nb = $parser->normalize_buff($frag);
#    push(@hashedBuffs, @nb);
#  }
#
#  is scalar(@hashedBuffs), 2, 'Parsed 2 buffs';
#
#  ok(
#    match_buff(
#      \@hashedBuffs,
#      attribute  => 'HP',
#      value      => 30,
#      class      => 'Mounted Troops',
#      conditions => ['Against Monsters']
#    ),
#    'Mounted Troops 20% HP buff (Against Monsters)'
#  );
#
#  ok(
#    match_buff(
#      \@hashedBuffs,
#      attribute  => 'Defense',
#      value      => 10,
#      conditions => ['Against Monsters']
#    ),
#    'Mounted Troops 10% Defense buff (Against Monsters)'
#  );
#
#  done_testing;
#};
#
#
##2 Star When attacking Monsters, Monsters Defense -10%, Troops HP +15%.
#subtest 'Aethelflaed’s Red2' => sub {
#  my $text = "When attacking Monsters, Monsters Defense -10%, Troops HP +15%.";
#
#  my @fragments = $parser->tokenize_buffs($text);
#  my @hashedBuffs;
#  foreach my $frag (@fragments) {
#    diag "frag is $frag";
#    my @nb = $parser->normalize_buff($frag);
#    push(@hashedBuffs, @nb);
#  }
#
#  is scalar(@hashedBuffs), 2, 'Parsed 2 buffs';
#
#  ok(
#    match_buff(
#      \@hashedBuffs,
#      attribute  => 'Defense',
#      value      => 10,
#      conditions => ['Monsters']
#    ),
#    '10% Defense debuff (Monsters)'
#  );
#
#  ok(
#    match_buff(
#      \@hashedBuffs,
#      attribute  => 'HP',
#      value      => 15,
#      conditions => ['Against Monsters']
#    ),
#    'Generic 15% HP buff (Against Monsters)'
#  );
#
#  done_testing;
#};
#
##3 Star When attacking Monsters, Monsters Attack -10%, Troops Defense +20%.
#subtest 'Aethelflaed’s Red3' => sub {
#  my $text = "When attacking Monsters, Monsters Attack -10%, Troops Defense +20%.";
#
#  my @fragments = $parser->tokenize_buffs($text);
#  my @hashedBuffs;
#  foreach my $frag (@fragments) {
#    diag "frag is $frag";
#    my @nb = $parser->normalize_buff($frag);
#    push(@hashedBuffs, @nb);
#  }
#
#  is scalar(@hashedBuffs), 2, 'Parsed 2 buffs';
#
#  ok(
#    match_buff(
#      \@hashedBuffs,
#      attribute  => 'Attack',
#      value      => 10,
#      conditions => ['Monsters']
#    ),
#    '10% Attack debuff (Monsters)'
#  );
#
#  ok(
#    match_buff(
#      \@hashedBuffs,
#      attribute  => 'Defense',
#      value      => 20,
#      conditions => ['Against Monsters']
#    ),
#    'Generic 20% Defense buff (Against Monsters)'
#  );
#
#  done_testing;
#};
#
##4 Star When attacking Monsters, Mounted Troop Attack +15%, Troops HP +20%.
#subtest 'Aethelflaed’s Red4' => sub {
#  my $text = "When attacking Monsters, Mounted Troop Attack +15%, Troops HP +20%.";
#
#  my @fragments = $parser->tokenize_buffs($text);
#  my @hashedBuffs;
#  foreach my $frag (@fragments) {
#    diag "frag is $frag";
#    my @nb = $parser->normalize_buff($frag);
#    push(@hashedBuffs, @nb);
#  }
#
#  is scalar(@hashedBuffs), 2, 'Parsed 2 buffs';
#
#  ok(
#    match_buff(
#      \@hashedBuffs,
#      attribute  => 'Attack',
#      value      => 15,
#      class      => "Mounted Troops",
#      conditions => ['Against Monsters']
#    ),
#    '15% Mounted Troops Attack buff (Against Monsters)'
#  );
#
#  ok(
#    match_buff(
#      \@hashedBuffs,
#      attribute  => 'HP',
#      value      => 20,
#      conditions => ['Against Monsters']
#    ),
#    'Generic 20% HP buff (Against Monsters)'
#  );
#
#  done_testing;
#};
#
##5 Star When attacking Monsters, Mounted Troop Attack +20%, Troops Defense and HP +10%.
#subtest 'Aethelflaed’s Red5' => sub {
#  my $text = "When attacking Monsters, Mounted Troop Attack +20%, Troops Defense and HP +10%.";
#
#  my @fragments = $parser->tokenize_buffs($text);
#  my @hashedBuffs;
#  foreach my $frag (@fragments) {
#    diag "frag is $frag";
#    my @nb = $parser->normalize_buff($frag);
#    push(@hashedBuffs, @nb);
#  }
#
#  is scalar(@hashedBuffs), 3, 'Parsed 3 buffs';
#
#  ok(
#    match_buff(
#      \@hashedBuffs,
#      attribute  => 'Attack',
#      value      => 20,
#      class      => "Mounted Troops",
#      conditions => ['Against Monsters']
#    ),
#    '15% Mounted Troops Attack buff (Against Monsters)'
#  );
#
#  ok(
#    match_buff(
#      \@hashedBuffs,
#      attribute  => 'Defense',
#      value      => 10,
#      conditions => ['Against Monsters']
#    ),
#    'Generic 10% Defense buff (Against Monsters)'
#  );
#
#  ok(
#    match_buff(
#      \@hashedBuffs,
#      attribute  => 'HP',
#      value      => 10,
#      conditions => ['Against Monsters']
#    ),
#    'Generic 10% HP buff (Against Monsters)'
#  );
#
#
#  done_testing;
#};
#
## Monarchy Restoration
## Increases mounted troops’ attack by 50% when General is leading the army to attack. Increases ground troops and mounted troops’ defense and HP by 40% when General brings any Dragon or Spiritual Beast to attack.
#diag 'start of Monarchy Restoration Skill Book';
#subtest 'Monarchy Restoration Skill Book' => sub {
#  my $text = "Increases mounted troops’ attack by 50% when General is leading the army to attack. Increases ground troops and mounted troops’ defense and HP by 40% when General brings any Dragon or Spiritual Beast to attack.";
#
#  my @fragments = $parser->tokenize_buffs($text);
#  my @hashedBuffs;
#  foreach my $frag (@fragments) {
#    diag "frag is $frag";
#    my @nb = $parser->normalize_buff($frag);
#    push(@hashedBuffs, @nb);
#  }
#
#  is scalar(@hashedBuffs), 5, 'Parsed 5 buffs';
#
#  ok(
#    match_buff(
#      \@hashedBuffs,
#      attribute   => 'Attack',
#      value       => 50,
#      class       => 'Mounted Troops',
#      conditions  => ['leading the army', 'Attacking']
#    ),
#    '50% Mounted Troop Attack buff (Attacking)'
#  );
#
#  ok(
#    match_buff(
#      \@hashedBuffs,
#      attribute   => 'Defense',
#      value       => 40,
#      class       => 'Ground Troops',
#      conditions  => ['brings a dragon', 'brings a spiritual beast', 'Attacking']
#    ),
#    '40% Ground Troop Defense buff (conditional Attacking)'
#  );
#
#  ok(
#    match_buff(
#      \@hashedBuffs,
#      attribute   => 'HP',
#      value       => 40,
#      class       => 'Ground Troops',
#      conditions  => ['brings a dragon', 'brings a spiritual beast', 'Attacking']
#    ),
#    '40% Ground Troop HP buff (conditional Attacking)'
#  );
#
#  ok(
#    match_buff(
#      \@hashedBuffs,
#      attribute   => 'Defense',
#      value       => 40,
#      class       => 'Mounted Troops',
#      conditions  => ['brings a dragon', 'brings a spiritual beast', 'Attacking']
#    ),
#    '40% Mounted Troop Defense buff (conditional Attacking)'
#  );
#
#  ok(
#    match_buff(
#      \@hashedBuffs,
#      attribute   => 'HP',
#      value       => 40,
#      class       => 'Mounted Troops',
#      conditions  => ['brings a dragon', 'brings a spiritual beast', 'Attacking']
#    ),
#    '40% Mounted Troop HP buff (conditional Attacking)'
#  );
#
#  done_testing;
#};
#
## Napoleonic Wars
## Increases mounted troops’ attack by 50% when General is leading the army to attack. Increases mounted troops’ defense and HP by 30% when General brings any Dragon or Spiritual Beast to attack.
#diag 'start of Napoleonic Wars Skill Book';
#subtest 'Napoleonic Wars Skill Book' => sub {
#  my $text = "Increases mounted troops’ attack by 50% when General is leading the army to attack. Increases mounted troops’ defense and HP by 30% when General brings any Dragon or Spiritual Beast to attack.";
#
#  my @fragments = $parser->tokenize_buffs($text);
#  my @hashedBuffs;
#  foreach my $frag (@fragments) {
#    diag "frag is $frag";
#    my @nb = $parser->normalize_buff($frag);
#    push(@hashedBuffs, @nb);
#  }
#
#  is scalar(@hashedBuffs), 3, 'Parsed 3 buffs';
#
#  ok(
#    match_buff(
#      \@hashedBuffs,
#      attribute   => 'Attack',
#      value       => 50,
#      class       => 'Mounted Troops',
#      conditions  => ['leading the army', 'Attacking']
#    ),
#    '50% Mounted Troop Attack buff (Attacking)'
#  );
#
#  ok(
#    match_buff(
#      \@hashedBuffs,
#      attribute   => 'Defense',
#      value       => 30,
#      class       => 'Mounted Troops',
#      conditions  => ['brings a dragon', 'brings a spiritual beast', 'Attacking']
#    ),
#    '40% Mounted Troop Defense buff (conditional Attacking)'
#  );
#
#  ok(
#    match_buff(
#      \@hashedBuffs,
#      attribute   => 'HP',
#      value       => 30,
#      class       => 'Mounted Troops',
#      conditions  => ['brings a dragon', 'brings a spiritual beast', 'Attacking']
#    ),
#    '40% Mounted Troop HP buff (conditional Attacking)'
#  );
#
#  done_testing;
#};
#

done_testing;
