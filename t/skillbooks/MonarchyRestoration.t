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
require Game::EvonyTKR::Logger::Config;
require Path::Tiny;
require Scalar::Util;

use Log::Log4perl qw(:levels);
#use Log::Log4perl qw(:easy);       #  <<- Tried these two lines first.
#Log::Log4perl->easy_init($DEBUG);  #

my $loggerConfig = Game::EvonyTKR::Logger::Config->new('test');
my $logConfig = Path::Tiny->cwd()->child('share/log4perl.test.conf ');
say $logConfig->absolute();
Log::Log4perl::init($logConfig->canonpath());

my $parser = Game::EvonyTKR::Shared::Parser->new();
$parser->logger->level($DEBUG);   # <-- tried each solution with and without this line
$parser->generate_grammar();

use List::MoreUtils qw(uniq);

## Monarchy Restoration
## Increases mounted troops’ attack by 50% when General is leading the army to attack. Increases ground troops and mounted troops’ defense and HP by 40% when General brings any Dragon or Spiritual Beast to attack.
diag 'start of Monarchy Restoration Skill Book';
subtest 'Monarchy Restoration Skill Book' => sub {
  my $text = "Increases mounted troops’ attack by 50% when General is leading the army to attack. Increases ground troops and mounted troops’ defense and HP by 40% when General brings any Dragon or Spiritual Beast to attack.";

  my @fragments = $parser->tokenize_buffs($text);
  my @hashedBuffs;
  foreach my $frag (@fragments) {
    diag "frag is " . Data::Printer::np($frag);
    my @nb = $parser->normalize_buff($frag);
    diag "size: " . scalar(@nb);
    diag "nb is " . Data::Printer::np(\@nb);
    push(@hashedBuffs, @nb);
  }
  if(scalar @hashedBuffs > 0 ) {
    diag "hashedBuffs size " . scalar(@hashedBuffs);
    for my $i (0 .. $#hashedBuffs) {
        diag "hashedBuffs[$i] = " . Data::Printer::np($hashedBuffs[$i]);
    }
  } else {
    diag "No buffs returned."
  }
  is scalar(@hashedBuffs), 5, 'Parsed 1 Monarchy Restoration buffs';

  ok(
    match_buff(
      \@hashedBuffs,
      attribute   => 'Attack',
      value       => 50,
      class       => 'Mounted Troops',
      conditions  => ['leading the army', 'Attacking']
    ),
    '50% Mounted Troop Attack buff (Attacking)'
  );

  ok(
    match_buff(
      \@hashedBuffs,
      attribute   => 'Defense',
      value       => 40,
      class       => 'Ground Troops',
      conditions  => ['brings a dragon', 'brings a spiritual beast', 'Attacking']
    ),
    '40% Ground Troop Defense buff (conditional Attacking)'
  );

  ok(
    match_buff(
      \@hashedBuffs,
      attribute   => 'HP',
      value       => 40,
      class       => 'Ground Troops',
      conditions  => ['brings a dragon', 'brings a spiritual beast', 'Attacking']
    ),
    '40% Ground Troop HP buff (conditional Attacking)'
  );

  ok(
    match_buff(
      \@hashedBuffs,
      attribute   => 'Defense',
      value       => 40,
      class       => 'Mounted Troops',
      conditions  => ['brings a dragon', 'brings a spiritual beast', 'Attacking']
    ),
    '40% Mounted Troop Defense buff (conditional Attacking)'
  );

  ok(
    match_buff(
      \@hashedBuffs,
      attribute   => 'HP',
      value       => 40,
      class       => 'Mounted Troops',
      conditions  => ['brings a dragon', 'brings a spiritual beast', 'Attacking']
    ),
    '40% Mounted Troop HP buff (conditional Attacking)'
  );

  done_testing();
};

done_testing();

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
