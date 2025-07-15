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
my $logConfig    = Path::Tiny->cwd()->child('share/log4perl.test.conf ');
say $logConfig->absolute();
Log::Log4perl::init($logConfig->canonpath());

my $parser = Game::EvonyTKR::Shared::Parser->new();
$parser->logger->level($DEBUG)
  ;    # <-- tried each solution with and without this line
$parser->generate_grammar();

use List::MoreUtils qw(uniq);

## Flavius Aetus

diag 'start of Flavius Aetus Ascending Attributes';

subtest 'Flavius Aetus Ascending Attributes' => sub {

  subtest '1 Star' => sub {
    my $text = 'Enemy Ranged Troop and Siege Machine Attack -5%, Enemy Ground Troop Defense -10%.';
    my $hb   = testText($text);

    is scalar(@{$hb}), 3, "Final Buff Count";
    # (2 troops × 2 attributes)

    ok(
      match_buff(
        $hb,
        attribute   => 'Attack',
        value       => 5,
        class       => 'Ranged Troops',
        condition   => ['Enemy']
      ),
      '5% Ranged Troops Attack debuff ()'
    );

    ok(
      match_buff(
        $hb,
        attribute   => 'Attack',
        value       => 5,
        class       => 'Siege Machines',
        condition   => ['Enemy']
      ),
      '5% Siege Machines Attack debuff ()'
    );

    ok(
      match_buff(
        $hb,
        attribute   => 'Defense',
        value       => 10,
        class       => 'Ground Troops',
        condition   => ['Enemy']
      ),
      '10% Ground Troops Defense debuff ()'
    );

    done_testing();
  };

  subtest '2 Star' => sub {
    my $text = 'Enemy Ground Troop HP -10%; when General is Mayor, Training Speed in this Subordinate City +15%.';
    my $hb   = testText($text);

    is scalar(@{$hb}), 2, "Final Buff Count";
    # (2 troops × 2 attributes)

    ok(
      match_buff(
        $hb,
        attribute   => 'HP',
        value       => 10,
        class       => 'Ground Troops',
        condition   => ['Enemy', 'When City Mayor for this SubCity']
      ),
      '10% Ground Troops Attack debuff (When City Mayor for this SubCity)'
    );

    ok(
      match_buff(
        $hb,
        attribute   => 'SubCity Training Speed',
        value       => 15,
        condition   => ['When City Mayor for this SubCity']
      ),
      '15% SubCity Training Speed buff (When City Mayor for this SubCity)'
    );

    done_testing();
  };

  subtest '3 Star' => sub {
    my $text = 'Enemy Ground Troop Defense -20%; when General is Mayor, Death into Survival Rate in this Subordinate City +15%.';
    my $hb   = testText($text);

    is scalar(@{$hb}), 2, "Final Buff Count";
    # (2 troops × 2 attributes)

    ok(
      match_buff(
        $hb,
        attribute   => 'Defense',
        value       => 20,
        class       => 'Ground Troops',
        condition   => ['Enemy',]
      ),
      '20% Ground Troops Defense debuff ()'
    );

    ok(
      match_buff(
        $hb,
        attribute   => 'SubCity Death to Survival',
        value       => 15,
        condition   => ['When City Mayor for this SubCity']
      ),
      '15% SubCity Death to Survival buff (When City Mayor for this SubCity)'
    );

    done_testing();
  };

  subtest '4 Star' => sub {
    my $text = 'Enemy Ranged Troop and Siege Machine Attack -10%, Enemy Ground Troop HP -10%.';
    my $hb   = testText($text);

    is scalar(@{$hb}), 3, "Final Buff Count";
    # (2 troops × 2 attributes)

    ok(
      match_buff(
        $hb,
        attribute   => 'Attack',
        value       => 10,
        class       => 'Ranged Troops',
        condition   => ['Enemy',]
      ),
      '10% Ranged Troops Defense debuff ()'
    );

    ok(
      match_buff(
        $hb,
        attribute   => 'Attack',
        value       => 10,
        class       => 'Siege Machines',
        condition   => ['Enemy',]
      ),
      '10% Siege Machines Attack debuff ()'
    );

    ok(
      match_buff(
        $hb,
        attribute   => 'HP',
        value       => 10,
        class       => 'Ground Troops',
        condition   => ['Enemy',]
      ),
      '10% Ground Troops HP debuff ()'
    );

    done_testing();
  };

  subtest '5 Star' => sub {
    my $text = 'Enemy Ranged Troop and Siege Machine Attack -15%, Enemy Ground Troop Defense and HP -15%.';
    my $hb   = testText($text);

    is scalar(@{$hb}), 4, "Final Buff Count";
    # (2 troops × 2 attributes)

    ok(
      match_buff(
        $hb,
        attribute   => 'Attack',
        value       => 15,
        class       => 'Ranged Troops',
        condition   => ['Enemy',]
      ),
      '15% Ranged Troops Defense debuff ()'
    );

    ok(
      match_buff(
        $hb,
        attribute   => 'Attack',
        value       => 15,
        class       => 'Siege Machines',
        condition   => ['Enemy',]
      ),
      '15% Siege Machines Attack debuff ()'
    );

    ok(
      match_buff(
        $hb,
        attribute   => 'Defense',
        value       => 15,
        class       => 'Ground Troops',
        condition   => ['Enemy',]
      ),
      '15% Ground Troops Defense debuff ()'
    );

    ok(
      match_buff(
        $hb,
        attribute   => 'HP',
        value       => 15,
        class       => 'Ground Troops',
        condition   => ['Enemy',]
      ),
      '15% Ground Troops HP debuff ()'
    );

    done_testing();
  };

  done_testing();
};
done_testing();

sub testText ($tt) {
  my @fragments = $parser->tokenize_buffs($tt);
  my @hashedBuffs;
  foreach my $frag (@fragments) {
    diag "frag is " . Data::Printer::np($frag);
    my @nb = $parser->normalize_buff($frag);
    diag "size: " . scalar(@nb);
    diag "nb is " . Data::Printer::np(\@nb);
    push(@hashedBuffs, @nb);
  }
  diag "final results are: " . Data::Printer::np(\@hashedBuffs);
  diag "size: " . scalar(@hashedBuffs);

  return \@hashedBuffs;
}

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
1;
__END__
