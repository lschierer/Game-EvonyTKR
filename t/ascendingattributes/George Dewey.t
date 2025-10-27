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

## George Dewey

diag 'start of George Dewey Ascending Attributes';

subtest 'George Dewey Ascending Attributes' => sub {

  subtest '1 Star' => sub {
    my $text = 'When General is the Main City Defense General, In-city Mounted Troop Defense +30% and HP +20%.';
    my $hb   = testText($text);

    is scalar(@{$hb}), 2, "Final Buff Count";
    # (2 troops × 2 attributes)

    ok(
      match_buff(
        $hb,
        attribute   => 'Defense',
        value       => 30,
        class       => 'Mounted Troops',
        condition   => ['When The Main Defense General', 'In City']
      ),
      '30% Mounted Troops Defense buff (When The Main Defense General In City)'
    );

    ok(
      match_buff(
        $hb,
        attribute   => 'HP',
        value       => 20,
        class       => 'Mounted Troops',
        condition   => ['When The Main Defense General', 'In City']
      ),
      '20% Mounted Troops HP buff (When The Main Defense General In City)'
    );

    done_testing();
  };

  subtest '2 Star' => sub {
    my $text = 'When General is the Main City Defense General, Hospital Capacity +20%.';
    my $hb   = testText($text);

    is scalar(@{$hb}), 1, "Final Buff Count";
    # (2 troops × 2 attributes)

    ok(
      match_buff(
        $hb,
        attribute   => 'Hospital Capacity',
        value       => 20,
        condition   => ['When The Main Defense General']
      ),
      '20% Hospital Capacity buff (When The Main Defense General)'
    );

    done_testing();
  };

  subtest '3 Star' => sub {
    my $text = 'When General is the Main City Defense General, In-city Siege Machine and Ranged Troop Attack +10% and HP +20%.';
    my $hb   = testText($text);

    is scalar(@{$hb}), 4, "Final Buff Count";
    # (2 troops × 2 attributes)

    ok(
      match_buff(
        $hb,
        attribute   => 'Attack',
        value       => 10,
        class       => 'Siege Machines',
        condition   => ['When The Main Defense General', 'In City']
      ),
      '10% Siege Machines Attack buff (When The Main Defense General In City)'
    );

    ok(
      match_buff(
        $hb,
        attribute   => 'HP',
        value       => 20,
        class       => 'Siege Machines',
        condition   => ['When The Main Defense General', 'In City']
      ),
      '20% Siege Machines HP buff (When The Main Defense General In City)'
    );

    ok(
      match_buff(
        $hb,
        attribute   => 'Attack',
        value       => 10,
        class       => 'Ranged Troops',
        condition   => ['When The Main Defense General', 'In City']
      ),
      '10% Ranged Troops Attack buff (When The Main Defense General In City)'
    );

    ok(
      match_buff(
        $hb,
        attribute   => 'HP',
        value       => 20,
        class       => 'Ranged Troops',
        condition   => ['When The Main Defense General', 'In City']
      ),
      '20% Ranged Troops HP buff (When The Main Defense General In City)'
    );

    done_testing();
  };

  subtest '5 Star' => sub {
    my $text = 'Increases In-city Mounted Troop HP by 15%, Defense by 25% and In-city Ranged Troop and Siege Machine Attack by 20% when General is the Main City Defense General.';
    my $hb   = testText($text);

    is scalar(@{$hb}), 4, "Final Buff Count";
    # (2 troops × 2 attributes)

    ok(
      match_buff(
        $hb,
        attribute   => 'HP',
        value       => 15,
        class       => 'Mounted Troops',
        condition   => ['When The Main Defense General', 'In City']
      ),
      '15% Mounted Troops HP buff (When The Main Defense General In City)'
    );

    ok(
      match_buff(
        $hb,
        attribute   => 'Defense',
        value       => 25,
        class       => 'Mounted Troops',
        condition   => ['When The Main Defense General', 'In City']
      ),
      '25% Mounted Troops Defense buff (When The Main Defense General In City)'
    );

    ok(
      match_buff(
        $hb,
        attribute   => 'Attack',
        value       => 20,
        class       => 'Ranged Troops',
        condition   => ['When The Main Defense General', 'In City']
      ),
      '20% Ranged Troops Attack buff (When The Main Defense General In City)'
    );

    ok(
      match_buff(
        $hb,
        attribute   => 'Attack',
        value       => 20,
        class       => 'Siege Machines',
        condition   => ['When The Main Defense General', 'In City']
      ),
      '20% Siege Machines Attack buff (When The Main Defense General In City)'
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
