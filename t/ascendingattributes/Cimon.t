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

## Cimon

diag 'start of Cimon Ascending Attributes';

subtest 'Cimon Ascending Attributes' => sub {

  subtest '1 Star' => sub {
    my $text = 'Enemy Ground Troop and Mounted Troop Attack -10%, Defense and HP -10%';
    my $hb   = testText($text);

    is scalar(@{$hb}), 6, "Final Buff Count";
    # (2 troops × 2 attributes)

    ok(
      match_buff(
        $hb,
        attribute   => 'Attack',
        value       => 10,
        class       => 'Ground Troops',
        condition   => ['Enemy']
      ),
      'Ground Troops 10% Attack debuff ()'
    );

    ok(
      match_buff(
        $hb,
        attribute   => 'Attack',
        value       => 10,
        class       => 'Mounted Troops',
        condition   => ['Enemy']
      ),
      'Mounted Troops 10% Attack debuff ()'
    );

    ok(
      match_buff(
        $hb,
        attribute   => 'Defense',
        value       => 10,
        class       => 'Ground Troops',
        condition   => ['Enemy']
      ),
      'Ground Troops 10% Defense debuff ()'
    );

    ok(
      match_buff(
        $hb,
        attribute   => 'Defense',
        value       => 10,
        class       => 'Mounted Troops',
        condition   => ['Enemy']
      ),
      'Mounted Troops 10% Defense debuff ()'
    );

    ok(
      match_buff(
        $hb,
        attribute   => 'HP',
        value       => 10,
        class       => 'Ground Troops',
        condition   => ['Enemy']
      ),
      'Ground Troops 10% HP debuff ()'
    );

    ok(
      match_buff(
        $hb,
        attribute   => 'HP',
        value       => 10,
        class       => 'Mounted Troops',
        condition   => ['Enemy']
      ),
      'Mounted Troops 10% HP debuff ()'
    );

    done_testing();
  };

  subtest '4 Star' => sub {
    my $text = 'Enemy Ranged Troop and Siege Machine Attack -15%, Defense and HP -15%';
    my $hb   = testText($text);

    is scalar(@{$hb}), 6, "Final Buff Count";

    ok(
      match_buff(
        $hb,
        attribute  => 'Attack',
        value      => 15,
        class      => 'Siege Machines',
        condition => ['Enemy']
      ),
      'Siege Machines 15% Attack debuff (Enemy)'
    );

    ok(
      match_buff(
        $hb,
        attribute  => 'Attack',
        value      => 15,
        class      => 'Ranged Troops',
        condition => ['Enemy']
      ),
      'Ranged Troops 15% Attack debuff (Enemy)'
    );

    ok(
      match_buff(
        $hb,
        attribute  => 'Defense',
        value      => 15,
        class      => 'Siege Machines',
        condition => ['Enemy']
      ),
      'Siege Machines 15% Defense debuff (Enemy)'
    );

    ok(
      match_buff(
        $hb,
        attribute  => 'Defense',
        value      => 15,
        class      => 'Ranged Troops',
        condition => ['Enemy']
      ),
      'Ranged Troops 15% Defense debuff (Enemy)'
    );

    ok(
      match_buff(
        $hb,
        attribute  => 'HP',
        value      => 15,
        class      => 'Siege Machines',
        condition => ['Enemy']
      ),
      'Siege Machines 15% HP debuff (Enemy)'
    );

    ok(
      match_buff(
        $hb,
        attribute  => 'HP',
        value      => 15,
        class      => 'Ranged Troops',
        condition => ['Enemy']
      ),
      'Ranged Troops 15% HP debuff (Enemy)'
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
