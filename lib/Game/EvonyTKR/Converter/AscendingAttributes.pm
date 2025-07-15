use v5.42.0;
use utf8::all;
use experimental qw(class);
use File::FindLib 'lib';
require Data::Printer;
require Path::Tiny;
require YAML::PP;
require Readonly;

require Game::EvonyTKR::Shared::Parser;
require Game::EvonyTKR::Model::Buff;
require Game::EvonyTKR::Model::Buff::Value;

class Game::EvonyTKR::Converter::AscendingAttributes :
  isa(Game::EvonyTKR::Shared::Constants) {
  use List::AllUtils qw( first all any none );
  use Carp;
  use namespace::autoclean;

  # PODNAME: Game::EvonyTKR::Converter::AscendingAttributes

  # input fields
  field $outputDir : param;
  field $debug : param //= 0;

  # internal control fields
  field $red = 1;

  # output fields
  field $name : reader = '';
  field $data : reader;

  ADJUST {
    Readonly::Hash1 my %temp => (
      purple1 => {
        label => '1st Purple Star',
        text  => '',
        buffs => [],
      },
      purple2 => {
        label => '2nd Purple Star',
        text  => '',
        buffs => [],
      },
      purple3 => {
        label => '3rd Purple Star',
        text  => '',
        buffs => [],
      },
      purple4 => {
        label => '4th Purple Star',
        text  => '',
        buffs => [],
      },
      purple5 => {
        label => '5th Purple Star',
        text  => '',
        buffs => [],
      },
      red1 => {
        label => '1st Red Star',
        text  => '',
        buffs => [],
      },
      red2 => {
        label => '2nd Red Star',
        text  => '',
        buffs => [],
      },
      red3 => {
        label => '3rd Red Star',
        text  => '',
        buffs => [],
      },
      red4 => {
        label => '4th Red Star',
        text  => '',
        buffs => [],
      },
      red5 => {
        label => '5th Red Star',
        text  => '',
        buffs => [],
      },
    );
    $data = \%temp;
  }

  method execute {
    say "=== Ascending AscendingAttributes Text to YAML Converter ===";
    $self->getName();
    $self->purpleOrRed();
    $self->getMainText();
    $self->parseText();
    $self->printYAML();
  }

  method printYAML () {
    my $result = {
      id        => $name,
      general   => $name,
    };
    my @keys = $self->AscendingAttributeLevelValues($red);
    foreach my $k (@keys){
      if($k eq 'none') {
        next;
      }
      push @{$result->{ascending} }, {
        level => $k,
        buff  => [map { $_->to_hash() } @{ $data->{lc($k)}->{buffs} }]
      };
    }
    my $yc = YAML::PP->new(
      schema       => [qw/ + Perl /],
      yaml_version => ['1.2', '1.1'],
    )->dump($result);
    my $filename = lc($name);
    $filename = "${filename}.yaml";
    if (!$outputDir->is_dir()) {
      $self->logger->error(
        "$outputDir is not a directory!!!" . $outputDir->stat());
    }
    $outputDir->child($filename)->touch();
    if ($debug) {
      say $yc;
      $outputDir->child($filename)->spew_utf8($yc);
    }
    else {
      $outputDir->child($filename)->spew_utf8($yc);
    }

  }

  method parseText {
    foreach my $index (1 .. 5) {
      my $key = $red ? 'red' : 'purple';
      $key = "$key$index";
      my $parser    = Game::EvonyTKR::Shared::Parser->new();
      my @fragments = $parser->tokenize_buffs($data->{$key}->{text});
      foreach my $frag (@fragments) {
        push(@{ $data->{$key}->{buffs} }, $parser->normalize_buff($frag));
      }
    }
  }

  method getMainText {
    say "Please paste the ascending attribute text below.";
    say
"Expected format: '1 Star Increases ranged troops' attack and defense by 45% and...'",
      say "Press Ctrl+D when finished, or type 'END' on a new line:",
      my @lines;
    while (my $line = <STDIN>) {
      chomp $line;    # Remove the newline character
      if ($line ne 'END') {
        push @lines, $line;
      }
      else {
        last;
      }
    }

    foreach my $index (0 .. 4) {
      unless ($index < scalar(@lines)) {
        $self->logger->logcroak(
          "Invalid Ascending Attribute Text: insufficient lines entered.");
      }
      my $line = $lines[$index];
      my $key  = $red ? 'red' : 'purple';
      if ($line =~ /^(\d) Star(?:\s*[-–—]\s*)? (.+)$/) {
        $key = "$key$1";
        if (exists $data->{$key}) {
          $data->{$key}->{text} = $2;
        }
        else {
          $self->logger->logcroak("Invalid Ascending Attribute Line: '$line'");
        }
      }
      else {
        $self->logger->logcroak("Invalid Ascending Attribute Line: '$line'");
      }
    }
  }

  method getName {
    say "Please enter the name of the general to whom this applies: ";
    open(my $tty, '<:encoding(UTF-8)', '/dev/tty')
      or $self->logger->logcroak("Cannot open /dev/tty: $!");
    my $potentialName = <$tty>;
    close $tty;
    chomp $potentialName;
    if (length($potentialName)) {
      $name = $potentialName;
    }
  }

  method purpleOrRed {
    my $answer = '';
    while (lc($answer) ne 'red' and lc($answer) ne 'purple') {
      if ($answer) {
        say "You must type 'red' or 'purple', not $answer";
      }
      else {
        say "Does this General ascend to Purple or Red Stars? (purple|red)";
      }

      open(my $tty, '<:encoding(UTF-8)', '/dev/tty')
        or $self->logger->logcroak("Cannot open /dev/tty: $!");
      $answer = <$tty>;
      close $tty;
      chomp $answer;
      $answer = lc($answer);
    }
    if ($answer eq 'red') {
      $red = 1;
    }
    else {
      $red = 0;
    }
  }

}
1;
__END__
