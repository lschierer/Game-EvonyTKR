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

class Game::EvonyTKR::Converter::Specialty :
  isa(Game::EvonyTKR::Shared::Constants) {
  use List::AllUtils qw( first all any none );
  use Carp;
  use namespace::autoclean;

  # PODNAME: Game::EvonyTKR::Converter::Specialty

  # input fields
  field $outputDir : param;
  field $debug : param //= 0;

  # internal control fields
  field $red = 1;

  # output fields
  field $name : reader = '';
  field $levels : reader;

  ADJUST {
    Readonly::Hash1 my %temp => (
      Green   => [],
      Blue    => [],
      Purple  => [],
      Orange  => [],
      Gold    => [],
    );
    $levels = \%temp;
  }

  method execute {
    say "=== Specialty Text to YAML Converter ===";
    $self->getName();
    $self->getMainText();
    $self->parseText();
    $self->printYAML();
  }

  method printYAML () {
  my $data = {
      name   => $name,
      levels => [
        map {
          my $level_name = $_;
          {
            level => $level_name,
            buffs => [ map { $_->to_hash } @{ $levels->{$level_name} } ],
          }
        } keys %{ $levels }
      ],
    };
    my $yc = YAML::PP->new(
      schema       => [qw/ + Perl /],
      yaml_version => ['1.2', '1.1'],
    )->dump($data);
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
    say "Please paste the specialty text below.";
    say
"Expected format: 'Increases ranged troops' attack and defense by 45% and...'",
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

    foreach my $index (0 .. scalar(@lines)) {
      my $line = $lines[$index];
      my $key  = 'Gold';
      if ($line =~ /^(\d) Star (.+)$/) {
        $key = "$key$1";
        if (exists $data->{$key}) {
          $data->{$key}->{text} = $2;
        }
        else {
          $self->logger->logcroak("Invalid Specialty Line: '$line'");
        }
      }
      else {
        $self->logger->logcroak("Invalid Specialty Line: '$line'");
      }
    }
  }

}
1;
__END__
