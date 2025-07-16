use v5.42.0;
use utf8::all;
use experimental qw(class);
use File::FindLib 'lib';
require Data::Printer;
require Path::Tiny;
require Readonly;
require JSON::PP;
require YAML::PP;
require Game::EvonyTKR::Shared::Parser;
require Game::EvonyTKR::Model::Buff;
require Game::EvonyTKR::Model::Buff::Value;

class Game::EvonyTKR::Converter::SkillBook :
  isa(Game::EvonyTKR::Shared::Constants) {
  use List::AllUtils qw( first all any none );
  use Carp;
  use namespace::autoclean;

  field $outputDir : param;
  field $debug : param //= 0;
  field $name = '';
  field $text = '';
  field $buffs;

  method getName {
    say "Please enter a name: ";
    open(my $tty, '<', '/dev/tty') or die "Cannot open /dev/tty: $!";
    my $potentialName = <$tty>;
    close $tty;
    chomp $potentialName;
    if (length($potentialName)) {
      $name = $potentialName;
    }
  }

  method getMainText {
    say "=== Skill Book Text to YAML Converter ===";
    say "Please paste the skill book text below.";
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
    $text = join(' ', @lines);
  }

  method parseSkillbookText {

    my $parser = Game::EvonyTKR::Shared::Parser->new();

    my @fragments = $parser->tokenize_buffs($text);
    foreach my $frag (@fragments) {
      push @$buffs, $parser->normalize_buff($frag);
    }

    $self->logger->debug(Data::Printer::np($buffs));

  }

  method printYAML () {
    my $data = {
      name  => $name,
      text  => $text,
      buffs => [map { $_->to_hash() } @$buffs],
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

  method execute {
    $self->getMainText();
    $self->parseSkillbookText();
    $self->getName();
    $self->printYAML();
  }

}
1;
