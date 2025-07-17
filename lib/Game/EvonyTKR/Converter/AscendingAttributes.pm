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
require Game::EvonyTKR::Converter::Helpers;

class Game::EvonyTKR::Converter::AscendingAttributes :
  isa(Game::EvonyTKR::Shared::Constants) {
  use List::AllUtils qw( first all any none );
  use Carp;
  use namespace::autoclean;

  # PODNAME: Game::EvonyTKR::Converter::AscendingAttributes

  # input fields
  field $tree : param : reader;
  field $outputDir : param;
  field $debug : param //= 0;
  field $red : param   //= 1;

  ADJUST {
    $self->logger->debug(sprintf(
'%s assumes that the class or module calling it has generated the required grammar for %s',
      __CLASS__, 'Game::EvonyTKR::Shared::Parser'
    ));
    $self->logger->debug(sprintf(
'%s assumes that the class or module calling it has also correctly set up the $tree field.',
      __CLASS__));
    # do not assume we were properly passed
    # a Path::Tiny::path
    $outputDir = Path::Tiny::path($outputDir);
  }

  # internal control fields
  field $helpers = Game::EvonyTKR::Converter::Helpers->new(debug => $debug);

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
    $self->logger->info(
      "=== Ascending AscendingAttributes Text to YAML Converter ===");
    $self->logger->debug(
      sprintf('ascending getMainText sees tree with length %s',
        length($tree->as_XML()))
    );
    $self->getMainText();
    $self->parseText();
    $self->printYAML();
  }

  method printYAML () {
    my $result = {
      id      => $name,
      general => $name,
    };
    my @keys = $self->AscendingAttributeLevelValues($red);
    foreach my $k (@keys) {
      if ($k eq 'none') {
        next;
      }
      push @{ $result->{ascending} },
        {
        level => $k,
        buff  => [map { $_->to_hash() } @{ $data->{ lc($k) }->{buffs} }],
        text  => $data->{ lc($k) }->{text},
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
      my $parser = Game::EvonyTKR::Shared::Parser->new();
      if (length($data->{$key}->{text})) {
        my @fragments = $parser->tokenize_buffs($data->{$key}->{text});
        foreach my $frag (@fragments) {
          push(@{ $data->{$key}->{buffs} }, $parser->normalize_buff($frag));
        }
      }
    }
  }

  method getMainText {
    $self->logger->trace(
      "Start of ::Converter::AscendingAttributes->getMainText");
    my $statsTable = $tree->look_down(
      '_tag'  => 'table',
      'class' => qr/stats-table/,
    );
    if ($statsTable) {
      $self->logger->debug("Calling Template2");
      $self->GetMainText_Template2();
    }
    else {
      $self->logger->trace("Calling Template1");
      $self->GetMainText_Template1();
    }

  }

  method Extract_Lines_from_UL ($ascendingUL) {
    if ($ascendingUL) {
      my $items = $helpers->extract_ul_details($ascendingUL);
      foreach my $index (0 .. (scalar(@{$items}) - 1)) {
        my $key = sprintf('%s%s', $red ? 'red' : 'purple', $index + 1,);
        $self->logger->debug("computed key '$key'");
        my $line = $items->[$index];
        $self->logger->debug("found line '$line'");
        $line =~ s/^\s+|\s+$//g;
        $line =~ s/^\d\s+Star\s*[-–—]\s*(.+)$/$1/;
        $data->{$key}->{text} = $line;
        $self->logger->debug(sprintf('added "%s" to "%s"', $line, $key));
      }
    }
    else {
      $self->logger->error("Could not find the required ul");
    }
  }

  method GetMainText_Template2 {
    my $container = $tree->look_down(
      '_tag'  => 'div',
      'class' => qr/\w+-ascension-buffs/,
    );
    unless ($container) {
      $self->logger->error("Cound not find ascension-buffs container");
      return;
    }
    $self->logger->debug("found container " . $container->starttag);
    my $h4 = $container->look_down('_tag' => qr/^h4$/);
    $self->logger->debug(sprintf('h4 is "%s"', $h4->as_trimmed_text));
    $name = $h4->as_trimmed_text =~
      s/Evony\s+(.+?)(?:[’']s)?\s+Ascension\s+Buffs/$1/r;
    $self->logger->debug("name is $name");
    my $ascendingUL = $helpers->find_next_ul_after_element($h4);

    unless (length($name)) {
      $self->logger->error("Cound not find name in document.");
    }
    unless ($ascendingUL) {
      $self->logger->error("Cound not find UL in ascension-buffs container.");
    }
    $self->Extract_Lines_from_UL($ascendingUL);
  }

  method GetMainText_Template1 {
    my $container = $tree->look_down(
      '_tag'  => 'div',
      'class' =>
qr/elementor-element-(?:\w){1,9}.elementor-widget.elementor-widget-theme-post-content/
    );

    unless ($container) {
      warn "Could not find theme-post-content div container";
      return [];
    }
    if ($debug) {
      $self->logger->debug("Found container: " . $container->starttag());
    }

    # Get all h2 and h3 elements in reading order
    my @headers = $container->look_down('_tag' => qr/^h[23]$/);

    # Find the second h2 (start of skillbook)
    my $target   = 2;
    my $h2_count = 0;
    my $start_index;

    for my $i (0 .. $#headers) {
      if ($headers[$i]->tag eq 'h2') {
        $h2_count++;
        if ($h2_count == $target) {
          $start_index = $i;
          last;
        }
      }
    }

    unless (defined $start_index) {
      warn "Could not find required h2 tag";
      return [];
    }

    my $ascendingUL;

    #extract 1 H3 tag and its UL after the second H2 tag
    my $h3_index = $start_index + 1;
    my $h3_count = 0;
    while ($h3_index <= $#headers) {
      if ($headers[$h3_index]->tag eq 'h3') {
        $h3_count++;
        unless ($h3_count == '2') {
          $h3_index++;
          next;
        }

        my $targetH3 = $headers[$h3_index];

        $name = $targetH3->as_trimmed_text =~
          s/Evony\s+(.+?)(?:[’']s)?\s+Ascension\s+Buffs/$1/r;
        $self->logger->debug(
          sprintf('targetH3 is "%s"', $targetH3->as_trimmed_text));
        $self->logger->debug("name is $name");
        $ascendingUL = $helpers->find_next_ul_after_element($targetH3);
        $self->Extract_Lines_from_UL($ascendingUL);
        last;
      }
      else {
        $h3_index++;
      }
    }
    unless (length($name)) {
      $self->logger->error("Could not find name in document.");
    }
    unless (defined $ascendingUL) {
      $self->logger->error("Cound not find ul for parsing.");
    }
  }
}
1;
__END__
