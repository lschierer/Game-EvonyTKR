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
  field $tree      : param :reader;
  field $outputDir : param;
  field $debug     : param //= 0;
  field $red       : param //= 1;

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
    $self->logger->debug(sprintf('ascending getMainText sees tree -- %s --', $tree->as_XML()));
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
        buff  => [map { $_->to_hash() } @{ $data->{ lc($k) }->{buffs} }]
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

  method getName () {

  }

  method getMainText {
    $self->logger->trace(
      "Start of ::Converter::AscendingAttributes->getMainText");


    my $header = $self->tree->look_down(sub {
      my $el = shift;
      return 0 unless $el->tag eq 'header';

      my $class = $el->attr('class') // '';
      my %classes = map { $_ => 1 } split /\s+/, $class;

      return $classes{'entry-header'} && $classes{'hentry-wrapper'};
    });

    unless ($header) {
      $self->logger->logcroak("header cannot be found in the provided file.");
    }

    my $nh = $header->look_down('_tag' => 'h1');
    unless ($nh) {
      $self->logger->logcroak("name H1 cannot be found in the provided file.");
    }

    $name = $nh->as_trimmed_text;

    # Find the container div
    my $container = $tree->look_down(
      '_tag'  => 'div',
      'class' => qr/entry-content.*th-content/
    );

    unless ($container) {
      warn "Could not find entry-content div container";
      return [];
    }
    if ($debug) {
      $self->logger->debug("Found container: " . $container->starttag());
    }

    # Get all h2 and h3 elements in reading order
    my @headers = $container->look_down('_tag' => qr/^h[23]$/);

    # Find the third h2 (start of skillbook)
    my $target   = 3;
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

    my $ascendingUL =
      $helpers->find_next_ul_after_element($headers[$start_index]);
    unless ($ascendingUL) {
      $self->logger->error("Cound not find required UL for ascending");
      return [];
    }
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

}
1;
__END__
