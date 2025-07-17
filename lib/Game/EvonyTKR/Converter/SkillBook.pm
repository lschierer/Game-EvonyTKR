use v5.42.0;
use utf8::all;
use experimental qw(class);
use File::FindLib 'lib';
require Data::Printer;
require HTML::TreeBuilder;
require HTTP::Tiny;
require IO::Socket::IP;    # for HTTP::Tiny;
require IO::Socket::SSL;
require Path::Tiny;
require Readonly;
require YAML::PP;
require Game::EvonyTKR::Shared::Parser;
require Game::EvonyTKR::Model::Buff;
require Game::EvonyTKR::Model::Buff::Value;
require Game::EvonyTKR::Converter::Helpers;

class Game::EvonyTKR::Converter::SkillBook :
  isa(Game::EvonyTKR::Shared::Constants) {
  use List::AllUtils qw( first all any none );
  use Carp;
  use namespace::autoclean;

  field $outputDir : param;
  field $debug : param //= 0;
  field $tree : param;

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

  field $name = '';
  field $text = '';
  field $buffs;

  field $skillbookHash;
  field $helpers = Game::EvonyTKR::Converter::Helpers->new(debug => $debug);

  method getMainText {
    my $statsTable = $tree->look_down(
      '_tag'  => 'table',
      'class' => qr/stats-table/,
    );
    if ($statsTable) {
      $self->GetMainText_Template2();
    }
    else {
      $self->GetMainText_Template1();
    }
  }

  method GetMainText_Template2 {
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
    my @headers = $container->look_down('_tag' => qr/^h[234]$/);

    # Find the first h3 (start of skillbook)
    my $target   = 1;
    my $h3_count = 0;
    my $start_index;

    for my $i (0 .. $#headers) {
      if ($headers[$i]->tag eq 'h3') {
        $h3_count++;
        if ($h3_count == $target) {
          $start_index = $i;
          last;
        }
      }
    }

    unless (defined $start_index) {
      warn "Could not find required h3 tag";
      return [];
    }

    #extract 1 H4 tag after the first H3 tag
    my $h4_index = $start_index;
    while ($h4_index <= $#headers) {
      if ($headers[$h4_index]->tag eq 'h4') {
        my $skillbookH4 = $headers[$h4_index];

        my $skillBookName = $skillbookH4->as_trimmed_text;
        my $para          = $helpers->find_next_p_after_element($skillbookH4);

        if ($para) {
          $name = $skillBookName;
          $text = $para->as_trimmed_text;
        }
        else {
          $self->logger->error("Could not find the required paragraph");
        }
        last;
      }
      else {
        $h4_index++;
      }
    }
    unless (length($name) && length($text)) {
      $self->logger->error("Cannot find and parse the required H3 tag!!");
    }

  }

  method GetMainText_Template1 {
    # Find the container div
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

    #extract 1 H3 tag after the second H2 tag
    my $h3_index = $start_index + 1;
    if ($h3_index <= $#headers) {
      if ($headers[$h3_index]->tag eq 'h3') {
        my $skillbookH3 = $headers[$h3_index];

        my $skillBookName = $skillbookH3->as_trimmed_text;
        my $para          = $helpers->find_next_p_after_element($skillbookH3);

        if ($para) {
          $name = $skillBookName;
          $text = $para->as_trimmed_text;
        }
        else {
          $self->logger->error("Could not find the required paragraph");
        }
      }
    }
    else {
      $self->logger->error("Cannot find the required H3 tag!!");
    }
  }

  method parseSkillbookText {

    my $parser = Game::EvonyTKR::Shared::Parser->new();

    my @fragments = $parser->tokenize_buffs($text);
    $self->logger->debug(sprintf('thee are %s fragments', scalar(@fragments)));
    foreach my $frag (@fragments) {
      my $b = $parser->normalize_buff($frag);
      $self->logger->debug(sprintf(
        'recieved %s from normalize_buff for "%s"',
        ref($b), Data::Printer::np($frag)
      ));
      push @{$buffs}, $b;
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
    say "=== Skill Book Text to YAML Converter ===";
    $self->logger->info('=== Skill Book Text to YAML Converter ===');
    $self->getMainText();
    $self->parseSkillbookText();
    $self->printYAML();
  }

}
1;
