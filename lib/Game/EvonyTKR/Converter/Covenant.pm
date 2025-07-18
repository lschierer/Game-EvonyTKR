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

#require Game::EvonyTKR;    # for dist_dir to work
require Game::EvonyTKR::Shared::Parser;
require Game::EvonyTKR::Model::Buff;
require Game::EvonyTKR::Model::Buff::Value;
require Game::EvonyTKR::Converter::Helpers;

class Game::EvonyTKR::Converter::Covenant :
  isa(Game::EvonyTKR::Shared::Constants) {
  use List::AllUtils qw( first all any none );
  use Carp;
  use namespace::autoclean;

  # PODNAME: Game::EvonyTKR::Converter::Covenant

  # input fields

  field $tree      : param;
  field $outputDir : param;
  field $debug     : param //= 0;

  # internal control fields
  field $parser  = Game::EvonyTKR::Shared::Parser->new();
  field $helpers = Game::EvonyTKR::Converter::Helpers->new(debug => $debug);

  # output fields
  field $primary      : reader;
  field $supporting   : reader = [];
  field $covenantHash : reader;

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

  method execute {
    say "=== Covenant Text to YAML Converter ===";
    $self->logger->info("=== Covenant Text to YAML Converter ===");
    $self->getMainText();
    if (scalar(keys %$covenantHash)) {
      $self->logger->debug(sprintf('there are %s keys in covenantHash',
        scalar(keys %$covenantHash),));
      $self->parseText();
      $self->printYAML();
    }
  }

  method parseText {
    $self->logger->debug("at start of parseText, covenantHash currently has "
        . Data::Printer::np($covenantHash));
    foreach my $key (keys %$covenantHash) {
      $self->logger->debug("parseText for key $key");
      my $covenant  = $covenantHash->{$key};
      my @sentences = split(/;;/, $covenant->{text});
      foreach my $sentence (@sentences) {
        if ($sentence =~ /(.+)(\((?:Local|Global)\))/) {
          my $text    = $1;
          my $passive = $2 eq '(Global)' ? 1 : 0;
          $self->logger->debug(sprintf(
            'using text "%s" as a %s buff',
            $text, $passive ? 'passive' : 'personal'
          ));

          my @fragments = $parser->tokenize_buffs($text);
          $self->logger->debug(
            sprintf('thee are %s fragments', scalar(@fragments)));

          foreach my $frag (@fragments) {
            my $b = $parser->normalize_buff($frag);
            $self->logger->debug(sprintf(
              'recieved %s from normalize_buff for "%s"',
              ref($b), Data::Printer::np($frag)
            ));
            if ($passive) {
              $b->set_passive(1);
            }
            push @{ $covenantHash->{$key}->{buffs} }, $b;
          }
        }
      }

      $self->logger->debug(sprintf(
        'found %s buffs for key %s: %s',
        scalar(@{ $covenantHash->{$key}->{buffs} }), $key,
        Data::Printer::np($covenantHash->{$key}->{buffs})
      ));

    }
  }

  method setupLevelsHash {
    Readonly::Hash1 my %temp => (
      war => {
        text  => '',
        buffs => [],
      },
      cooperation => {
        text  => '',
        buffs => [],
      },
      peace => {
        text  => '',
        buffs => [],
      },
      faith => {
        text  => '',
        buffs => [],
      },
      honor => {
        text  => '',
        buffs => [],
      },
      civilization => {
        text  => '',
        buffs => [],
      },
    );
    my $levels = \%temp;
    return $levels;
  }

  method printYAML () {

    my $result = {
      name     => $primary,
      generals => $supporting,
      levels   => [],
    };
    foreach my $key (keys %$covenantHash) {
      push @{ $result->{levels} },
        {
        category => $key,
        buffs    => [map { $_->to_hash } @{ $covenantHash->{$key}->{buffs} }],
        text     => $covenantHash->{$key}->{text},
        };
    }

    my $yc = YAML::PP->new(
      schema       => [qw/ + Perl /],
      yaml_version => ['1.2', '1.1'],
    )->dump($result);
    my $filename = lc($primary);
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

  method getMainText {
    $self->logger->trace("Start of ::Converter::Covenant->getMainText");

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

  method GetMainText_Template2 () {
    my $container = $tree->look_down(
      '_tag'  => 'div',
      'class' => qr/\w+-covenant-container/
    );

    unless ($container) {
      warn "Could not find covenant-container div";
      return [];
    }
    if ($debug) {
      $self->logger->debug("Found container: " . $container->starttag());
    }

    # Get all h2 and h3 elements in reading order
    my @headers = $container->look_down('_tag' => qr/^h[23]$/);
    unless (scalar(@headers) > 0) {
      $self->logger->error("No headers found");
    }

    # Find the first h3 (start of the pertinent info)
    my $target   = 1;
    my $h3_count = 0;
    my $start_index;

    my $targetH3;
    for my $i (0 .. $#headers) {
      if ($headers[$i]->tag eq 'h3') {
        $h3_count++;
        if ($h3_count == $target) {
          $targetH3    = $headers[$i];
          $start_index = $i;
          last;
        }
      }
    }

    unless (defined $start_index) {
      warn "Could not find required h2 tag";
      return [];
    }
    $self->logger->debug("found start_index $start_index");

    $primary =
      $targetH3->as_trimmed_text =~ s/Evony\s+(.+?)\s+Covenant:?/$1/r;
      $primary =~ s/[\x{0022}\x{0027}\x{2018}\x{2019}\x{201C}\x{201D}\x{0060}\x{00B4}]s//g;
      $primary =~ s/Evony\s+(.+?)\s+Covenant:/$1/;
    $self->logger->debug("found primary '$primary'");
    my $supporting_para = $helpers->find_next_p_after_element($targetH3);
    if ($supporting_para) {
      my @links = $supporting_para->look_down('_tag' => 'a');
      if (scalar @links) {
        foreach my $link (@links) {
          $self->logger->debug(
            sprintf('supporting general "%s"', $link->as_trimmed_text));
          push @{$supporting}, $link->as_trimmed_text;
        }
      }
      else {
        $self->logger->error("Could not find links in supporting_para");
      }
    }
    else {
      $self->logger->error("Cound not find supporting_para");
    }

    $covenantHash = $self->setupLevelsHash();
    my $detailsTable = $helpers->find_next_table_after_element($targetH3);
    unless ($detailsTable) {
      $self->logger->error("cound not find detailsTable");
    }

    my @rows = $detailsTable->look_down('_tag' => 'tr');
    unless (scalar(@rows)) {
      $self->logger->error("no rows in table");
    }

    foreach my $ri (0 .. $#rows) {
      next unless $ri > 0;    # skip the header row.
      my $row   = $rows[$ri];
      my @cells = $row->look_down('_tag' => 'td');
      my $key   = lc($cells[0]->as_trimmed_text);
      # the source site occasionally accidentically has extra text
      # in the cell being used as the key.
      $key = first { $key =~ /$_/i } @{ $self->CovenantLevelValues };
      $self->logger->debug("getting text for key $key");

      my $fragment = $cells[1]->as_HTML;
      $fragment =~ s/<td>(.+)<\/td>/$1/;
      # some fragments have divs inside the td tag
      # those divs may have one or more attributes.
      $fragment =~ s/<div.*?>(.+)<\/div>/$1/;
      $fragment =~ s/<br\s*\/?>/;;/g;
      $self->logger->debug("fragment for $key is $fragment");
      if (exists $covenantHash->{$key}) {
        $covenantHash->{$key}->{text} = $fragment;
      }
      else {
        $self->logger->error("key $key is invalid, valid keys are "
            . join(', ', keys %$covenantHash));
      }
    }
  }

  method GetMainText_Template1 () {
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
    unless (scalar(@headers) > 0) {
      $self->logger->error("No headers found");
    }

    # Find the Third h2 (start of the pertinent info)
    my $target   = 4;
    my $h2_count = 0;
    my $start_index;

    my $targetH2;
    for my $i (0 .. $#headers) {
      if ($headers[$i]->tag eq 'h2') {
        $h2_count++;
        if ($h2_count == $target) {
          $targetH2    = $headers[$i];
          $start_index = $i;
          last;
        }
      }
    }

    unless (defined $start_index) {
      warn "Could not find required h2 tag";
      return;
    }
    $self->logger->debug("found start_index $start_index");

    $primary =
      $targetH2->as_trimmed_text =~ s/Evony\s+(.+?)\s+Covenant:?/$1/r;
      $primary =~ s/[\x{0022}\x{0027}\x{2018}\x{2019}\x{201C}\x{201D}\x{0060}\x{00B4}]s//g;
      $primary =~ s/Evony\s+(.+?)\s+Covenant:/$1/;
    my $supporting_para = $helpers->find_next_p_after_element($targetH2);
    if ($supporting_para) {
      my @links = $supporting_para->look_down('_tag' => 'a');
      if (scalar @links) {
        foreach my $link (@links) {
          $self->logger->debug(
            sprintf('supporting general "%s"', $link->as_trimmed_text));
          push @{$supporting}, $link->as_trimmed_text;
        }
      }
      else {
        $self->logger->error("Could not find links in supporting_para");
        return;
      }
    }
    else {
      $self->logger->error("Cound not find supporting_para");
      return;
    }

    $covenantHash = $self->setupLevelsHash();
    my $detailsTable = $helpers->find_next_table_after_element($targetH2);

    unless (defined $detailsTable) {
      $self->logger->error("cound not find detailsTable");
      return;
    }
    $self->logger->debug("$detailsTable is " . builtin::blessed $detailsTable);

    my @rows = $detailsTable->look_down('_tag' => 'tr');
    unless (scalar(@rows)) {
      $self->logger->error("no rows in table");
      return;
    }

    foreach my $ri (0 .. $#rows) {
      next unless $ri > 0;    # skip the header row.
      my $row   = $rows[$ri];
      my @cells = $row->look_down('_tag' => 'td');
      my $key   = lc($cells[0]->as_trimmed_text);
      # the source site occasionally accidentically has extra text
      # in the cell being used as the key.
      $key = first { $key =~ /$_/i } @{ $self->CovenantLevelValues };
      $self->logger->debug("getting text for key $key");

      my $fragment = $cells[1]->as_HTML;
      $fragment =~ s/<td>(.+)<\/td>/$1/;
      $fragment =~ s/<br\s*\/?>/;;/g;
      $self->logger->debug("fragment for $key is $fragment");
      if (exists $covenantHash->{$key}) {
        $covenantHash->{$key}->{text} = $fragment;
      }
      else {
        $self->logger->error("key $key is invalid, valid keys are "
            . join(', ', keys %$covenantHash));
      }
    }
  }

}
1;
__END__
