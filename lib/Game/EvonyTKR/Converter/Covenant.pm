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
  # This will eventually be something online that I have to fetch.
  field $generalFile : param //= '';
  field $outputDir : param;
  field $debug : param //= 0;

  # internal control fields
  field $parser  = Game::EvonyTKR::Shared::Parser->new();
  field $helpers = Game::EvonyTKR::Converter::Helpers->new(debug => $debug);

  # output fields
  field $covenantHash : reader;

  ADJUST {
    $self->logger->debug(sprintf(
'%s assumes that the class or module calling it has generated the required grammar for %s',
      __CLASS__, 'Game::EvonyTKR::Shared::Parser'
    ));
    # do not assume we were properly passed
    # a Path::Tiny::path
    $outputDir = Path::Tiny::path($outputDir);

    if (length($generalFile) == 0) {
      $self->logger->debug("setting generalFile to default test file in "
          . $self->distDir->canonpath());
      $generalFile =
        $self->distDir->child('collections/share/TestGeneralFile.html');
    }
  }

  method execute {
    say "=== Covenant Text to YAML Converter ===";
    $self->getMainText();
    $self->parseText();
  }

  method parseText {
    foreach my $covenant (@$covenantHash) {
      my $fileName   = sprintf('%s.yaml', $specialty->{name});
      my $lcFileName = lc($fileName);
      if ($outputDir->child($fileName)->is_file()) {
        $self->logger->debug(sprintf(
          'Specialty "%s" has already been converted - "%s" already exists',
          $specialty->{name}, $outputDir->child($fileName)->canonpath()
        ));
        next;
      }
      elsif ($outputDir->child($lcFileName)->is_file()) {
        $self->logger->debug(sprintf(
          'Specialty "%s" has already been converted - "%s" already exists',
          $specialty->{name}, $outputDir->child($lcFileName)->canonpath()
        ));
        next;
      }
      else {
        # this is based off what we do in the test harness
        $self->logger->debug(
          sprintf('"%s" needs to be converted.', $specialty->{name}));
        my @hashedBuffs;
        for my $detail (@{ $specialty->{details} }) {
          my @fragments = $parser->tokenize_buffs($detail);
          foreach my $frag (@fragments) {
            $self->logger->debug(sprintf(
              'frag for "%s" is %s',
              $specialty->{name}, Data::Printer::np($frag)
            ));
            my $nb;
            @{$nb} = $parser->normalize_buff($frag);
            $self->logger->debug('after normalize_buff, size of frag is %s',
              scalar(@{$nb}));
            $self->logger->debug(sprintf(
              'this fragment was normalized to -- %s -- ',
              Data::Printer::np($nb)));
            push(@hashedBuffs, @{$nb});
          }
        }
        if (scalar(@hashedBuffs)) {
          $self->printYAML($specialty->{name}, \@hashedBuffs);
        }
      }
    }
  }

  method setupLevelsHash {
    Readonly::Hash1 my %temp => (
      war           => [],
      cooperation   => [],
      peace         => [],
      faith         => [],
      honor         => [],
      civilization  => [],
    );
    my $levels = \%temp;
    return $levels;
  }



  method printYAML ($name, $buffs) {

    my $levels = $self->setupLevelsHash();



    my $result = {
      name   => $name,
      levels => [
        map {
          my $level_name = $_;
          {
            level => $level_name,
            buffs => [map { $_->to_hash } @{ $levels->{$level_name} }],
          }
        } qw/Green Blue Purple Orange Gold/ # Fixed order instead of keys %{$levels}
      ],
    };

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

  method getMainText {
    $self->logger->trace("Start of ::Converter::Covenant->getMainText");
    unless ($generalFile->is_file()) {
      $self->logger->logcroak("$generalFile must be a file.");
      exit -1;
    }
    my $html_content = $generalFile->slurp_utf8();
    my $tree = HTML::TreeBuilder->new();
    $tree->parse($html_content);
    $tree->eof();

    # Find the container div
    my $container = $tree->look_down(
      '_tag'  => 'div',
      'class' =>
        qr/elementor-element-(?:\w){1,8}.*elementor-widget-theme-post-content/
    );

    unless ($container) {
      warn "Could not find specialty container div";
      return [];
    }
    if ($debug) {
      $self->logger->debug("Found container: " . $container->starttag());
    }

  }

  method extract_specialties ($html_content) {

    my $tree = HTML::TreeBuilder->new();
    $tree->parse($html_content);
    $tree->eof();





    # Get all h2 and h3 elements in reading order
    my @headers = $container->look_down('_tag' => qr/^h[23]$/);

    # Find the third h2 (start of specialties)
    my $h2_count = 0;
    my $specialty_start_index;

    for my $i (0 .. $#headers) {
      if ($headers[$i]->tag eq 'h2') {
        $h2_count++;
        if ($h2_count == 3) {
          $specialty_start_index = $i;
          last;
        }
      }
    }

    unless (defined $specialty_start_index) {
      warn "Could not find second h2 tag";
      return [];
    }

    # Extract the 4 h3 tags after the second h2
    my @specialty_h3s;
    for my $i (($specialty_start_index + 1) .. ($specialty_start_index + 4)) {
      last if $i > $#headers;
      if ($headers[$i]->tag eq 'h3') {
        push @specialty_h3s, $headers[$i];
      }
    }

    # For each h3, find its associated ul
    my @specialties;
    for my $h3 (@specialty_h3s) {
      my $specialty_name = $h3->as_trimmed_text;

      # Find the next ul element after this h3 in document order
      my $ul = $helpers->find_next_ul_after_element($h3);

      if ($ul) {
        push @specialties,
          {
          name       => $specialty_name,
          details    => $helpers->extract_ul_details($ul),
          h3_element => $h3,
          ul_element => $ul
          };
      }
      else {
        warn "Could not find ul for specialty: $specialty_name";
      }
    }
    return \@specialties;
  }

}
1;
__END__
