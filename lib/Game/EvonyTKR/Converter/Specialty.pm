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

class Game::EvonyTKR::Converter::Specialty :
  isa(Game::EvonyTKR::Shared::Constants) {
  use List::AllUtils qw( first all any none );
  use Carp;
  use namespace::autoclean;

  # PODNAME: Game::EvonyTKR::Converter::Specialty

  # input fields
  # This will eventually be something online that I have to fetch.
  field $tree :param;
  field $outputDir : param;
  field $debug : param //= 0;

  # internal control fields
  field $parser  = Game::EvonyTKR::Shared::Parser->new();
  field $helpers = Game::EvonyTKR::Converter::Helpers->new(debug => $debug);

  # output fields
  field $specialties : reader;

  ADJUST {
    $self->logger->debug(sprintf(
'%s assumes that the class or module calling it has generated the required grammar for %s',
      __CLASS__, 'Game::EvonyTKR::Shared::Parser'
    ));
    $self->logger->debug(sprintf(
      '%s assumes that the class or module calling it has also correctly set up the $tree field.',
      __CLASS__
    ));
    # do not assume we were properly passed
    # a Path::Tiny::path
    $outputDir = Path::Tiny::path($outputDir);

  }

  method execute {
    say "=== Specialty Text to YAML Converter ===";
    $self->logger->debug(sprintf('specialty sees tree -- %s --', $tree->as_XML()));
    $self->getMainText();
    $self->parseText();
  }

  method parseText {
    foreach my $specialty (@$specialties) {
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
      Green  => [],
      Blue   => [],
      Purple => [],
      Orange => [],
      Gold   => [],
    );
    my $levels = \%temp;
    return $levels;
  }

  method create_skeleton_buff ($original_buff) {
    # Create a new buff object with the same properties but value = 0
    my $skeleton_value = Game::EvonyTKR::Model::Buff::Value->new(
      number => 0,
      unit   => $original_buff->value->unit  # Keep same unit (percentage, etc.)
    );

    my $skeleton_buff = Game::EvonyTKR::Model::Buff->new(
      attribute => $original_buff->attribute,
      value     => $skeleton_value,
      passive   => $original_buff->passive,
    );

    # Copy target if it exists
    if ($original_buff->targetedType) {
      $skeleton_buff->set_target($original_buff->targetedType);
    }

    # Copy conditions
    for my $condition ($original_buff->conditions) {
      $skeleton_buff->set_condition($condition);
    }

    return $skeleton_buff;
  }

  method printYAML ($name, $buffs) {

    my $levels = $self->setupLevelsHash();

    # Put actual buffs in Gold level
    for my $b (@{$buffs}) {
      push @{ $levels->{Gold} }, $b;
    }

    # Create skeleton buffs for other levels
    for my $level_name (qw/Green Blue Purple Orange/) {
      for my $gold_buff (@{$buffs}) {
        # Clone the buff but set value to 0
        my $skeleton_buff = $self->create_skeleton_buff($gold_buff);
        push @{ $levels->{$level_name} }, $skeleton_buff;
      }
    }

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
    $self->logger->trace("Start of ::Converter::Specialty->getMainText");

    $specialties = $self->extract_specialties();

    for my $specialty (@$specialties) {
      $self->logger->debug(
        sprintf('Specialty: "%s" Details: ', $specialty->{name}));
      for my $detail (@{ $specialty->{details} }) {
        $self->logger->debug("  - $detail");
      }
    }

  }

  method extract_specialties {
    # Find the container div
    my $container = $tree->look_down(
      '_tag'  => 'div',
      'class' =>  qr/entry-content.*th-content/
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
    my $target = 4;
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

    # Extract the 4 h3 tags after the second h2
    my @specialty_h3s;
    for my $i (($start_index + 1) .. ($start_index + 4)) {
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
