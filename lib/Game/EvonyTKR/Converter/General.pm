use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require Path::Tiny;
require Readonly;
require JSON::PP;
require YAML::PP;
require Game::EvonyTKR::Model::BasicAttributes;
require Game::EvonyTKR::Model::BasicAttribute;

class Game::EvonyTKR::Converter::General :
  isa(Game::EvonyTKR::Shared::Constants) {
  use List::AllUtils qw( first all any none );
  use namespace::autoclean;

  field $tree      : param;
  field $outputDir : param;
  field $debug     : param //= 0;

  field $name = '';

  field $basic = Game::EvonyTKR::Model::BasicAttributes->new();
  field $book  = '';

  field $specialties;

  field $ascending = 0;
  field $stars     = 'none';

  field @types;

  # internal control fields
  field $helpers = Game::EvonyTKR::Converter::Helpers->new(debug => $debug);

  method getPrimaryFields {
    my $statsTable = $tree->look_down(
      '_tag'  => 'table',
      'class' => qr/stats-table/,
    );
    if ($statsTable) {
      $self->getPrimaryFields_Template2();
    }
    else {
      $self->getPrimaryFields_Template1();
    }
  }

  method printYAML {
    say
"This makes a ton of assumptions because I cannot reliably find all the info needed on the source page.";
    my $data = {
      name             => $name,
      basic_attributes => {
        leadership => {
          base      => $basic->leadership->base,
          increment => $basic->leadership->increment,
        },
        attack => {
          base      => $basic->attack->base,
          increment => $basic->attack->increment,
        },
        defense => {
          base      => $basic->defense->base,
          increment => $basic->defense->increment,
        },
        politics => {
          base      => $basic->politics->base,
          increment => $basic->politics->increment,
        },
      },
      book        => '',
      specialties => '',
      ascending   => 'true',
      stars       => 'red5',
      type        => '',
    };
    $self->logger->debug(sprintf('general is %s', Data::Printer::np($data)));
    say YAML::PP->new(
      schema       => [qw/ + Perl /],
      yaml_version => ['1.2', '1.1'],
    )->dump($data);

  }

  method execute {
    say "=== Basic Stats ===";
    $self->logger->info("=== Basic Stats ===");
    $self->getPrimaryFields();
    $self->printYAML();
  }

  method getPrimaryFields_Template2 {

    my $h1 = $tree->look_down(
      '_tag'    => qr/^h1$/,
      'class'   => qr/elementor-heading-title.elementor-size-default/,
    );
    if($h1){
      $name = $h1->as_trimmed_text =~ s/Evony\s+(.*)$/$1/r;
    }

    my $container = $tree->look_down(
      '_tag'  => 'div',
      'class' => qr/\w+-stats-left/
    );

    unless ($container) {
      $self->logger->error("Cannot find $container for stats");
      return;
    }
    my $statsTable = $container->look_down('_tag' => 'table');
    unless ($statsTable) {
      $self->logger->error("Cannot find $statsTable for stats");
      return;
    }
    my @rows = $statsTable->look_down('_tag', 'tr');
    my @keys = qw(leadership attack defense politics);
    if(scalar(@rows) != scalar(@keys)){
      $self->error("something wrong with this page, too many rows found");
      return;
    }
    foreach my $index (0..$#keys){
      my $row = $rows[$index];
      my $key = $keys[$index];
      my @cells = $row->look_down('_tag' => 'td');
      my $value;
      my $increment;
      if($cells[0]->as_trimmed_text =~ /$key/i) {
        $value = scalar(@cells) >= 1 ? $cells[1]->as_trimmed_text : 0;
        $increment = scalar(@cells) >= 3 ? $cells[3]->as_trimmed_text : 0;
      } else {
        $value = scalar(@cells) >= 2 ? $cells[2]->as_trimmed_text : 0;
        $increment = scalar(@cells) >= 4 ? $cells[4]->as_trimmed_text : 0;
      }

      my $ba = Game::EvonyTKR::Model::BasicAttribute->new(
        base            => $value,
        increment       => $increment,
        attribute_name  => $key,
      );
      $basic->setAttribute($key, $ba);
    }
  }

  method getPrimaryFields_Template1 {
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

    # Find the First h2 (start of the pertinent info)
    my $target   = 1;
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

    my $targetH2 = $headers[$start_index];
    unless ($targetH2) {
      $self->logger->error("Cannot find targetH2");
    }

    $name = $targetH2->as_trimmed_text =~ s/(.+?)\s*[-–—]\s*Stats:/$1/r;
    $self->logger->debug("name is $name");

    my $statsH3 = $headers[$start_index + 1];
    # the attributes are in the next 4 paragraph elements.
    my @keys = qw(leadership attack defense politics);
    my @paras = $helpers->find_all_p_after_element($statsH3);

    if (@paras < @keys) {
        $self->logger->error("Expected 4 stats <p> tags after H3, found " . scalar @paras);
    } else {
        for my $i (0..$#keys) {
            my $key = $keys[$i];
            my $text = $paras[$i]->as_trimmed_text;
            my ($value) = $text =~ /\Q$key\E\s*:\s*([0-9.]+)/i;
            if (defined $value) {
                $self->logger->debug("setting $key to $value");
                my $ba = Game::EvonyTKR::Model::BasicAttribute->new(
                  base            => $value,
                  increment       => 0,
                  attribute_name  => $key,
                );
                $basic->setAttribute($key, $ba);
            } else {
                $self->logger->warn("Could not extract $key value from '$text'");
            }
        }
    }

  }

}

1;
__END__
