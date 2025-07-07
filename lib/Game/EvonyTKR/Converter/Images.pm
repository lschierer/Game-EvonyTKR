use v5.40.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require HTML::TreeBuilder;
require HTTP::Tiny;
require Path::Tiny;
require IO::Socket::IP;    # for HTTP::Tiny;
require IO::Socket::SSL;
require GD;

class Game::EvonyTKR::Data::Images {
  use File::Share ':all';
  use Carp;
  our $VERSION = 'v0.01.0';

  field $generalDictionaryFile;
  field $tree;
  field $images  = {};
  field $distDir = Path::Tiny::path(dist_dir('Game::EvonyTKR::Data'));

  method getgeneralDictionaryFile {
    # this should ask the user where the general dictionary is.
    # I don't know how best to implement that. I'm hard coding it for now.

    my $dictionary = $distDir->child('GeneralDictionaryGrid.html');
    if ($dictionary->is_file()) {
      $generalDictionaryFile = $dictionary;
    }
    else {
      croak("$dictionary is not a file");
    }
  }

  method parseDictionary {
    if (defined $generalDictionaryFile) {
      $tree = HTML::TreeBuilder->new();
      $tree->parse_file($generalDictionaryFile->canonpath());
    }
    else {
      croak("ERROR: generalDictionaryFile is not defined");
    }
  }

  method getImageUrls {
    if (defined $tree) {
      foreach my $div ($tree->look_down('class', 'entry')) {
        my $name = $div->look_down(_tag => 'p')->as_trimmed_text();
        if (!exists $images->{$name}) {
          my $imgTag = $div->look_down(_tag => 'img');
          my %attr   = $imgTag->all_external_attr();
          $images->{$name}->{url} = $attr{'src'};
        }
      }
    }
  }

  method getImages {
    my $targetDir = $distDir->parent->child('data/images/generals/');
    $targetDir->mkdir({
      mode => 0710,
    });
    my $http  = HTTP::Tiny->new();
    my $index = 0;
    foreach my $name (sort(keys %{$images})) {
      if (exists $images->{$name}->{url}
        && (not $targetDir->child("$name.png")->is_file())) {
        my $url = $images->{$name}->{url};

        say "fetching image for $name from $url";
        my $request = $http->get($url);
        if ($request->{success}) {
          say("successful pull for $name");
          my $content = $request->{content};
          my $image;
          my $content_type = $request->{headers}{'content-type'} || '';

          if ($content_type =~ /image\/png/i) {
            $image = GD::Image->newFromPngData($content);
          }
          elsif ($content_type =~ /image\/jpe?g/i) {
            $image = GD::Image->newFromJpegData($content);
          }
          else {
            # Try both if content-type is unclear
            $image = GD::Image->new($content);
            if (!$image) {
              croak(sprintf(
                'cannot create image for %s out of %s',
                $name, Data::Printer::np($content_type)
              ));
            }
          }

          unless ($image) {
            croak("failed to create image for $name - invalid image data");
          }

          my $png_data = $image->png;
          $targetDir->child("$name.png")->spew_raw($png_data);

          say("saved $name.png (" . length($png_data) . " bytes)");

        }
        else {
          croak(sprintf(
            'image request failed for %s with status %s and reason %s.',
            $name, $request->{status}, $request->{reason}
          ));
        }
        if ($index++ == 6) {
          my $pause = 5;
          say "pausing for $pause seconds";
          sleep($pause);
          $index = 0;
        }
      }
    }
  }

  method execute {
    $self->getgeneralDictionaryFile();
    $self->parseDictionary();
    $self->getImageUrls();
    $self->getImages();
  }
}
1;
