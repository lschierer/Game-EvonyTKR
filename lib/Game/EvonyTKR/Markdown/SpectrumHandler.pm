use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Markdent::Handler::HTMLStream::Fragment;
require Data::Printer;
require Scalar::Util;

package Game::EvonyTKR::Markdown::SpectrumHandler;

use Mojo::Base 'Markdent::Handler::HTMLStream::Fragment', -strict, -signatures;
use Markdent::CheckedOutput;
use Markdent::Types;
use Params::ValidationCompiler qw( validation_for );

# Override the start_paragraph method to add Spectrum CSS classes
sub start_paragraph {
  my $self = shift;

# Instead of calling the parent method directly, which would output the complete tag,
# we'll call _stream_start_tag with our custom attributes
  $self->_stream_start_tag(
    'p',
    {
      class => "spectrum-Body spectrum-Body--serif spectrum-Body--sizeM"
    }
  );

}

sub start_list_item {

  my $self = shift;

# Instead of calling the parent method directly, which would output the complete tag,
# we'll call _stream_start_tag with our custom attributes
  $self->_stream_start_tag(
    'li',
    {
      class => "spectrum-Body spectrum-Body--serif spectrum-Body--sizeM"
    }
  );
}

{
  my $validator = validation_for(
    params => {
      uri   => { type => t('Str') },
      title => {
        type     => t('Str'),
        optional => 1,
      },
      id => {
        type     => t('Str'),
        optional => 1,
      },
      is_implicit_id => {
        type     => t('Bool'),
        optional => 1,
      },
    }
  );

  sub start_link {
    my $self = shift;
    my %p    = $validator->(@_);

    delete @p{ grep { !defined $p{$_} } keys %p };

    $self->_stream_start_tag(
      'a',
      {
        href  => $p{uri},
        class => 'spectrum-Link spectrum-Link--primary spectrum-Link--quiet',
        exists $p{title} ? (title => $p{title}) : (),
      },
    );
  }
}

{
  my $validator = validation_for(
    params => [
      level => { type => t('HeaderLevel') },
    ],
    named_to_list => 1,
  );

  sub start_header {
    my $self = shift;
    my ($level) = $validator->(@_);

    my $spectrum = {
      1 => "spectrum-Heading spectrum-Heading--sizeXXL",
      2 => "spectrum-Heading spectrum-Heading--sizeXL",
      3 => "spectrum-Heading spectrum-Heading--sizeL",
      4 => "spectrum-Heading spectrum-Heading--sizeM",
      5 => "spectrum-Heading spectrum-Heading--sizeS",
      6 => "spectrum-Heading spectrum-Heading--sizeXS",
    };

    $self->_stream_start_tag(
      'h' . $level,
      {
        class => $spectrum->{$level},
      }
    );
  }
}

sub start_emphasis {
  my $self = shift;

  $self->_stream_start_tag(
    'em',
    {
      class => "spectrum-Body-emphasized"
    }
  );
}

sub start_strong {
  my $self = shift;

  $self->_stream_start_tag(
    'strong',
    {
      class => "spectrum-Body-strong"
    }
  );
}

1;
