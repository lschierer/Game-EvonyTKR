package Game::EvonyTKR::Role::Spectrum;
use v5.42.0;
use Moo::Role;
use Mojo::DOM58;

=head1 NAME

Game::EvonyTKR::Role::Spectrum - Apply Spectrum CSS classes to HTML

=head1 SYNOPSIS

    package MyHandler;
    use Moo;
    with 'Game::EvonyTKR::Role::Spectrum';

    sub render_page {
        my $self = shift;
        my $html = "<h1>Title</h1><p>Content</p>";
        my $styled = $self->apply_spectrum_css($html);
    }

=head1 DESCRIPTION

Provides functionality to apply Adobe Spectrum CSS classes to HTML content.
This ensures consistent styling across the application.

=head1 METHODS

=head2 apply_spectrum_css($html)

Applies Spectrum CSS classes to HTML content. Returns the modified HTML string.

=cut

sub apply_spectrum_css {
  my ($self, $html) = @_;

  return '' unless defined $html && length($html);

  my $dom = Mojo::DOM58->new($html);

  my %spectrum_h = (
    h1 => "spectrum-Heading spectrum-Heading--sizeXXL",
    h2 => "spectrum-Heading spectrum-Heading--sizeXL",
    h3 => "spectrum-Heading spectrum-Heading--sizeL",
    h4 => "spectrum-Heading spectrum-Heading--sizeM",
    h5 => "spectrum-Heading spectrum-Heading--sizeS",
    h6 => "spectrum-Heading spectrum-Heading--sizeXS",
  );

  # Add header classes
  for my $tag (keys %spectrum_h) {
    $dom->find($tag)->each(sub { $_->attr(class => $spectrum_h{$tag}) });
  }

  # Add paragraph classes
  $dom->find('p')->each(sub {
    $_->attr(
      class => "spectrum-Body spectrum-Body--serif spectrum-Body--sizeM");
  });

  # Add list item classes
  $dom->find('li')->each(sub {
    $_->attr(
      class => "spectrum-Body spectrum-Body--serif spectrum-Body--sizeM");
  });

  # Add link classes
  $dom->find('a')->each(sub {
    $_->attr(
      class => "spectrum-Link spectrum-Link--primary spectrum-Link--quiet");
  });

  # Add emphasis and strong classes
  $dom->find('em')->each(sub {
    $_->attr(class => "spectrum-Body-emphasized");
  });

  $dom->find('strong')->each(sub {
    $_->attr(class => "spectrum-Body-strong");
  });

  # Add divider class
  $dom->find('hr')->each(sub {
    $_->attr(class => 'spectrum-Divider spectrum-Divider--sizeM');
  });

  # Add table classes
  $dom->find('table')->each(sub {
    $_->attr(class => 'spectrum-Table spectrum-Table--sizeM');
  });

  $dom->find('thead')->each(sub {
    $_->attr(class => 'spectrum-Table-head');
  });

  $dom->find('tbody')->each(sub {
    $_->attr(class => 'spectrum-Table-body');
  });

  $dom->find('th')->each(sub {
    $_->attr(class => 'spectrum-Table-headCell');
  });

  $dom->find('td')->each(sub {
    $_->attr(class => 'spectrum-Table-cell');
  });

  $dom->find('tr')->each(sub {
    $_->attr(class => 'spectrum-Table-row');
  });

  return $dom->to_string;
}

1;
