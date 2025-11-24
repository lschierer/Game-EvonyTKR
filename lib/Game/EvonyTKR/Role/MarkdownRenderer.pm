use v5.42.0;
use utf8::all;
use File::FindLib 'lib';
require Pandoc;
require YAML::PP;
require Mojo::DOM58;

package Game::EvonyTKR::Role::MarkdownRenderer {
  use Mojo::Base -role, -signatures;
  use Carp;

  # NOTE: This role has an interdependency on Game::EvonyTKR::Log::Config
  # The consuming class must also compose Role::Logger (or otherwise provide
  # a logger() method). ControllerBase composes both roles.

  my $customCommonMark = join('+',
    qw(commonmark alerts attributes autolink_bare_uris footnotes implicit_header_references pipe_tables raw_html rebase_relative_paths smart gfm_auto_identifiers)
  );

  # Lazily initialized Pandoc instance
  sub _pandoc ($self) {
    state $pandoc = Pandoc->new();
    return $pandoc;
  }

  # Pure function: Parse YAML frontmatter from a markdown file
  # Returns hashref with title, order, front_matter, yaml_data, content
  # Returns 0 on error
  sub parse_markdown_frontmatter ($self, $file_path) {
    my $logger = $self->logger;

    unless ($file_path && $file_path->isa('Mojo::File')) {
      $logger->error("file_path must be a 'Mojo::File' not "
          . (ref($file_path) || 'undefined'));
      return 0;
    }

    unless (-f $file_path) {
      if (-d $file_path) {
        if (-f $file_path->child('index.md')) {
          $file_path = $file_path->child('index.md');
        }
        else {
          $logger->error(
            "Cannot render a directory without an index.md file for $file_path"
          );
          return 0;
        }
      }
      else {
        $logger->error("Markdown file not found: $file_path");
        return 0;
      }
    }

    my $ypp = YAML::PP->new(
      schema       => [qw/ + Perl /],
      yaml_version => ['1.2', '1.1'],
    );

    my $front_matter = {};
    my $content      = $file_path->slurp('UTF-8');
    my $title        = $file_path->basename('.md');
    if ($content =~ s/^---\s*\n(.*?)\n---\s*\n//s) {
      my $yaml = $1;
      eval { $front_matter = $ypp->load_string($yaml); };
      if ($@) {
        $logger->error("Error parsing YAML front matter: $@");
      }
      elsif (ref $front_matter eq 'HASH') {
        $title = $front_matter->{title}
          if exists $front_matter->{title};
      }
    }

    my $order =
      100 + (exists $front_matter->{order} ? $front_matter->{order} : 0);
    return {
      title        => $title,
      order        => $order,
      front_matter => $front_matter,
      yaml_data    => $front_matter,
      content      => $content,
    };
  }

  # Pure function: Convert markdown content to HTML via Pandoc
  sub convert_markdown ($self, $content) {
    return '' unless defined $content && length($content);
    return $self->_pandoc->convert($customCommonMark => 'html', $content);
  }

  # Pure function: Convert a markdown snippet to HTML with Spectrum formatting
  sub render_markdown_snippet ($self, $snippet) {
    my $logger = $self->logger;

    if (not defined $snippet or length($snippet) == 0) {
      $logger->warn('snippet must be present!!');
      return '';
    }

    my $html_content = $self->convert_markdown($snippet);
    $html_content = $self->spectrum_formatting($html_content);
    $logger->debug("html_content for snippet is $html_content");
    return $html_content;
  }

  # Pure function: Apply Spectrum CSS classes to HTML
  sub spectrum_formatting ($self, $html_content) {
    my $dom = Mojo::DOM58->new($html_content);

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

    # Add emphasis class
    $dom->find('em')->each(sub {
      $_->attr(class => "spectrum-Body-emphasized");
    });

    # Add strong class
    $dom->find('strong')->each(sub {
      $_->attr(class => "spectrum-Body-strong");
    });

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

  # Controller method: Render a markdown file as a page
  # This method expects $self to be a Mojolicious::Controller
  sub render_markdown_page ($self, $app, $file_path, $opts = {}) {

    unless(defined($self->app)){
      $self->logger->error('app is not defined in render_markdown_page');
      return ;
    }

    my $startstash = $self->stash();
    my @stashkeys  = keys %$startstash;
    $self->logger->debug("stash at start of render_markdown_page has keys "
        . join(", ", @stashkeys));

    unless ($file_path && ref($file_path) && blessed($file_path)) {
      $self->logger->error("file_path must be a 'Mojo::File' not "
          . (ref($file_path) || 'undefined'));
      if(ref($file_path) eq 'HASH') {
        $self->logger->debug('file path is ' . Data::Printer::np($file_path));
      }
      return $self->reply->not_found;
    }
    unless($file_path->isa('Mojo::File')){
      $self->logger->error("file_path must be a 'Mojo::File' not "
          . (ref($file_path) || 'undefined'));
      return $self->reply->not_found;
    }

    my $parsedFile = $self->parse_markdown_frontmatter($file_path);
    unless ($parsedFile) {
      $self->logger->error("error parsing front matter for $file_path");
      return $self->reply->not_found;
    }

    # Only set stash values that aren't already set
    foreach my $key (keys %{ $parsedFile->{front_matter} }) {
      $self->stash($key => $parsedFile->{front_matter}->{$key})
        unless exists $self->stash->{$key};
    }

    # Only set title if not already set
    $self->stash(title => $parsedFile->{title}) unless exists $self->stash->{title};

    # Only set layout if not already set
    my $layout = $parsedFile->{front_matter}->{layout} // 'default';
    $layout =~ s/standard/default/;
    $self->stash(layout => $layout) unless exists $self->stash->{layout};

    $self->logger->debug("layout is " . $self->stash('layout'));

    # Use template from options, then stash, then default to 'markdown'
    my $template = $opts->{template} // $self->stash('template') // 'markdown';
    $self->logger->debug("Using template: $template");

    $self->logger->debug("Looking for template: $template.html.ep");

    my $html_content = $self->convert_markdown($parsedFile->{content});

    # Temporary debug logging for images
    if ($html_content =~ /<img/) {
      $self->logger->debug("Found img tags in HTML output");
    }
    else {
      $self->logger->debug(
        "No img tags found in HTML output. Raw content contains: "
          . (
          $parsedFile->{content} =~ /!\[.*?\]\(.*?\)/
          ? "image markdown syntax"
          : "no image markdown syntax"
          )
      );
    }

    $html_content = $self->spectrum_formatting($html_content);

    $self->logger->debug("html is now $html_content");
    # Add markdown content to stash but don't override existing content
    if (!exists $self->stash->{markdown_content}) {
      $self->stash(markdown_content => $html_content);
    }

    # Use existing content if available, otherwise use markdown content
    my $content  = $self->stash('content') // $html_content;
    my $endstash = $self->stash();
    $self->logger->debug("items type before render: "
        . (ref($endstash->{items}) || 'not a reference'));
    if (ref($endstash->{items}) eq 'HASH') {
      $self->logger->debug(
        "items has " . scalar(keys %{ $endstash->{items} }) . " keys");
    }
    $self->logger->debug("finally decided on template $template");
    return $self->render(
      template => $template,
      layout   => $self->stash('layout'),
      content  => $content
    );
  }
}
1;
__END__

=pod

=head1 NAME

Game::EvonyTKR::Role::MarkdownRenderer - Role for rendering markdown content in controllers

=head1 SYNOPSIS

  package MyController {
    use Mojo::Base 'Mojolicious::Controller';
    use Mojo::Base 'Game::EvonyTKR::Role::MarkdownRenderer', -role;

    sub some_page ($self) {
      my $md_path = Mojo::File->new('/path/to/page.md');
      return $self->render_markdown_page($md_path, { template => 'custom' });
    }
  }

=head1 METHODS

=head2 parse_markdown_frontmatter($file_path)

Parse YAML frontmatter from a Mojo::File. Returns hashref or 0 on error.

=head2 convert_markdown($content)

Convert markdown string to HTML via Pandoc.

=head2 render_markdown_snippet($snippet)

Convert a markdown snippet to HTML with Spectrum CSS formatting.

=head2 spectrum_formatting($html)

Apply Spectrum CSS classes to HTML content.

=head2 render_markdown_page($file_path, $opts)

Controller method to render a markdown file as a full page response.

=cut
