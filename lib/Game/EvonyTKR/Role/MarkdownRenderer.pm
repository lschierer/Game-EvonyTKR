use v5.40.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
use namespace::clean;

package Game::EvonyTKR::Role::MarkdownRenderer {
  use Mojo::Base -role, -signatures;
  use Mojo::File;
  use YAML::PP;
  use Text::MultiMarkdown;
  require Data::Printer;
  use Log::Log4perl;
  use Carp;

  our $VERSION = 'v0.01.0';
  # Create a logger for the entire package

  # Render markdown file with YAML front matter
  sub render_markdown_file($self, $c, $path) {
    my $logger = Log::Log4perl->get_logger(ref($self));

    $logger->debug("Rendering markdown file: $path with render_markdown_file");

    # Check if file exists
    my $file = Mojo::File->new($path);
    unless (-f $file) {
      $logger->error("Markdown file not found: $path");
      return $self->reply->not_found;
    }

    # Read file content
    my $content = $file->slurp;
    $logger->debug("found " . length($content) . " content from slurp");

    # Parse YAML front matter and markdown content
    my ($front_matter, $markdown) = $self->parse_front_matter($content, $file);
    $logger->trace("Got front matter "
        . Data::Printer::np($front_matter)
        . " in render_markdown_file for $file");

    # Convert markdown to HTML
    my $html = $self->markdown_to_html($markdown);

    # Add front matter data to stash
    foreach my $key (keys %$front_matter) {
      $c->stash($key => $front_matter->{$key});
    }

    # Set title if available in front matter
    $c->stash(title => $front_matter->{title}) if $front_matter->{title};

    $logger->trace("html is " . $html);

    # Use specified layout or default
    my $layout = $front_matter->{layout} // 'default';
    $layout =~ s/standard/default/;
    $c->stash(layout => $layout);

    $logger->trace("layout is $layout");

    $logger->trace("front matter keys: " . join(", ", keys %$front_matter));

    # Set content for rendering
    # Render with the specified layout
    return $c->render(
      template => "markdown",
      layout   => $layout,
      content  => $html,
    );

  }

  # Parse YAML front matter from content
  sub parse_front_matter($self, $content, $file) {
    my $class  = ref($self);
    my $logger = Log::Log4perl->get_logger($class);
    $logger->debug("start of parse_front_matter for $file");
    my $front_matter = {};
    my $markdown     = $content;

    # Check if content has front matter (starts with ---)
    if ($content =~ /^---\s*\n(.*?)\n---\s*\n(.*)/s) {
      my $yaml_content = $1;
      $markdown = $2;

      # Parse YAML
      my $yaml = YAML::PP->new;
      eval { $front_matter = $yaml->load_string($yaml_content); };
      if ($@) {
        $logger->error("Error parsing YAML front matter: $@");
      }
    }
    else {
      $logger->warn("no front matter for $file found by parse_front_matter");
    }
    $logger->debug("front matter is " . Data::Printer::np($front_matter));
    return ($front_matter, $markdown);
  }

  # Convert markdown to HTML
  sub markdown_to_html($c, $markdown) {
    my $parser = Text::MultiMarkdown->new(tab_width => 2,);
    return $parser->markdown($markdown);
  }
};

1;

__END__

#ABSTRACT: Base controller for rendering Markdown files with YAML front matter

=pod

=head1 DESCRIPTION

A base controller that provides functionality to render Markdown files with YAML front matter.
This can be used as a base class for controllers that need to render content from Markdown files.

=head1 METHODS

=head2 render_markdown_file($path)

Renders a Markdown file with YAML front matter. The front matter is parsed and added to the stash,
and the Markdown content is converted to HTML and rendered with the specified layout.

=head2 parse_front_matter($content)

Parses YAML front matter from content. Returns a hashref of front matter data and the remaining content.

=head2 markdown_to_html($markdown)

Converts Markdown to HTML using MultiMarkdown.

=cut
