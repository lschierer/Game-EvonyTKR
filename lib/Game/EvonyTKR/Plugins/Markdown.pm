use v5.40.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
use Mojo::File;
use Path::Iterator::Rule;
require Text::MultiMarkdown;
require Data::Printer;
require YAML::PP;

package Game::EvonyTKR::Plugins::Markdown {
  use Mojo::Base 'Mojolicious::Plugin', -strict, -signatures;
  use Carp;

  sub register ($self, $app, $config) {
    my $logger = Log::Log4perl->get_logger(__PACKAGE__);
    $logger->info("Registering static page routes");

    # Add helper method for rendering markdown files
    $app->helper(render_markdown_file => sub ($c, $file_path, $opts = {})  {
      $logger->debug("render_markdown_file requested for '$file_path'");
      return $self->_render_markdown_file($c, $file_path, $opts);
    });

    $app->helper(parse_markdown_frontmatter => sub ($c, $file_path) {
      return $self->_parse_markdown_frontmatter($file_path);
    });
  }

  sub _parse_markdown_frontmatter {
    my ($self, $file_path) = @_;
    my $logger = Log::Log4perl->get_logger(__PACKAGE__);

    unless ($file_path && $file_path->isa('Mojo::File')) {
      $logger->error("file_path must be a 'Mojo::File' not " . (ref($file_path) || 'undefined'));
      return 0;
    }

    unless (-f $file_path) {
      if( -d $file_path ) {
        if (-f $file_path->child('index.md') ){
          $file_path = $file_path->child('index.md');
        } else {
          $logger->error("Cannot render a directory without an index.md file for $file_path");
          return 0;
        }
      } else {
        $logger->error("Markdown file not found: $file_path");
        return 0;
      }
    }

    my $ypp = YAML::PP->new(
        schema       => [qw/ + Perl /],
        yaml_version => ['1.2', '1.1'],
      );

    my $front_matter = {};
    my $content   = $file_path->slurp('UTF-8');
    my $title = $file_path->basename('.md');
    if ($content =~ s/^---\s*\n(.*?)\n---\s*\n//s) {
      my $yaml = $1;
      eval { $front_matter = $ypp->load_string($yaml); };
      if ($@) {
        $logger->error("Error parsing YAML front matter: $@");
      }
      elsif (ref $front_matter eq 'HASH') {
        # Use title from front matter if available
        $title = $front_matter->{title}
          if exists $front_matter->{title};
      }
    }

    my $order = 100 + (exists $front_matter->{order} ? $front_matter->{order} : 0);
    return {
      title         => $title,
      order         => $order,
      front_matter  => $front_matter,
      content       => $content,
    }
  }

  # Private method to render markdown file
  sub _render_markdown_file ($self, $c, $file_path, $opts = {}) {
      if(not defined $opts) {
        $opts = {};
      }
      my $logger = Log::Log4perl->get_logger(__PACKAGE__);

      my $startstash = $c->stash();
      my @stashkeys = keys %$startstash;
      $logger->debug("stash at start of _render_markdown_file has keys " . join(", ", @stashkeys ) );

      unless ($file_path && $file_path->isa('Mojo::File')) {
          $logger->error("file_path must be a 'Mojo::File' not " . (ref($file_path) || 'undefined'));
          return $c->reply->not_found;
      }

      my $parsedFile = $self->_parse_markdown_frontmatter($file_path);
      unless ($parsedFile) {
          $logger->error("error parsing front matter for $file_path");
          return $c->reply->not_found;
      }

      # Only set stash values that aren't already set
      foreach my $key (keys %{ $parsedFile->{front_matter} }) {
          $c->stash($key => $parsedFile->{front_matter}->{$key})
              unless exists $c->stash->{$key};
      }

      # Only set title if not already set
      $c->stash(title => $parsedFile->{title}) unless exists $c->stash->{title};

      # Only set layout if not already set
      my $layout = $parsedFile->{front_matter}->{layout} // 'default';
      $layout =~ s/standard/default/;
      $c->stash(layout => $layout) unless exists $c->stash->{layout};

      $logger->trace("layout is " . $c->stash('layout'));

      # Use template from options, then stash, then default to 'markdown'
      my $template = $opts->{template} // $c->stash('template') // 'markdown';
      $logger->debug("Using template: $template");

      # Debug template paths
      $logger->debug("Template paths: " . join(", ", @{$c->app->renderer->paths}));
      $logger->debug("Looking for template: $template.html.ep");

      my $mm = Text::MultiMarkdown->new(
          tab_width   => 2,
          unicode_ids => 1,
      );

      # Convert markdown to HTML
      my $html_content = $mm->markdown($parsedFile->{content});

      # Add markdown content to stash but don't override existing content
      if (!exists $c->stash->{markdown_content}) {
          $c->stash(markdown_content => $html_content);
      }

      # Use existing content if available, otherwise use markdown content
      my $content = $c->stash('content') // $html_content;
      my $endstash = $c->stash();
      $logger->debug("items type before render: " . (ref($endstash->{items}) || 'not a reference'));
      if (ref($endstash->{items}) eq 'HASH') {
          $logger->debug("items has " . scalar(keys %{$endstash->{items}}) . " keys");
      }
      $logger->debug("finally decided on template $template");
      return $c->render(
          template => $template,
          layout => $c->stash('layout'),
          content => $content
      );
  }


};

1;
