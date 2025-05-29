use v5.40.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
use namespace::clean;

package Game::EvonyTKR::Plugins::ControllerBase {
  use Mojo::Base 'Mojolicious::Controller';
  use Mojo::Base 'Mojolicious::Plugin', -signatures, -role;
  use Log::Log4perl;
  require Mojo::File;
  require Text::MultiMarkdown;
  require YAML::PP;
  require Data::Printer;

  my $base = '';
  my $routes;

  sub getBase($self) {
    return $base;
  }

  sub register($self, $app, $config = {}) {
    my $logger = Log::Log4perl->get_logger(__PACKAGE__);
    $logger->info("Registering routes for " . ref($self));

    my @parts = split(/::/, ref($self));
    $base = pop(@parts);
    my $r = $app->routes;

    # Set up routes
    $routes = $r->any("/$base");
    $routes->get('/')
      ->to(controller => $base, action => 'index')
      ->name("${base}_index");

    $logger->info("Routes for $base registered successfully");
  }

  sub getRoutes($self) {
    return $routes;
  }

  sub index($self) {
    $self->stash(
      layout   => 'default',
      template => 'markdown',
      content  => "Hello from the $base Controller",
    );

    $self->render();
  }

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

    my $template = $self->stash('template') // 'markdown';
    $logger->debug("Using template: $template");

    $logger->trace("front matter keys: " . join(", ", keys %$front_matter));

    # Set content for rendering
    # Render with the specified layout
    return $c->render(
      template => $template,
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

}
1;
