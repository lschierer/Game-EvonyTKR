# Proof of Concept Implementation Guide

## Goal
Get the Game-EvonyTKR homepage working with PAGI-WebServer, replacing the Mojolicious Root controller with PAGI routing and templates.

## What We're Replacing

### Current (Mojolicious)
```
lib/Game/EvonyTKR/Controller/Root.pm    → PAGI route handler
lib/Game/EvonyTKR/Role/StaticPages.pm   → PAGI::WebServer::Role::MarkdownPages + custom
lib/Game/EvonyTKR/Role/MarkdownRenderer.pm → PAGI::WebServer::Markdown + Spectrum CSS
templates/root/index.html.ep → templates/root/index.tt
share/pages/index.md (unchanged)
```

### Target (PAGI)
```
bin/server.pl - Entry point
lib/Game/EvonyTKR/WebServer.pm - Main PAGI app
lib/Game/EvonyTKR/Role/Spectrum.pm - Spectrum CSS formatting
lib/Game/EvonyTKR/Handler/Root.pm - Root routes handler
templates/root/index.tt - Template Toolkit version
```

## Step-by-Step Implementation

### Step 1: Create Entry Point (`bin/server.pl`)

**File:** `Game-EvonyTKR/bin/server.pl`

```perl
#!/usr/bin/env perl
use v5.42.0;
use experimental qw(class);
use utf8::all;

use FindBin;
use lib "$FindBin::Bin/../lib";

use Game::EvonyTKR::WebServer;
use Log::Log4perl qw(:easy);

# Initialize logging
Log::Log4perl->easy_init($DEBUG);

# Create and run server
my $server = Game::EvonyTKR::WebServer->new(
    port => $ENV{PORT} || 3000,
    host => $ENV{HOST} || '127.0.0.1',
);

say "Starting EvonyTKR PAGI server on $server->{host}:$server->{port}";
$server->run();
```

### Step 2: Create Main PAGI Application

**File:** `Game-EvonyTKR/lib/Game/EvonyTKR/WebServer.pm`

```perl
package Game::EvonyTKR::WebServer;
use v5.42.0;
use Moo;
use FindBin;
use Path::Tiny;
use Log::Log4perl qw(get_logger);

use PAGI::WebServer;
use PAGI::WebServer::Router;
use PAGI::WebServer::Navigation;
use Template;

# Import handler modules
use Game::EvonyTKR::Handler::Root;

has host => (
    is => 'ro',
    default => '127.0.0.1',
);

has port => (
    is => 'ro',
    default => 3000,
);

has pagi => (
    is => 'lazy',
);

has router => (
    is => 'lazy',
);

has navigation => (
    is => 'lazy',
);

has template => (
    is => 'lazy',
);

sub _build_pagi {
    my $self = shift;

    return PAGI::WebServer->new(
        host => $self->host,
        port => $self->port,
        handler => sub {
            my ($request) = @_;
            return $self->handle_request($request);
        },
    );
}

sub _build_router {
    my $self = shift;
    return PAGI::WebServer::Router->new;
}

sub _build_navigation {
    my $self = shift;
    return PAGI::WebServer::Navigation->new;
}

sub _build_template {
    my $self = shift;

    my $templates_dir = path($FindBin::Bin)->parent->child('templates');

    return Template->new({
        INCLUDE_PATH => $templates_dir->stringify,
        INTERPOLATE  => 0,
        EVAL_PERL    => 0,
        ENCODING     => 'UTF-8',
    });
}

sub BUILD {
    my $self = shift;

    my $logger = get_logger(__PACKAGE__);
    $logger->info("Initializing Game::EvonyTKR::WebServer");

    # Register handlers
    Game::EvonyTKR::Handler::Root->register($self);

    $logger->info("Routes registered");
}

sub handle_request {
    my ($self, $request) = @_;

    my $logger = get_logger(__PACKAGE__);
    my $path = $request->uri->path;

    $logger->debug("Handling request for: $path");

    # Route the request
    my ($handler, $captures) = $self->router->match(
        $request->method,
        $path
    );

    unless ($handler) {
        $logger->warn("No route found for $path");
        return $self->render_404($request);
    }

    # Call handler
    return $handler->($request, $captures, $self);
}

sub render_404 {
    my ($self, $request) = @_;

    return HTTP::Response->new(
        404,
        'Not Found',
        ['Content-Type' => 'text/html; charset=UTF-8'],
        '<h1>404 - Page Not Found</h1>'
    );
}

sub render_template {
    my ($self, $template_name, $vars, $opts) = @_;
    $opts //= {};

    my $logger = get_logger(__PACKAGE__);

    # Add navigation to vars
    $vars->{navigation} //= $self->navigation->render();

    my $output;

    # Render with layout if specified
    if ($opts->{layout}) {
        # Render content first
        my $content;
        unless ($self->template->process($template_name, $vars, \$content)) {
            $logger->error("Template error: " . $self->template->error);
            return;
        }

        # Then wrap in layout
        $vars->{content} = $content;
        unless ($self->template->process($opts->{layout}, $vars, \$output)) {
            $logger->error("Layout error: " . $self->template->error);
            return;
        }
    }
    else {
        unless ($self->template->process($template_name, $vars, \$output)) {
            $logger->error("Template error: " . $self->template->error);
            return;
        }
    }

    return HTTP::Response->new(
        200,
        'OK',
        ['Content-Type' => 'text/html; charset=UTF-8'],
        $output
    );
}

sub run {
    my $self = shift;
    $self->pagi->run();
}

1;
```

### Step 3: Create Spectrum CSS Role

**File:** `Game-EvonyTKR/lib/Game/EvonyTKR/Role/Spectrum.pm`

```perl
package Game::EvonyTKR::Role::Spectrum;
use v5.42.0;
use Moo::Role;
use Mojo::DOM58;

# Apply Spectrum CSS classes to HTML content
sub apply_spectrum_css {
    my ($self, $html) = @_;

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
        $_->attr(class => "spectrum-Body spectrum-Body--serif spectrum-Body--sizeM");
    });

    # Add list item classes
    $dom->find('li')->each(sub {
        $_->attr(class => "spectrum-Body spectrum-Body--serif spectrum-Body--sizeM");
    });

    # Add link classes
    $dom->find('a')->each(sub {
        $_->attr(class => "spectrum-Link spectrum-Link--primary spectrum-Link--quiet");
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
```

### Step 4: Create Root Handler

**File:** `Game-EvonyTKR/lib/Game/EvonyTKR/Handler/Root.pm`

```perl
package Game::EvonyTKR::Handler::Root;
use v5.42.0;
use Moo;
use Path::Tiny;
use FindBin;
use Log::Log4perl qw(get_logger);

use PAGI::WebServer::Markdown;

with 'Game::EvonyTKR::Role::Spectrum';

has markdown => (
    is => 'lazy',
    default => sub { PAGI::WebServer::Markdown->new },
);

sub register {
    my ($class, $app) = @_;

    my $logger = get_logger(__PACKAGE__);
    $logger->info("Registering Root handler routes");

    # Register routes
    $app->router->add_route(
        GET => qr{^/$},
        sub { $class->new->index(@_) }
    );

    $app->router->add_route(
        GET => qr{^/Reference$},
        sub { $class->new->single_page(@_) }
    );

    $app->router->add_route(
        GET => qr{^/policy/privacy$},
        sub { $class->new->single_page(@_) }
    );

    # Add navigation items
    $app->navigation->add_item({
        title => 'Home',
        path  => '/',
        order => 0,
    });

    $app->navigation->add_item({
        title => 'Reference',
        path  => '/Reference',
        order => 104,
    });

    $app->navigation->add_item({
        title => 'Privacy Policy',
        path  => '/policy/privacy',
        order => 200,
    });
}

sub index {
    my ($self, $request, $captures, $app) = @_;

    my $logger = get_logger(__PACKAGE__);
    $logger->debug("Rendering root index");

    my $md_file = path($FindBin::Bin)->parent->child('share/pages/index.md');

    unless ($md_file->exists) {
        $logger->error("index.md not found at $md_file");
        return HTTP::Response->new(
            500,
            'Error',
            ['Content-Type' => 'text/html'],
            '<h1>Error</h1><p>Homepage not found</p>'
        );
    }

    # Parse frontmatter and render markdown
    my ($frontmatter, $html) = $self->markdown->render_with_frontmatter($md_file->stringify);

    # Apply Spectrum CSS
    my $styled_html = $self->apply_spectrum_css($html);

    my $title = $frontmatter->{title} || 'EvonyTKR';

    return $app->render_template('root/index.tt', {
        content => $styled_html,
        title   => $title,
    }, {
        layout => 'layouts/default.tt'
    });
}

sub single_page {
    my ($self, $request, $captures, $app) = @_;

    my $logger = get_logger(__PACKAGE__);
    my $path = $request->uri->path;
    $logger->debug("Rendering static page: $path");

    # Convert URL path to file path
    my $md_file = path($FindBin::Bin)->parent->child("share/pages$path.md");

    # Try index.md if directory
    unless ($md_file->exists) {
        $md_file = path($FindBin::Bin)->parent->child("share/pages$path/index.md");
    }

    unless ($md_file->exists) {
        $logger->warn("Markdown file not found: $md_file");
        return HTTP::Response->new(
            404,
            'Not Found',
            ['Content-Type' => 'text/html'],
            '<h1>404</h1><p>Page not found</p>'
        );
    }

    # Parse and render
    my ($frontmatter, $html) = $self->markdown->render_with_frontmatter($md_file->stringify);
    my $styled_html = $self->apply_spectrum_css($html);

    my $title = $frontmatter->{title} || $path;

    return $app->render_template('page/markdown.tt', {
        content => $styled_html,
        title   => $title,
    }, {
        layout => 'layouts/default.tt'
    });
}

1;
```

### Step 5: Convert Templates

**File:** `Game-EvonyTKR/templates/root/index.tt`

```html
[% content %]
```

That's it! The content is already rendered HTML from the markdown file, wrapped by the layout.

**File:** `Game-EvonyTKR/templates/page/markdown.tt`

```html
[% content %]
```

Same simple wrapper for markdown pages.

The layout template (`templates/layouts/default.tt`) should already work from Moj

olicious, just update the template syntax if needed.

## Testing the PoC

### 1. Start the server

```bash
cd Game-EvonyTKR
chmod +x bin/server.pl
./bin/server.pl
```

### 2. Test homepage

```bash
curl http://127.0.0.1:3000/
```

Should return the rendered index.md with Spectrum CSS classes.

### 3. Compare with Mojolicious

```bash
# Start Mojolicious version
./bin/game-evonytkr daemon

# Start PAGI version
./bin/server.pl

# Compare HTML output
diff <(curl -s http://127.0.0.1:3000/ | tidy -i) \
     <(curl -s http://127.0.0.1:3001/ | tidy -i)
```

## Success Criteria

- [ ] Server starts without errors
- [ ] Homepage loads at http://127.0.0.1:3000/
- [ ] Markdown is converted to HTML
- [ ] Spectrum CSS classes are applied
- [ ] Navigation renders
- [ ] Layout template wraps content
- [ ] HTML output matches Mojolicious version (modulo minor whitespace)

## Next Steps After PoC

Once the PoC works:
1. Add static file serving (CSS, JS, images)
2. Implement async data loading helper
3. Migrate Specialties controller
4. Continue with Books, Ascending Attributes, etc.
