#!/usr/bin/env perl
use v5.42.0;
use experimental qw(class);
use utf8::all;
use Encode qw(encode_utf8);
use lib '../PAGI-WebServer/lib';
use lib 'lib';

use PAGI::WebServer;
use PAGI::WebServer::Router;
use PAGI::WebServer::Markdown;
use PAGI::WebServer::Navigation;
use PAGI::WebServer::Template;
use PAGI::Server;
use Future::AsyncAwait;
use Path::Tiny;
use IO::Async::Loop;
use Log::Log4perl qw(:easy);
use URI::Escape qw(uri_unescape);
use Game::EvonyTKR::Loader::Specialties;
use Game::EvonyTKR::Loader::Books;
use Game::EvonyTKR::Loader::AscendingAttributes;

# Initialize logging
Log::Log4perl->easy_init($DEBUG);

my $framework = PAGI::WebServer->new;
$framework->setup_logging;

my $markdown = PAGI::WebServer::Markdown->new;
my $template = PAGI::WebServer::Template->new(
    template_dir => 'templates',
    include_path => ['templates', 'templates/partials']
);
my $pages_dir = path('share/pages');

# Load specialty data
my $specialty_loader = Game::EvonyTKR::Loader::Specialties->new(
    data_dir => 'share/collections/data/specialties'
);
say "Loading specialties...";
my $specialty_count = $specialty_loader->load_all();
say "Loaded $specialty_count specialties";

# Load books data
my $books_loader = Game::EvonyTKR::Loader::Books->new(
    data_dir => 'share/collections/data'
);
say "Loading books...";
my $books_count = $books_loader->load_all();
say "Loaded $books_count books";

# Load ascending attributes data (no routes, just data for general pages)
my $ascending_loader = Game::EvonyTKR::Loader::AscendingAttributes->new(
    data_dir => 'share/collections/data/ascending attributes'
);
say "Loading ascending attributes...";
my $aa_count = $ascending_loader->load_all();
say "Loaded $aa_count ascending attributes";

# Create navigation
my $nav = PAGI::WebServer::Navigation->new;

# Create router
my $router = PAGI::WebServer::Router->new;

# Apply Spectrum CSS to HTML
sub apply_spectrum_css {
    my ($html) = @_;

    require Mojo::DOM58;
    my $dom = Mojo::DOM58->new($html);

    my %spectrum_h = (
        h1 => "spectrum-Heading spectrum-Heading--sizeXXL",
        h2 => "spectrum-Heading spectrum-Heading--sizeXL",
        h3 => "spectrum-Heading spectrum-Heading--sizeL",
        h4 => "spectrum-Heading spectrum-Heading--sizeM",
        h5 => "spectrum-Heading spectrum-Heading--sizeS",
        h6 => "spectrum-Heading spectrum-Heading--sizeXS",
    );

    for my $tag (keys %spectrum_h) {
        $dom->find($tag)->each(sub { $_->attr(class => $spectrum_h{$tag}) });
    }

    $dom->find('p')->each(sub {
        $_->attr(class => "spectrum-Body spectrum-Body--serif spectrum-Body--sizeM");
    });

    $dom->find('li')->each(sub {
        $_->attr(class => "spectrum-Body spectrum-Body--serif spectrum-Body--sizeM");
    });

    $dom->find('a')->each(sub {
        $_->attr(class => "spectrum-Link spectrum-Link--primary spectrum-Link--quiet");
    });

    $dom->find('em')->each(sub {
        $_->attr(class => "spectrum-Body-emphasized");
    });

    $dom->find('strong')->each(sub {
        $_->attr(class => "spectrum-Body-strong");
    });

    $dom->find('hr')->each(sub {
        $_->attr(class => 'spectrum-Divider spectrum-Divider--sizeM');
    });

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

# Auto-discover markdown pages for navigation
$pages_dir->visit(
    sub {
        my ($path) = @_;
        return if $path->is_dir;
        return unless $path =~ /\.md$/;

        # Convert file path to route path
        my $rel_path = $path->relative($pages_dir);
        my $route    = "/$rel_path";
        $route =~ s/\.md$//;
        $route =~ s|/index$||;    # Remove /index for index files

        # Parse frontmatter to get title and order
        my $content = $path->slurp_utf8;
        my ($frontmatter, $markdown_content) = $markdown->parse_frontmatter($content);

        # Use title from frontmatter, or generate from filename
        my $title = $frontmatter->{title};
        if (!$title) {
            $title = $path->basename;
            $title =~ s/\.md$//;
            $title =~ s/[-_]/ /g;
            $title =~ s/\b(\w)/\U$1/g;    # Capitalize words
        }

        # Get order from frontmatter - only if explicitly set
        my $options = {};
        if (defined $frontmatter->{order}) {
            $options->{order} = $frontmatter->{order};
        }

        # Skip policy pages (hardcoded exclusion like Mojolicious)
        return if $route =~ m{^/policy};

        # Add route to navigation (will default to 999 if no order specified)
        $nav->add_route($route, $title, $options);
    },
    { recurse => 1 }
);

# Add policy/privacy manually (not auto-discovered)
$nav->add_route('/policy/privacy', 'Privacy Policy', { order => 200 });
$nav->add_route('/Reference/Specialties', 'Specialties', { order => 10, parent => '/Reference' });
$nav->add_route('/Reference/Books', 'Books', { order => 20, parent => '/Reference' });
$nav->add_route('/Reference/Books/Skill', 'Skill Books', { order => 10, parent => '/Reference/Books' });
$nav->add_route('/Reference/Books/Generic', 'Generic Books', { order => 20, parent => '/Reference/Books' });

# Add individual specialties to navigation
my $specialty_order = 0;
for my $specialty_name (sort @{$specialty_loader->list_specialties}) {
    $nav->add_route("/Reference/Specialties/$specialty_name", $specialty_name, {
        order => $specialty_order++,
        parent => '/Reference/Specialties'
    });
}

# Add individual skill books to navigation
my $skill_order = 0;
for my $book_name (sort @{$books_loader->list_skill_books}) {
    $nav->add_route("/Reference/Books/Skill/$book_name", $book_name, {
        order => $skill_order++,
        parent => '/Reference/Books/Skill'
    });
}

# Add individual generic books to navigation
my $generic_order = 0;
for my $book_key (sort @{$books_loader->list_generic_books}) {
    my $book = $books_loader->get_generic_book($book_key);
    my $display_name = $book_key;  # Already has "Name (Level X)" format
    $nav->add_route("/Reference/Books/Generic/$book_key", $display_name, {
        order => $generic_order++,
        parent => '/Reference/Books/Generic'
    });
}

# Add route for homepage
$router->get('/' => async sub {
    my ($scope, $receive, $send) = @_;

    my $index_file = $pages_dir->child('index.md');

    unless ($index_file->exists) {
        await $send->({
            type    => 'http.response.start',
            status  => 404,
            headers => [['content-type', 'text/plain']],
        });
        await $send->({
            type => 'http.response.body',
            body => 'Homepage not found',
            more => 0,
        });
        return;
    }

    # Parse frontmatter and render markdown
    my ($frontmatter, $content_html) = $markdown->render_with_frontmatter($index_file->stringify);

    # Apply Spectrum CSS
    $content_html = apply_spectrum_css($content_html);

    my $title = $frontmatter->{title} || 'EvonyTKR';
    my $current_year = (localtime)[5] + 1900;
    my $navigation_html = $nav->render('/');

    my $vars = {
        content      => $content_html,
        title        => $title,
        current_year => $current_year,
        sidebar      => 0,  # Homepage doesn't have sidebar
        navigation   => $navigation_html,
    };

    my $html = $template->render('root/index.tt', $vars,
        { layout => 'layouts/default.tt' });

    my $bytes = encode_utf8($html);

    await $send->({
        type    => 'http.response.start',
        status  => 200,
        headers => [['content-type', 'text/html; charset=utf-8']],
    });
    await $send->({
        type => 'http.response.body',
        body => $bytes,
        more => 0,
    });
});

# Add route for CSS files
$router->get('/css/*' => async sub {
    my ($scope, $receive, $send) = @_;

    my $path = $scope->{path};
    my ($filename) = $path =~ m{^/css/(.+)$};

    if ($filename) {
        my $css_file = path('share/public/css')->child($filename);

        if ($css_file->exists && $css_file->is_file) {
            my $content = $css_file->slurp_utf8;
            my $bytes   = encode_utf8($content);

            await $send->({
                type    => 'http.response.start',
                status  => 200,
                headers => [['content-type', 'text/css; charset=utf-8']],
            });
            await $send->({
                type => 'http.response.body',
                body => $bytes,
                more => 0,
            });
            return;
        }
    }

    # CSS file not found
    await $send->({
        type    => 'http.response.start',
        status  => 404,
        headers => [['content-type', 'text/plain']],
    });
    await $send->({
        type => 'http.response.body',
        body => 'CSS Not Found',
        more => 0,
    });
});

# Add route for JS files
$router->get('/js/*' => async sub {
    my ($scope, $receive, $send) = @_;

    my $path = $scope->{path};
    my ($filename) = $path =~ m{^/js/(.+)$};

    if ($filename) {
        my $js_file = path('share/public/js')->child($filename);

        if ($js_file->exists && $js_file->is_file) {
            my $content = $js_file->slurp_utf8;
            my $bytes   = encode_utf8($content);

            # Determine content type based on file extension
            my $content_type = $filename =~ /\.js$/ ? 'application/javascript; charset=utf-8'
                             : $filename =~ /\.map$/ ? 'application/json; charset=utf-8'
                             : 'text/plain; charset=utf-8';

            await $send->({
                type    => 'http.response.start',
                status  => 200,
                headers => [['content-type', $content_type]],
            });
            await $send->({
                type => 'http.response.body',
                body => $bytes,
                more => 0,
            });
            return;
        }
    }

    # JS file not found
    await $send->({
        type    => 'http.response.start',
        status  => 404,
        headers => [['content-type', 'text/plain']],
    });
    await $send->({
        type => 'http.response.body',
        body => 'JS Not Found',
        more => 0,
    });
});

# Add route for image files
$router->get('/images/*' => async sub {
    my ($scope, $receive, $send) = @_;

    my $path = $scope->{path};
    my ($filename) = $path =~ m{^/images/(.+)$};

    if ($filename) {
        my $img_file = path('share/public/images')->child($filename);

        if ($img_file->exists && $img_file->is_file) {
            my $content = $img_file->slurp_raw;  # Binary content for images

            # Determine content type based on file extension
            my $content_type = $filename =~ /\.jpe?g$/i ? 'image/jpeg'
                             : $filename =~ /\.png$/i   ? 'image/png'
                             : $filename =~ /\.gif$/i   ? 'image/gif'
                             : $filename =~ /\.svg$/i   ? 'image/svg+xml'
                             : $filename =~ /\.webp$/i  ? 'image/webp'
                             : $filename =~ /\.ico$/i   ? 'image/x-icon'
                             : 'application/octet-stream';

            await $send->({
                type    => 'http.response.start',
                status  => 200,
                headers => [['content-type', $content_type]],
            });
            await $send->({
                type => 'http.response.body',
                body => $content,
                more => 0,
            });
            return;
        }
    }

    # Image file not found
    await $send->({
        type    => 'http.response.start',
        status  => 404,
        headers => [['content-type', 'text/plain']],
    });
    await $send->({
        type => 'http.response.body',
        body => 'Image Not Found',
        more => 0,
    });
});

# Add route for Specialties index
$router->get('/Reference/Specialties' => async sub {
    my ($scope, $receive, $send) = @_;

    my $items = [map { $specialty_loader->get_specialty($_) }
                 @{$specialty_loader->list_specialties}];

    my $html = $template->render('specialties/index.tt', {
        items => $items,
        collection_name => 'Specialties',
        title => 'Details of General Specialties',
        current_year => (localtime)[5] + 1900,
        sidebar => 1,
        navigation => $nav->render('/Reference/Specialties'),
    }, {
        layout => 'layouts/default.tt'
    });

    my $bytes = encode_utf8($html);

    await $send->({
        type    => 'http.response.start',
        status  => 200,
        headers => [['content-type', 'text/html; charset=utf-8']],
    });
    await $send->({
        type => 'http.response.body',
        body => $bytes,
        more => 0,
    });
});

# Add route for Specialties details
$router->get('/Reference/Specialties/*' => async sub {
    my ($scope, $receive, $send) = @_;

    my $path = $scope->{path};
    my ($specialty_name) = $path =~ m{^/Reference/Specialties/(.+?)/?$};

    my $item = $specialty_loader->get_specialty($specialty_name);

    unless ($item) {
        await $send->({
            type    => 'http.response.start',
            status  => 404,
            headers => [['content-type', 'text/plain']],
        });
        await $send->({
            type => 'http.response.body',
            body => 'Specialty not found',
            more => 0,
        });
        return;
    }

    my $html = $template->render('specialties/details.tt', {
        item => $item,
        title => "Details for the $specialty_name Specialty",
        current_year => (localtime)[5] + 1900,
        sidebar => 1,
        navigation => $nav->render("/Reference/Specialties/$specialty_name"),
    }, {
        layout => 'layouts/default.tt'
    });

    my $bytes = encode_utf8($html);

    await $send->({
        type    => 'http.response.start',
        status  => 200,
        headers => [['content-type', 'text/html; charset=utf-8']],
    });
    await $send->({
        type => 'http.response.body',
        body => $bytes,
        more => 0,
    });
});

# Books routes
# Main books index
$router->get('/Reference/Books' => async sub {
    my ($scope, $receive, $send) = @_;

    my $html = $template->render('books/index.tt', {
        title => 'Books',
        current_year => (localtime)[5] + 1900,
        sidebar => 1,
        navigation => $nav->render('/Reference/Books'),
    }, {
        layout => 'layouts/default.tt'
    });

    my $bytes = encode_utf8($html);

    await $send->({
        type    => 'http.response.start',
        status  => 200,
        headers => [['content-type', 'text/html; charset=utf-8']],
    });
    await $send->({
        type => 'http.response.body',
        body => $bytes,
        more => 0,
    });
});

# Skill books index
$router->get('/Reference/Books/Skill' => async sub {
    my ($scope, $receive, $send) = @_;

    my $items = [map { $books_loader->get_skill_book($_) }
                 @{$books_loader->list_skill_books}];

    my $html = $template->render('books/skill_books_index.tt', {
        title => 'Skill Books',
        items => $items,
        current_year => (localtime)[5] + 1900,
        sidebar => 1,
        navigation => $nav->render('/Reference/Books/Skill'),
    }, {
        layout => 'layouts/default.tt'
    });

    my $bytes = encode_utf8($html);

    await $send->({
        type    => 'http.response.start',
        status  => 200,
        headers => [['content-type', 'text/html; charset=utf-8']],
    });
    await $send->({
        type => 'http.response.body',
        body => $bytes,
        more => 0,
    });
});

# Generic books index
$router->get('/Reference/Books/Generic' => async sub {
    my ($scope, $receive, $send) = @_;

    my $items = [map { $books_loader->get_generic_book($_) }
                 @{$books_loader->list_generic_books}];

    my $html = $template->render('books/generic_books_index.tt', {
        title => 'Generic Books',
        items => $items,
        current_year => (localtime)[5] + 1900,
        sidebar => 1,
        navigation => $nav->render('/Reference/Books/Generic'),
    }, {
        layout => 'layouts/default.tt'
    });

    my $bytes = encode_utf8($html);

    await $send->({
        type    => 'http.response.start',
        status  => 200,
        headers => [['content-type', 'text/html; charset=utf-8']],
    });
    await $send->({
        type => 'http.response.body',
        body => $bytes,
        more => 0,
    });
});

# Skill book detail
$router->get('/Reference/Books/Skill/*' => async sub {
    my ($scope, $receive, $send) = @_;

    my $path = $scope->{path};
    my ($name) = $path =~ m{^/Reference/Books/Skill/(.+?)/?$};
    $name = uri_unescape($name) if defined $name;

    my $item = $books_loader->get_skill_book($name);

    unless ($item) {
        await $send->({
            type    => 'http.response.start',
            status  => 404,
            headers => [['content-type', 'text/html; charset=utf-8']],
        });
        await $send->({
            type => 'http.response.body',
            body => encode_utf8("<h1>Skill Book Not Found</h1>"),
            more => 0,
        });
        return;
    }

    my $html = $template->render('books/details.tt', {
        title => $item->name,
        item => $item,
        current_year => (localtime)[5] + 1900,
        sidebar => 1,
        navigation => $nav->render("/Reference/Books/Skill/$name"),
    }, {
        layout => 'layouts/default.tt'
    });

    my $bytes = encode_utf8($html);

    await $send->({
        type    => 'http.response.start',
        status  => 200,
        headers => [['content-type', 'text/html; charset=utf-8']],
    });
    await $send->({
        type => 'http.response.body',
        body => $bytes,
        more => 0,
    });
});

# Generic book detail
$router->get('/Reference/Books/Generic/*' => async sub {
    my ($scope, $receive, $send) = @_;

    my $path = $scope->{path};
    my ($name) = $path =~ m{^/Reference/Books/Generic/(.+?)/?$};
    $name = uri_unescape($name) if defined $name;

    my $item = $books_loader->get_generic_book($name);

    unless ($item) {
        await $send->({
            type    => 'http.response.start',
            status  => 404,
            headers => [['content-type', 'text/html; charset=utf-8']],
        });
        await $send->({
            type => 'http.response.body',
            body => encode_utf8("<h1>Generic Book Not Found</h1>"),
            more => 0,
        });
        return;
    }

    my $html = $template->render('books/details.tt', {
        title => $item->name,
        item => $item,
        current_year => (localtime)[5] + 1900,
        sidebar => 1,
        navigation => $nav->render("/Reference/Books/Generic/$name"),
    }, {
        layout => 'layouts/default.tt'
    });

    my $bytes = encode_utf8($html);

    await $send->({
        type    => 'http.response.start',
        status  => 200,
        headers => [['content-type', 'text/html; charset=utf-8']],
    });
    await $send->({
        type => 'http.response.body',
        body => $bytes,
        more => 0,
    });
});

# Add wildcard route for markdown pages
$router->get('*' => async sub {
    my ($scope, $receive, $send) = @_;

    my $path = $scope->{path};
    $path =~ s|^/||;  # Remove leading slash

    # Try exact path first
    my $md_file = $pages_dir->child("$path.md");

    # If not found, try as directory with index.md
    unless ($md_file->exists) {
        $md_file = $pages_dir->child($path, 'index.md');
    }

    unless ($md_file->exists) {
        await $send->({
            type    => 'http.response.start',
            status  => 404,
            headers => [['content-type', 'text/plain']],
        });
        await $send->({
            type => 'http.response.body',
            body => 'Page not found',
            more => 0,
        });
        return;
    }

    # Parse and render
    my ($frontmatter, $content_html) = $markdown->render_with_frontmatter($md_file->stringify);
    $content_html = apply_spectrum_css($content_html);

    my $title = $frontmatter->{title} || $path;
    my $current_year = (localtime)[5] + 1900;
    my $navigation_html = $nav->render($path);

    my $vars = {
        content      => $content_html,
        title        => $title,
        current_year => $current_year,
        sidebar      => 1,
        navigation   => $navigation_html,
    };

    my $html = $template->render('page/markdown.tt', $vars,
        { layout => 'layouts/default.tt' });

    my $bytes = encode_utf8($html);

    await $send->({
        type    => 'http.response.start',
        status  => 200,
        headers => [['content-type', 'text/html; charset=utf-8']],
    });
    await $send->({
        type => 'http.response.body',
        body => $bytes,
        more => 0,
    });
});

# Create event loop and server
my $loop = IO::Async::Loop->new;

my $server = PAGI::Server->new(
    app  => $router->to_app,
    host => '127.0.0.1',
    port => 3000,
);

say "Starting EvonyTKR PAGI server on 127.0.0.1:3000";

$loop->add($server);
$server->listen->get;

# Keep the event loop running
$loop->run;
