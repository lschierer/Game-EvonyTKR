package Game::EvonyTKR::Handler::Root;
use v5.42.0;
use utf8::all;
use Moo;
use Path::Tiny;
use FindBin;
use Log::Log4perl qw(get_logger);
use HTTP::Response;
use HTTP::Status qw(:constants);

use PAGI::WebServer::Markdown;

with 'Game::EvonyTKR::Role::Spectrum';

has markdown => (
    is => 'lazy',
    default => sub { PAGI::WebServer::Markdown->new },
);

=head1 NAME

Game::EvonyTKR::Handler::Root - Handler for root routes

=head1 SYNOPSIS

    Game::EvonyTKR::Handler::Root->register($app);

=head1 DESCRIPTION

Handles the root homepage and static markdown pages.

=head1 METHODS

=head2 register($app)

Register routes and navigation items with the application.

=cut

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

=head2 index($request, $captures, $app)

Render the homepage from share/pages/index.md

=cut

sub index {
    my ($self, $request, $captures, $app) = @_;

    my $logger = get_logger(__PACKAGE__);
    $logger->debug("Rendering root index");

    my $md_file = path($FindBin::Bin)->parent->child('share/pages/index.md');

    unless ($md_file->exists) {
        $logger->error("index.md not found at $md_file");
        return HTTP::Response->new(
            HTTP_INTERNAL_SERVER_ERROR,
            'Error',
            ['Content-Type' => 'text/html; charset=UTF-8'],
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

=head2 single_page($request, $captures, $app)

Render a static markdown page from share/pages/

=cut

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
            HTTP_NOT_FOUND,
            'Not Found',
            ['Content-Type' => 'text/html; charset=UTF-8'],
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
