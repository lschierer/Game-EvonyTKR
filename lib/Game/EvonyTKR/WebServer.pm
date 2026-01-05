package Game::EvonyTKR::WebServer;
use v5.42.0;
use Moo;
use FindBin;
use Path::Tiny;
use Log::Log4perl qw(get_logger);
use HTTP::Response;
use HTTP::Status qw(:constants);

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
    eval {
        return $handler->($request, $captures, $self);
    };

    if ($@) {
        $logger->error("Error handling request: $@");
        return $self->render_500($@);
    }
}

sub render_404 {
    my ($self, $request) = @_;

    return HTTP::Response->new(
        HTTP_NOT_FOUND,
        'Not Found',
        ['Content-Type' => 'text/html; charset=UTF-8'],
        '<h1>404 - Page Not Found</h1>'
    );
}

sub render_500 {
    my ($self, $error) = @_;

    my $logger = get_logger(__PACKAGE__);
    $logger->error("Internal server error: $error");

    return HTTP::Response->new(
        HTTP_INTERNAL_SERVER_ERROR,
        'Internal Server Error',
        ['Content-Type' => 'text/html; charset=UTF-8'],
        '<h1>500 - Internal Server Error</h1>'
    );
}

sub render_template {
    my ($self, $template_name, $vars, $opts) = @_;
    $opts //= {};

    my $logger = get_logger(__PACKAGE__);

    # Add navigation to vars
    $vars->{navigation} //= $self->navigation->render();

    # Add current year
    $vars->{current_year} //= (localtime)[5] + 1900;

    my $output;

    # Render with layout if specified
    if ($opts->{layout}) {
        # Render content first
        my $content;
        unless ($self->template->process($template_name, $vars, \$content)) {
            $logger->error("Template error: " . $self->template->error);
            return $self->render_500("Template error: " . $self->template->error);
        }

        # Then wrap in layout
        $vars->{content} = $content;
        unless ($self->template->process($opts->{layout}, $vars, \$output)) {
            $logger->error("Layout error: " . $self->template->error);
            return $self->render_500("Layout error: " . $self->template->error);
        }
    }
    else {
        unless ($self->template->process($template_name, $vars, \$output)) {
            $logger->error("Template error: " . $self->template->error);
            return $self->render_500("Template error: " . $self->template->error);
        }
    }

    return HTTP::Response->new(
        HTTP_OK,
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

__END__

=head1 NAME

Game::EvonyTKR::WebServer - PAGI-based web server for EvonyTKR

=head1 SYNOPSIS

    use Game::EvonyTKR::WebServer;

    my $server = Game::EvonyTKR::WebServer->new(
        port => 3000,
        host => '127.0.0.1',
    );

    $server->run();

=head1 DESCRIPTION

Main application class for the EvonyTKR PAGI-based web server.
Handles routing, template rendering, and request dispatching.

=cut
