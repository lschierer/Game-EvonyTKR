package Game::EvonyTKR::Controller::Glossary;
use v5.42.0;
use utf8::all;
use Mooish::Base -standard;
extends 'Game::EvonyTKR::Controller::ControllerBase';

=head1 NAME

Game::EvonyTKR::Controller::Glossary - Controller for Glossary pages

=head1 DESCRIPTION

Handles the glossary of terms at /Reference/Glossary.

=cut

my $base = '/Reference/Glossary';

sub controller_name ($self) {
  return "Glossary";
}

sub getBase ($self) {
  return $base;
}

sub build ($self) {
  $self->logger->info("Building Glossary controller");

  # Call parent to register common routes
  $self->SUPER::build();

  # Add navigation
  $self->add_navigation_route($base, 'Glossary', { order => 80, parent => '/Reference' });

  # Main glossary page
  $self->router->add(
    $base,
    {
      to => sub ($self, $ctx) {
        return $self->index($ctx);
      },
      action => 'http.*',
    }
  );

  $self->logger->info("Registered Glossary routes");
}

sub index ($self, $ctx) {
  $self->logger->debug("Rendering glossary index");

  my $glossary_loader = $self->glossary_loader();

  unless ($glossary_loader) {
    return $self->render_error($ctx, 500, "Glossary data not loaded");
  }

  my $terms = $glossary_loader->get_all_terms();
  my $available_letters = $glossary_loader->get_available_letters();

  $self->logger->debug(sprintf(
    "Glossary has %d terms across %d letters",
    scalar(@$terms), scalar(@$available_letters)
  ));

  my $vars = {
    terms             => $terms,
    available_letters => $available_letters,
    linkBase          => $base,
    title             => 'Glossary of Terms',
    current_year      => (localtime)[5] + 1900,
    css_files         => ['/css/glossary.css'],
    sidebar           => 1,
    navigation        => $self->render_navigation($ctx->req->path),
    site_logo         => $self->site_logo(),
  };

  return $self->template('glossary/index.tt', $vars);
}

1;

__END__

=head1 ROUTES

=head2 GET /Reference/Glossary

Displays the glossary index with all terms grouped by letter.
Terms have anchors for deep linking from other pages.

=head1 AUTHOR

Game::EvonyTKR Development Team

=cut
