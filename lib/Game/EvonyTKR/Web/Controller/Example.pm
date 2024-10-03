package Game::EvonyTKR::Web::Controller::Example;
# ABSTRACT: to be replaced
use Mojo::Base 'Mojolicious::Controller', -signatures;
# VERSION
# This action will render a template
sub welcome ($self) {

  # Render template "example/welcome.html.ep" with message
  $self->render(msg => 'Welcome to the Mojolicious real-time web framework!');
}

1;
