package Game::EvonyTKR::Controller::Example{
  use Mojo::Base 'Mojolicious::Controller', -signatures;
  our $VERSION = 'v0.30.0';

  # This action will render a template
  sub welcome ($self) {

    # Render template "example/welcome.html.ep" with message
    $self->render(msg => 'Welcome to the Mojolicious real-time web framework!');
  }

}
1;

__END__

#ABSTRACT: A generic example from the Mojolicous generate

=pod

=head1 DESCRIPTION

a generic example controller from the Mojolicious generate

=cut
