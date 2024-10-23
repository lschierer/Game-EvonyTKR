use v5.40.0;
use experimental qw(class);
use File::FindLib 'lib';

package Game::EvonyTKR::Web::Controller::General::Conflicts {
  use Carp;
  use Data::Printer;
  use Devel::Peek;
  use File::ShareDir ':ALL';
  use File::Spec;
  use Util::Any ':all';
  use Game::EvonyTKR::Data;
  use Game::EvonyTKR::General;
  use Game::EvonyTKR::Web::Model::Generals::Conflicts;
  use HTML::FromANSI;
  use namespace::autoclean;
# VERSION
  use Mojo::Base 'Mojolicious::Controller', -signatures;

  # This action will render a template
  sub welcome ($self) {

    # Render template "example/welcome.html.ep" with message
    $self->render(msg => 'Welcome to the Mojolicious real-time web framework!');
  }

}
1;

__END__
# ABSTRACT: to be replaced

1;
