use v5.42.0;
use utf8::all;
use File::FindLib 'lib';
use Mojo::File;
use namespace::autoclean;

package Game::EvonyTKR::Controller::Root {
  use Mojo::Base 'Game::EvonyTKR::Controller::ControllerBase';
  use Mojo::Base 'Game::EvonyTKR::Role::StaticPages', -role;
  use Mojo::Home;
  use Carp;

  sub register ($c, $app, $config = {}) {
    $c->logger->info("Registering root landing page route");
    $c->SUPER::register($app, $config);

    # Register the root route
    $app->routes->get('/')->to(
      controller => 'Root',
      action     => 'index'
    )->name('root_index');

    $app->add_navigation_item({
      title => 'Home',
      path  => '/',
      order => 0,
    });


    $app->routes->get('/Reference')->to(
      controller => 'Root',
      action     => 'single_page'
    )->name('root_index');

    $app->add_navigation_item({
      title => 'Reference',
      path  => '/Reference',
      order => 0,
    });

    $app->routes->get('/policy/privacy')->to(
      controller => 'Root',
      action     => 'single_page'
    )->name('root_index');

    $app->add_navigation_item({
      title => 'Privacy Policy',
      path  => '/policy/privacy',
      order => 0,
    });
  }


  sub index ($c) {
    unless($c){
      croak('controller is undefined in root index method');
      return;
    }
    my $home = Mojo::Home->new->detect;
    my $index_path = $home->child('share/pages/index.md');

    $c->logger->debug("Rendering root index from $index_path");

    unless (-f $index_path) {
      $c->logger->error("Root index.md not found at $index_path");
      return $c->render(
        template => 'markdown',
        layout   => 'default',
        content  => '<p>Welcome to EvonyTKR</p>',
      );
    }

    unless($index_path && ref($index_path) && $index_path->isa('Mojo::File')){
      $index_path = Mojo::File->new($index_path);
    }
    $c->logger->debug(sprintf('root index is a "%s"', $index_path->isa('Mojo::File') ? 'Mojo::File' : ref($index_path) ? ref($index_path) : 'scalar'));

    return $c->render_markdown_page($c->app, $index_path,  { template => 'root/index'});
  }
}

1;
__END__

=pod

=head1 NAME

Game::EvonyTKR::Controller::Root - Controller for the root landing page

=head1 DESCRIPTION

Handles the root route (/) by rendering share/pages/index.md as the landing page.

=cut
