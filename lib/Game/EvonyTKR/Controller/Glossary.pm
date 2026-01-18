package Game::EvonyTKR::Controller::Glossary;
use v5.42.0;
use utf8::all;
use Mojo::Base 'Game::EvonyTKR::Controller::ControllerBase', -signatures;

sub controller_name ($c) {
  return "Glossary";
}

my $base = '/Reference/Glossary';

sub getBase ($c) {
  $base =~ s{/$}{};
  return $base;
}

sub register ($c, $app, $config = {}) {
  $c->logger->info("Registering routes for " . ref($c));
  $c->SUPER::register($app, $config);

  my @parts     = split(/::/, ref($c));
  my $baseClass = pop(@parts);

  my $controller_name =
      $c->can('controller_name')
    ? $c->controller_name()
    : $baseClass;

  $c->logger->debug("got controller_name $controller_name.");

  my $mainRoutes = $app->routes->any($base);
  $mainRoutes->get('/')
    ->to(controller => $controller_name, action => 'index')
    ->name("${base}_index");

  $app->add_navigation_item({
    title => 'Glossary of Terms',
    path  => $base,
    order => 60,
  });
}

sub prereqs ($c) {
  return ['load_all_glossary_terms'];
}

sub index ($c) {
  return if $c->check_prereqs_or_wait($c->prereqs);

  $c->logger->debug("Rendering glossary index");

  # Load all terms from persistence
  my $terms_data = $c->list_glossary_terms();

  # Convert wire format to objects
  require Game::EvonyTKR::Model::Glossary;
  my @terms;
  foreach my $td ($terms_data->@*) {
    my $tdo = Game::EvonyTKR::Model::Glossary->from_wire_hash($td);
    unless ($tdo) {
      $c->logger->error(
        sprintf('failed to create object from term %s',
          exists($td->{term}) ? $td->{term} : Data::Printer::np($td))
      );
      next;
    }
    $tdo->rendered_def($c->render_markdown_snippet($tdo->definition));
    $c->logger->debug(sprintf(
      'glossary definition for term "%s" before markdown rendering:: %s',
      $tdo->term, $tdo->definition
    ));
    $c->logger->debug(sprintf(
      'glossary definition for term "%s" after markdown rendering:: %s',
      $tdo->term, $tdo->rendered_def
    ));
    push @terms, $tdo;
  }

  # Sort terms alphabetically
  @terms = sort { lc($a->term) cmp lc($b->term) } @terms;

  # Build list of letters that have terms
  my %letters_with_terms;
  foreach my $term (@terms) {
    my $first_letter = $term->first_letter();
    $letters_with_terms{$first_letter} = 1;
  }
  my @letters = sort keys %letters_with_terms;

  $c->stash(
    terms             => \@terms,
    available_letters => \@letters,
    linkBase          => $base,
  );

  return $c->template(template => 'glossary/index');
}

1;
__END__

=head1 NAME

Game::EvonyTKR::Controller::Glossary - Glossary controller

=head1 DESCRIPTION

Displays glossary terms loaded from persistence.

=cut
