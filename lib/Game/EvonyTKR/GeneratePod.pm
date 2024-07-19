package Game::EvonyTKR::GeneratePod;
use v5.40.0;
use Carp;
use Pod::Weaver;
use File::Slurp;
use PPI::Document;
use namespace::autoclean;
# ABSTRACT: Generate POD docs for Game::EvonyTKR distribution

our $VERSION = 'v0.0.3';

sub opt_spec {
  return (["version|v", "print version"],);
}

sub validate_args {
  my ($self, $opt, $args) = @_;

  # no args allowed but options!
  $self->usage_error("No args allowed") if @$args;
}

sub execute {
  my ($self, $opt, $args) = @_;

  if ($opt->{version}) {
    say $VERSION;
    return;
  }

  my $filename = shift @ARGV;

  # create Pod::Weaver engine, taking 'weaver.ini' config
  # into account
  my $weaver = Pod::Weaver->new_from_config;

  my $perl = File::Slurp::read_file($filename);

  # get PPI DOM of the file to process
  my $ppi = PPI::Document->new(\$perl);

  # get its POD as an Pod::Elemental DOM object
  my $pod = Pod::Elemental->read_string(join '',
    @{ $ppi->find('PPI::Token::Pod') || [] });

  # ...and remove it from the PPI
  $ppi->prune('PPI::Token::Pod');

  # weaver, do your stuff
  my $doc = $weaver->weave_document({
    pod_document => $pod,
    ppi_document => $ppi,
  });

  # print the generated POD
  print $doc->as_pod_string;

}
