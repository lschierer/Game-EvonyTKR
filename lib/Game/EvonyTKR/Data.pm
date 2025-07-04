use v5.40.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Game::EvonyTKR::Data::Images;
require App::Cmd::Simple;

package Game::EvonyTKR::Data {
  use parent qw(App::Cmd::Simple);
  use Devel::Local;
  use Carp;
  our $VERSION = 'v0.01.0';

  sub opt_spec {
    return ([
      "mode" => hidden => {
        one_of => [
          ["images",  "Get the Image Data"],
          ['general', 'Convert a General to normalized YAML'],
        ],
        required => 1,
      }
    ]);
  }

  sub validate_args {
    my ($self, $opt, $args) = @_;
    # no args allowed but options!
    $self->usage_error("No args allowed") if @$args;

  }

  sub execute {
    my ($self, $opt, $args) = @_;
    #my $result = $opt->{blortex} ? blortex() : blort();
    if ($opt->mode eq 'images') {
      my $imageHander = Game::EvonyTKR::Data::Images->new();
      $imageHander->execute();
    }
    else {
      if (length($opt->mode) > 0) {
        croak(sprintf('Mode %s is not supported.', $opt->mode));
      }
      else {
        croak("missing mode option");
      }
    }

    return 0;
  }
}
1;
