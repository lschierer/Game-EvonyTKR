use v5.40.0;
use experimental qw(class);
use utf8::all;
require Data::Printer;
require App::Cmd::Simple;
use namespace::clean;

package Game::EvonyTKR::Importer {
  use parent qw(App::Cmd::Simple);
  use File::FindLib 'lib';
  require Game::EvonyTKR::Importer::General;
  require Game::EvonyTKR::Importer::SkillBook;
  use Carp;
  use Log::Log4perl qw(:easy);
  our $VERSION = 'v0.01.0';

  sub opt_spec {
    return ([
      "mode" => hidden => {
        one_of => [
          ['general',   'Convert a General to normalized YAML'],
          ['skillbook', 'Convert SkillBook Text to normalized YAML'],
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
    Log::Log4perl::Config->utf8(1);
    Log::Log4perl->easy_init($DEBUG);
    #my $result = $opt->{blortex} ? blortex() : blort();
    if ($opt->mode eq 'general') {
      my $generalHander = Game::EvonyTKR::Importer::General->new();
      $generalHander->execute();
    }
    if ($opt->mode eq 'skillbook') {
      my $bookHandler = Game::EvonyTKR::Importer::SkillBook->new();
      $bookHandler->execute();
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
__END__
