use v5.42.0;
use experimental qw(class);
use utf8::all;
require Data::Printer;
require App::Cmd::Simple;
require File::Share;
require Path::Tiny;
use namespace::clean;

package Game::EvonyTKR::Converter {
  use parent qw(App::Cmd::Simple);
  use File::FindLib 'lib';
  require Game::EvonyTKR::Converter::General;
  require Game::EvonyTKR::Converter::SkillBook;
  use Carp;
  use Log::Log4perl qw(:easy);
  our $VERSION = 'v0.01.0';

  sub opt_spec {
    return (
      ["debug|d", 'Enable debug mode'],
      [
        "mode" => hidden => {
          one_of => [
            ['general',   'Convert a General to normalized YAML'],
            ['skillbook', 'Convert SkillBook Text to normalized YAML'],
          ],
          required => 1,
        },
      ]
    );
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
    my $dd = Path::Tiny::path(File::Share::dist_dir('Game-EvonyTKR'));
    my $debug = $opt->{debug} ? 1 : 0;

    #my $result = $opt->{blortex} ? blortex() : blort();
    if ($opt->mode eq 'general') {
      my $generalHander = Game::EvonyTKR::Converter::General->new();
      $generalHander->execute();
    }
    if ($opt->mode eq 'skillbook') {
      my $bookHandler = Game::EvonyTKR::Converter::SkillBook->new(
        outputDir => $dd->child('/collections/data/skill books/'),
        debug     => $debug,
        );
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
