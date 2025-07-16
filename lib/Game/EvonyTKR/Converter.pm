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
  require Game::EvonyTKR::Converter::AscendingAttributes;
  require Game::EvonyTKR::Converter::Specialty;
  require Game::EvonyTKR::Shared::Parser;
  require Game::EvonyTKR::Logger::Config;
  use Carp;
  use Log::Log4perl qw(:easy);
  our $VERSION = 'v0.01.0';

  sub opt_spec {
    return (
      ["debug|d", 'Enable debug mode'],
      [
        "mode" => hidden => {
          one_of => [
            ['general', 'Convert a General to normalized YAML'],
            [
              'ascending',
              'Convert the Ascending Attributes of a General to normalized YAML'
            ],
            ['skillbook', 'Convert SkillBook Text to normalized YAML'],
            ['specialty', 'Convert Specialty Text to normalized YAML'],
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
    binmode(STDOUT, ":utf8");
    binmode(STDERR, ":utf8");

    my $debug = $opt->{debug} ? 1 : 0;

    Log::Log4perl::Config->utf8(1);
    my $loggerConfig = Game::EvonyTKR::Logger::Config->new('test');
    my $logConfig;
    if ($debug) {
      $logConfig = Path::Tiny->cwd()->child('share/log4perl.development.conf ');
    }
    else {
      $logConfig = Path::Tiny->cwd()->child('share/log4perl.conf ');
    }
    Log::Log4perl::init($logConfig->canonpath());

    my $dd = Path::Tiny::path(File::Share::dist_dir('Game-EvonyTKR'));

    Game::EvonyTKR::Shared::Parser->new()->generate_grammar();

    #my $result = $opt->{blortex} ? blortex() : blort();
    if ($opt->mode eq 'general') {
      my $generalHander = Game::EvonyTKR::Converter::General->new();
      $generalHander->execute();
    }
    elsif ($opt->mode eq 'skillbook') {
      my $bookHandler = Game::EvonyTKR::Converter::SkillBook->new(
        outputDir => $dd->child('/collections/data/skill books/'),
        debug     => $debug,
      );
      $bookHandler->execute();
    }
    elsif ($opt->mode eq 'ascending') {
      my $handler = Game::EvonyTKR::Converter::AscendingAttributes->new(
        outputDir => $dd->child('/collections/data/ascending attributes/'),
        debug     => $debug,
      );
      $handler->execute();
    }
    elsif ($opt->mode eq 'specialty') {
      my $handler = Game::EvonyTKR::Converter::Specialty->new(
        outputDir => $dd->child('/collections/data/specialties/'),
        debug     => $debug,
      );
      $handler->execute();
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
