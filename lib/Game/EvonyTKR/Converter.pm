use v5.42.0;
use experimental qw(class);
use utf8::all;
require Data::Printer;
require App::Cmd::Simple;
require File::Share;
require Path::Tiny;
require HTML::TreeBuilder;
require LWP::Protocol::https;
require LWP::UserAgent;
use namespace::clean;

package Game::EvonyTKR::Converter {
  use parent qw(App::Cmd::Simple);
  use File::FindLib 'lib';
  require Game::EvonyTKR::Converter::General;
  require Game::EvonyTKR::Converter::SkillBook;
  require Game::EvonyTKR::Converter::AscendingAttributes;
  require Game::EvonyTKR::Converter::Specialty;
  require Game::EvonyTKR::Converter::Covenant;
  require Game::EvonyTKR::Shared::Parser;
  require Game::EvonyTKR::Logger::Config;
  use Carp;
  use Sereal::Encoder;
  use Sereal::Decoder;
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
              'all',
              'Convert everything to normalized YAML from a provided URL'
            ],
          ],
          required => 1,
        },
      ],
      ['url=s', "the url from which to fetch data, only used with --all"],
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
    my $loggerConfig = Game::EvonyTKR::Logger::Config->new('Game::EvonyTKR');
    my $logConfig;
    if ($debug) {
      $logConfig = Path::Tiny->cwd()->child('share/log4perl.development.conf ');
    }
    else {
      $logConfig = Path::Tiny->cwd()->child('share/log4perl.production.conf ');
    }
    Log::Log4perl::init($logConfig->canonpath());
    my $logger = Log::Log4perl->get_logger(__PACKAGE__);

    my $dd = Path::Tiny::path(File::Share::dist_dir('Game-EvonyTKR'));

    Game::EvonyTKR::Shared::Parser->new()->generate_grammar();

    #my $result = $opt->{blortex} ? blortex() : blort();
    if ($opt->mode eq 'general') {
      my $generalHander = Game::EvonyTKR::Converter::General->new();
      $generalHander->execute();
    }
    elsif ($opt->mode eq 'all') {
      if (length($opt->url) == 0) {
        $logger->logcroak("A URL is required with --all");
        exit -1;
      }
      $self->doAll($logger, $opt->url, $opt->debug);
    }
    else {
      if (length($opt->mode) > 0) {
        $logger->logcroak(sprintf('Mode %s is not supported.', $opt->mode));
      }
      else {
        $logger->logcroak("missing mode option");
      }
    }

    return 0;
  }

  sub doAll ($self, $logger, $url, $debug) {
    $logger->debug("::Converter->doAll started.");
    my $dd       = Path::Tiny::path(File::Share::dist_dir('Game-EvonyTKR'));
    my $ua       = LWP::UserAgent->new;
    my $response = $ua->get($url);
    unless ($response->is_success) {
      $logger->logcroak("Failed to fetch $url: ", $response->status_line);
    }
    my $tree = HTML::TreeBuilder->new;
    $tree->parse_content($response->decoded_content);
    $tree->eof();
    my $encoder = Sereal::Encoder->new({ freeze_callbacks => 1 });
    my $decoder = Sereal::Decoder->new;

    $logger->debug("tree is " . Data::Printer::np($tree));
    $tree->look_down(_tag => 'script')->delete()
      for $tree->look_down(_tag => 'script');
    my $serialized = $encoder->encode($tree);
    my $handler;
    $handler = Game::EvonyTKR::Converter::SkillBook->new(
      tree      => $decoder->decode($serialized),
      outputDir => $dd->child('/collections/data/skill books/'),
      debug     => $debug,
    );
    $handler->execute();
    $handler = Game::EvonyTKR::Converter::AscendingAttributes->new(
      tree      => $decoder->decode($serialized),
      outputDir => $dd->child('/collections/data/ascending attributes/'),
      debug     => $debug,
    );
    $handler->execute();
    $handler = Game::EvonyTKR::Converter::Specialty->new(
      tree      => $decoder->decode($serialized),
      outputDir => $dd->child('/collections/data/specialties/'),
      debug     => $debug,
    );
    $handler->execute();
    $handler = Game::EvonyTKR::Converter::Covenant->new(
      tree      => $decoder->decode($serialized),
      outputDir => $dd->child('/collections/data/covenants/'),
      debug     => $debug,
    );
    $handler->execute();
    $handler = Game::EvonyTKR::Converter::General->new(
      tree      => $decoder->decode($serialized),
      outputDir => $dd->child('/collections/data/generals/'),
      debug     => $debug,
    );
    $handler->execute();
  }
}
1;
__END__
