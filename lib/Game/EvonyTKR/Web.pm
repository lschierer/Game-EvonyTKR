use v5.40.0;
use utf8::all;
use experimental qw(class defer);
use Carp;

use FindBin;
use lib "$FindBin::Bin/../../lib";

use Log::Log4perl;


package Game::EvonyTKR::Web {
  use Util::Any -all;
  use namespace::clean;
  use parent qw(App::Cmd::Simple);
  use Data::Printer;
  
  sub opt_spec {
    return (
      [ "env|E=s",  "set the plack Environment" ],,
    );
  }

  sub validate_args {
    my ($self, $opt, $args) = @_;

    # no args allowed but options
    $self->usage_error("No args allowed") if @$args;
  }

  sub execute {
    my ($self, $opt, $args) = @_;
    binmode(STDOUT, ":encoding(UTF-8)"); # apparently not the same thing as "use utf8;"
    binmode(STDIN, ":encoding(UTF-8)"); # apparently not the same thing as "use utf8;"
    
    my $logfile = _logInit();
    my $logger = Log::Log4perl::get_logger('Game::EvonyTKR');
    $logger->debug("Logging initialized with logfile $logfile");

    my @DancerOpts;
    if($opt->{env}){
      my $env = $opt->{'env'};
      push @DancerOpts, qw{ -E };
      push @DancerOpts, $env;
      if($env =~ /development/i ) {
        push @DancerOpts, qw{ -L Shotgun };
      }
      $logger->debug("options are " . np @DancerOpts);
    }

    # Copied from the plackup script, which I am not using because I need to initialize log4perl myself.
    my $runner = Plack::Runner->new;
    $runner->parse_options(@DancerOpts);

    $logger->info('Starting Game::EvonyTKR');
    $runner->run(Game::EvonyTKR::Web::Root->to_app());

  }

  sub _logInit() {
    my $home   = File::HomeDir->my_home;
    my $logDir = File::Spec->catdir($home, 'var/log/Perl/dist/Game-Evony/');
    if (!-r -w -x -o -d $logDir) {
      make_path($logDir, "0770");
    }
    my $logFile = File::Spec->catfile($logDir, 'dancer2.log');
    if (!-e $logFile) {
      File::Touch::touch($logFile);
      chmod(0600, $logFile);
    }

    my %conf = (
      "log4perl.rootLogger" => "ALL, logFile",

      "log4perl.appender.logFile"          => "Log::Log4perl::Appender::File",
      "log4perl.appender.logFile.utf8"     => 1,
      "log4perl.appender.logFile.filename" => $logFile,
      "log4perl.appender.Logfile.DatePattern" => "yyyy-MM-dd",
      "log4perl.appender.Logfile.TZ"          => "UTC",
      "log4perl.appender.logFile.mode"        => "append",
      "log4perl.appender.logFile.layout"      =>
        "Log::Log4perl::Layout::PatternLayout",
      "log4perl.appender.logFile.layout.ConversionPattern" =>
        "[%p] %d (%C line %L) %m%n",
    );
    # ... passed as a reference to init()
    Log::Log4perl::init(\%conf);
    return $logFile;
  }

}

true;

package Game::EvonyTKR::Web::Root {
  use namespace::clean;
  use FindBin;
  use lib "$FindBin::Bin/../../lib";
  use Dancer2 appname => 'Game::EvonyTKR';
  use Game::EvonyTKR::Web::General;

  set engines => {
    serializer => {
        JSON => {
          allow_blessed   => 1,
          allow_nonref    => 1,
          allow_tags      => 1,
          convert_blessed => 1,
          max_depth       => 5,
          pretty          => 0,
          utf8            => 1,
        }
    }
  };

  set serializer      => 'JSON';
  set content_type    => 'application/json';

  get '/' => sub {
    status_ok('success');
  };

}
true;

__END__