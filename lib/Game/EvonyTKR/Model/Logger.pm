use v5.42.0;
use experimental qw(class);
use utf8::all;
use namespace::autoclean;

class Game::EvonyTKR::Model::Logger {
  #PODNAME: Game::EvonyTKR::Model::Logger
  use Carp;
  use Log::Log4perl qw(:levels);    # <-- do NOT import get_logger
  use Scalar::Util  qw(blessed);
  use JSON::PP      ();
  use Env           qw(DEV_MODE PERL_ENV MOJO_MODE);
  our $VERSION = 'v0.31.0';

  use overload
    '""'       => \&to_string,              # used for concat too
    'bool'     => sub { $_[0]->_isTrue },
    'fallback' => 0;                        # allow Perl defaults for the rest

  field $logger : reader;                   # readonly accessor -> $obj->logger
  field $category : reader : param = __CLASS__;

  field $_debug : reader = 0;

  ADJUST {
    # decide dev-ness; prefer DEV_MODE, else PERL_ENV/MOJO_MODE
    my $v = $DEV_MODE // $PERL_ENV // $MOJO_MODE // '';
    $_debug = ($v && $v !~ /^(?:0|false|prod(?:uction)?)$/i) ? 1 : 0;

    # initialize on construction (merged from a pre-existing ADJUST block)
    $self->get_logger;
    $logger->debug('logging module debug set to ' . $_debug ? 'true' : 'false');
  }

  method trace { $self->logger->trace(@_) }
  method debug { $self->logger->debug(@_) }
  method info  { $self->logger->info(@_) }
  method warn  { $self->logger->warn(@_) }
  method error { $self->logger->error(@_) }
  method fatal { $self->logger->fatal(@_) }

  # Normalize $level to a Log::Log4perl constant if a string is given
  method _norm_level ($level) {
    return $level if defined $level && $level =~ /^\d+$/;   # already a constant
    my %by_name = (
      trace => $TRACE,
      debug => $DEBUG,
      info  => $INFO,
      warn  => $WARN,
      error => $ERROR,
      fatal => $FATAL,
    );
    return $by_name{ lc($level // '') } // $WARN;
  }

  method dev_guard ($msg, $level = $WARN) {
    if ($self->_debug) { $self->logger->logcroak($msg) }
    else               { $self->logger->log($self->_norm_level($level), $msg) }
    return;
  }

  method get_logger($cat = $category) {
    # If you use a Log4perl config, make sure it's already initialized elsewhere
    #Log::Log4perl::Config->utf8(1);
    # only if you really need this, and you've loaded that module
    $logger = Log::Log4perl->get_logger($cat);    # <-- set the field directly
    return $logger;
  }

  # ----- Optional helpers for DDP / JSON -----

  method to_hash {
    return { category => $category };
  }

  method TO_JSON {
    return $self->to_hash;
  }

  method to_string {
    return JSON::PP->new->utf8->allow_blessed->convert_blessed->encode($self);
  }

  method Freezer {
    return $self->to_hash;
  }

  method _isTrue {
    return
         defined($self)
      && ref($self)
      && blessed($self)
      && blessed($self) eq __CLASS__;
  }
}
1;
