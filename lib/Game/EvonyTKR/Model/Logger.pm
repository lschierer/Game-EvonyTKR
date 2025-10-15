use v5.42.0;
use experimental qw(class);
use utf8::all;
use namespace::autoclean;
require Log::Log4perl;

class Game::EvonyTKR::Model::Logger {
  #PODNAME: Game::EvonyTKR::Model::Logger
  use Carp;
  use Scalar::Util qw(blessed);
  use JSON::PP     ();
  use Env          qw(DEV_MODE PERL_ENV MOJO_MODE);
  our $VERSION = 'v0.31.0';

  use overload
    '""'       => \&to_string,              # used for concat too
    'bool'     => sub { $_[0]->_isTrue },
    'fallback' => 0;                        # allow Perl defaults for the rest

  field $logger : reader;                   # readonly accessor -> $obj->logger

  ADJUST {
    # decide dev-ness; prefer DEV_MODE, else PERL_ENV/MOJO_MODE
    $logger = Log::Log4perl->get_logger(__CLASS__);
  }

  method trace { $self->logger->debug(@_) }
  method debug { $self->logger->debug(@_) }
  method info  { $self->logger->info(@_) }
  method warn  { $self->logger->warn(@_) }
  method error { $self->logger->error(@_) }
  method fatal { $self->logger->error(@_) }

  # Normalize $level to a constant if a string is given
  method _norm_level ($level) {
    return $level if defined $level && $level =~ /^\d+$/;   # already a constant
    my %by_name = (
      trace => 'DEBUG',
      debug => 'DEBUG',
      info  => 'INFO',
      warn  => 'WARN',
      error => 'ERR',
      fatal => 'ERR',
    );
    return $by_name{ lc($level // '') } // 'WARN';
  }

  # ----- Optional helpers for DDP / JSON -----

  method to_hash {
    return {};
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
