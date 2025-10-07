use v5.42.0;
use experimental qw(class);
use utf8::all;
use namespace::autoclean;

class Game::EvonyTKR::Model::Logger {
  #PODNAME: Game::EvonyTKR::Model::Logger
  use Carp;
  use Scalar::Util qw(blessed);
  use JSON::PP     ();
  use Env          qw(DEV_MODE PERL_ENV MOJO_MODE);
  use Game::EvonyTKR::Shared::Logger;
  our $VERSION = 'v0.31.0';

  use overload
    '""'       => \&to_string,              # used for concat too
    'bool'     => sub { $_[0]->_isTrue },
    'fallback' => 0;                        # allow Perl defaults for the rest

  field $logger : reader;                   # readonly accessor -> $obj->logger

  ADJUST {
    # decide dev-ness; prefer DEV_MODE, else PERL_ENV/MOJO_MODE
    $logger = Game::EvonyTKR::Shared::Logger::get_logger(__CLASS__);
    $logger->DEBUG(
      sprintf('logging set to "%s" for "%s"', $logger->level(), __CLASS__));
  }

  method trace { $self->logger->DEBUG(@_) }
  method debug { $self->logger->DEBUG(@_) }
  method info  { $self->logger->INFO(@_) }
  method warn  { $self->logger->WARN(@_) }
  method error { $self->logger->ERR(@_) }
  method fatal { $self->logger->ERR(@_) }

  # Normalize $level to a Log::Log4perl constant if a string is given
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

  method dev_guard ($msg, $level = 'WARN') {
    if ($self->_debug) { $self->logger->ERR($msg), croak($msg) }
    else               { $self->logger->ERR($msg) }
    return;
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
