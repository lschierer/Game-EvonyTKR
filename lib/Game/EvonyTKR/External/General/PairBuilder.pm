# DEPRECATED: This file has been refactored into separate components:
# - Game::EvonyTKR::External::General::Pair::Workflow (workflow orchestration)
# - Game::EvonyTKR::External::General::Pair::Builder (business logic)
#
# This file is kept for backward compatibility and will redirect to the new architecture.

use v5.40;
use experimental qw(class);
use utf8::all;
require Game::EvonyTKR::External::General::Pair::Builder;

# Compatibility wrapper - delegates to new Builder class
class Game::EvonyTKR::External::General::PairBuilder {
  use Carp;

  field $builder;

  ADJUST {
    $builder = Game::EvonyTKR::External::General::Pair::Builder->new();
    carp "Game::EvonyTKR::External::General::PairBuilder is deprecated. "
      . "Use Game::EvonyTKR::External::General::Pair::Builder instead.";
  }

  # Delegate all method calls to the new builder
  method AUTOLOAD {
    my $method = our $AUTOLOAD;
    $method =~ s/.*:://;
    return if $method eq 'DESTROY';

    return $builder->$method(@_);
  }
}

1;
