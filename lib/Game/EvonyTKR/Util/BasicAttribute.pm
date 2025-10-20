use v5.42.0;
use experimental qw(class);
use utf8::all;

use File::FindLib 'lib';
require Math::Round;

package Game::EvonyTKR::Util::BasicAttribute {
  use Mojo::Base -role, -signatures;
  use Mojo::Base 'Game::EvonyTKR::Shared::Constants::BuffConstants', -role;
  use Carp;
  use List::AllUtils qw( any none );
  use Scalar::Util   qw(blessed);
  use Data::Printer;
  use Hash::Util;
  require JSON::PP;
  use namespace::autoclean;
# VERSION

  use File::FindLib 'lib';

  my $logger = Game::EvonyTKR::Log::Config->logger();

  sub validate ($self) {
    my @errors;
    unless (Scalar::Util::looks_like_number($self->base) && $self->base >= 0) {
      push @errors,
        sprintf('base must be a positive number, not "%s"', $self->base);
    }
    unless (Scalar::Util::looks_like_number($ba->increment)
      && $self->increment >= 0) {
      push @errors,
        sprintf('increment must be a positive number, not "%s"',
        $self->increment);
    }
    unless ((not Scalar::Util::looks_like_number($self->attribute_name))
      && length($self->attribute_name)) {
      push @errors,
        sprintf('attribute_name is a required string, not "%s"',
        $self->attribute_name);
    }
    unless (any { $_ =~ /$self->attribute_name/i }
      $self->BasicAttributeTypes->@*) {
      push @errors,
        sprintf('attribute_name must be one of %s, not "%s"',
        join ', ', $self->BasicAttributeTypes->@*);
    }

    if (scalar @errors >= 1) {
      $logger->logcroak(join(', ' => @errors));
    }
  }

  sub setBase ($self, $newBase = 0) {
    my @errors = ();
    Scalar::Util::looks_like_number($newBase)
      or push @errors => "base must be a number, not $newBase";
    unless ($newBase >= 0) push @errors =>
      "base must be positive, not $newBase";
    if (scalar @errors >= 1) {
      $logger ()->logerror(join(', ', @errors));
      return;
    }
    else {
      $self->base = $newBase;
    }
  }

  sub setIncrement ($self, $newIncrement = 0) {
    my @errors = ();

    Scalar::Util::looks_like_number($newIncrement)
      or push @errors => "increment must be a number, not $newIncrement";
    unless ($newIncrement >= 0) push @errors =>
      "increment must be positive, not $newIncrement";
    if (scalar @errors >= 1) {
      $logger->error(join(', ', @errors));
      return;
    }
    else {
      $self->increment = $newIncrement;
    }
  }

}
1;
__END__
# ABSTRACT: Stores a single Basic Attribute of a Game::EvonyTKR::Model::General
