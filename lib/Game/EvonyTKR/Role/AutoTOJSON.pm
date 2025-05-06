package Game::EvonyTKR::Role::AutoTOJSON;
use v5.38;
use strict;
use warnings;

sub import {
  my ($class) = @_;
  my $caller = caller;

  no strict 'refs';
  *{"${caller}::TO_JSON"} = sub {
    my ($self) = @_;
    $self->can('toHashRef')
      ? $self->toHashRef
      : die "Class $caller must implement toHashRef to use AutoTOJSON";
  };
}

1;
