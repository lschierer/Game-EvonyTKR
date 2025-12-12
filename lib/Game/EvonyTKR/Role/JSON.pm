package Game::EvonyTKR::Role::JSON;
use v5.42.0;
use utf8::all;
use Mojo::Base -role, -signatures;
use Carp;
require JSON::PP;

our $json_instance;

has JSON => sub {
  $json_instance //=
    JSON::PP->new()->utf8(1)->allow_blessed(1)->convert_blessed(1);
  return $json_instance;
};

sub encode ($self, $data) {
  return $self->JSON->encode($data);
}

sub pretty_encode ($self, $data) {
  # Create a temporary pretty instance to avoid affecting the main one
  return $self->JSON->pretty(1)->encode($data)->pretty(0);
}

sub decode ($self, $data) {
  return $self->JSON->decode($data);
}

1;
__END__

=head1 NAME

Game::EvonyTKR::Role::JSON - Centralized JSON configuration

=head1 DESCRIPTION

Provides consistent JSON encoding/decoding across the application with
proper UTF-8 and blessed object handling configured.

=cut
