use v5.40.0;
use utf8::all;

package Game::EvonyTKR::Logger::UTF8FileRotate;
use parent 'Log::Dispatch::FileRotate';
use IO::Handle;

sub _open_file {
  my $self = shift;
  $self->SUPER::_open_file(@_);
  binmode $self->{fh}, ':utf8';
}
1;
