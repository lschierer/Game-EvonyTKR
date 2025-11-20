use v5.42.0;
use utf8::all;

package Game::EvonyTKR::Logger::MojoLog4Perl;
use Mojo::Base 'Mojo::Log';
use Log::Log4perl ();

# Category can be injected; defaults to 'Mojo'
has l4p => sub { Log::Log4perl->get_logger('Mojo') };

sub debug { shift->_fwd(debug => @_) }
sub info  { shift->_fwd(info  => @_) }
sub warn  { shift->_fwd(warn  => @_) }
sub error { shift->_fwd(error => @_) }
sub fatal { shift->_fwd(fatal => @_) }

# (optional) Mojolicious also calls ->trace in some versions
sub trace { shift->_fwd(trace => @_) }    # map to debug if you want

sub _fwd {
  my ($self, $level, @lines) = @_;
  my $msg = join('', map { ref($_) ? "$_" : $_ } @lines);
  my $l4p = $self->l4p;
  $l4p->$level($msg);
  return $self;
}

1;
__END__
