use v5.42.0;
use utf8::all;
use File::FindLib 'lib';
require Log::Log4perl;
require Game::EvonyTKR::Log::Config;
use namespace::autoclean;

package Game::EvonyTKR::Role::Logger {
  use Mojo::Base -role, -signatures;

  sub get_effective_caller {
    my $depth = 1;
    while (my $caller = caller($depth++)) {
      # Ignore known non-class contexts (e.g., eval)
      next if $caller =~ /^(eval|main)$/;

      # Return first valid class found
      return $caller if $caller->isa('Game::EvonyTKR::Model::Logger');
    }
    # Fallback to a default strategy
    return blessed(shift) || ref(shift);
  }

  sub logger ($self) {
    my $effective_class = get_effective_caller($self);
    print STDERR "effective_class is $effective_class for "
      . blessed($self) . "\n";

    my $log = Log::Log4perl->get_logger($effective_class);
    return $log;
  }

  sub trace { my $self = shift; $self->logger->debug(@_); }
  sub debug { my $self = shift; $self->logger->debug(@_); }
  sub info  { my $self = shift; $self->logger->info(@_); }
  sub warn  { my $self = shift; $self->logger->warn(@_); }
  sub error { my $self = shift; $self->logger->error(@_); }
  sub fatal { my $self = shift; $self->logger->error(@_); }
}
1;
__END__
