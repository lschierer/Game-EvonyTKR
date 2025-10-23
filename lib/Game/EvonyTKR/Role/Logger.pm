use v5.42.0;
use utf8::all;
use File::FindLib 'lib';
require Log::Log4perl;
require Game::EvonyTKR::Log::Config;
use namespace::autoclean;

package Game::EvonyTKR::Role::Logger {
  use Mojo::Base -role, -signatures;
  use Carp;

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
    #print STDERR "effective_class is $effective_class for "
    #  . blessed($self) . "\n";

    my $log = Log::Log4perl->get_logger($effective_class);
    return $log;
  }

  has 'trace' => sub { my $self = shift; $self->logger->debug(@_); };
  has 'debug' => sub { my $self = shift; $self->logger->debug(@_); };
  has 'info'  => sub { my $self = shift; $self->logger->info(@_); };
  has 'warn'  => sub { my $self = shift; $self->logger->warn(@_); };
  has 'error' => sub { my $self = shift; $self->logger->error(@_); };
  has 'fatal' => sub { my $self = shift; $self->logger->logcroak(@_); };
}
1;
__END__
