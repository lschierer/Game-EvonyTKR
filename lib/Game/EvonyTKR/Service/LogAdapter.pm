    package Game::EvonyTKR::Service::LogAdapter;
    use Moo;
    extends 'Mojo::Log';
    use Log::Log4perl;

    my $l4p;

    BEGIN {
      $l4p = Game::EvonyTKR::Role::Logging::get_logger(__PACKAGE__);
    }

    sub new {
      my ($class, %args) = @_;
      my $self = $class->SUPER::new(%args);
      $self->{_log4perl_logger} =
        Game::EvonyTKR::Role::Logging::get_logger($args{category}
          || 'Mojolicious');
      return $self;
    }

    sub debug {
      my ($self, @args) = @_;
      my $mesg = scalar(@args) ? join ' ', @args : '';
      $self->{_log4perl_logger}->debug($mesg);
    }

    sub info {
      my ($self, @args) = @_;
      my $mesg = scalar(@args) ? join ' ', @args : '';
      $self->{_log4perl_logger}->info($mesg);
    }

    sub warn {
      my ($self, @args) = @_;
      my $mesg = scalar(@args) ? join ' ', @args : '';
      $self->{_log4perl_logger}->warn($mesg);
    }

    sub error {
      my ($self, @args) = @_;
      my $mesg = scalar(@args) ? join ' ', @args : '';
      $self->{_log4perl_logger}->error($mesg);
    }

    sub fatal {
      my ($self, @args) = @_;
      my $mesg = scalar(@args) ? join ' ', @args : '';
      $self->{_log4perl_logger}->fatal($mesg);
    }

    # Implement your custom format method here
    sub format {
      my ($self, @args) = @_;
      # Example: Join arguments with a space
      return join ' ', @args;
    }

    1;
