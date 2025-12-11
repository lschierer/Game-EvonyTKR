package Game::EvonyTKR::Service::Persistence;
use v5.42.0;
use utf8::all;
use Mojo::Base -base, -signatures;

# Mode-gated persistence factory
has 'mode' => sub { $ENV{MOJO_MODE} || 'development' };

has 'backend' => sub ($self) {
  if ($self->mode eq 'development') {
    require Game::EvonyTKR::Service::SQLitePersistence;
    return Game::EvonyTKR::Service::SQLitePersistence->new;
  } else {
    require Game::EvonyTKR::Service::DynamoDBPersistence;
    return Game::EvonyTKR::Service::DynamoDBPersistence->new;
  }
};

# Delegate all methods to the appropriate backend
sub AUTOLOAD ($self, @args) {
  our $AUTOLOAD;
  my $method = $AUTOLOAD;
  $method =~ s/.*:://;
  return if $method eq 'DESTROY';
  
  return $self->backend->$method(@args);
}

1;
__END__

=head1 NAME

Game::EvonyTKR::Service::Persistence - Mode-gated persistence factory

=head1 DESCRIPTION

Provides different persistence backends based on application mode:
- development: SQLite (local, efficient)
- production/other: DynamoDB (scalable, cost-effective)

=cut
