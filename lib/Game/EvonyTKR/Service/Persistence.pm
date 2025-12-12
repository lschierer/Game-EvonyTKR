package Game::EvonyTKR::Service::Persistence;
use v5.42.0;
use utf8::all;
use Mojo::Base -base, -signatures;

# Mode-gated persistence factory
has 'mode' => sub { $ENV{MOJO_MODE} || 'development' };
has 'config';  # Optional config hash from NotYAMLConfig

has 'backend' => sub ($self) {
  my $config = $self->config || {};
  my $persistence_config = $config->{persistence} || {};

  # Debug logging
  require Data::Dumper;
  warn sprintf("[Persistence] Mode: %s\n", $self->mode);
  warn sprintf("[Persistence] Config keys: %s\n", join(', ', keys %$config));
  warn sprintf("[Persistence] AWS config: %s\n", Data::Dumper::Dumper($config->{aws})) if $config->{aws};

  if ($self->mode eq 'development') {
    require Game::EvonyTKR::Service::SQLitePersistence;
    return Game::EvonyTKR::Service::SQLitePersistence->new(config => $persistence_config);
  } else {
    require Game::EvonyTKR::Service::DynamoDBPersistence;
    my $aws_config = $config->{aws} || {};
    return Game::EvonyTKR::Service::DynamoDBPersistence->new(
      config => $persistence_config,
      aws_config => $aws_config,
    );
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
