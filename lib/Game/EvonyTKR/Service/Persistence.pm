package Game::EvonyTKR::Service::Persistence;
use v5.42.0;
use utf8::all;
use Mojo::Base -base, -signatures;

# Mode-gated persistence factory
has 'mode' => sub { $ENV{MOJO_MODE} || 'development' };
has 'config';    # Optional config hash from NotYAMLConfig

has 'backend' => sub ($self) {
  my $config             = $self->config          || {};
  my $persistence_config = $config->{persistence} || {};

  # Debug logging
  require Data::Dumper;
  warn sprintf(
    "[Persistence] MOJO_MODE=%s, mode attribute=%s\n",
    $ENV{MOJO_MODE} // 'unset',
    $self->mode
  );
  warn sprintf("[Persistence] Config keys: %s\n", join(', ', keys %$config));
  warn sprintf("[Persistence] Persistence config: %s\n",
    Data::Dumper::Dumper($persistence_config));
  warn sprintf("[Persistence] AWS config: %s\n",
    Data::Dumper::Dumper($config->{aws}))
    if $config->{aws};

  # Determine backend: explicit config > mode-based fallback
  my $backend_type = $persistence_config->{backend};

  # Fall back to mode-based selection if no explicit backend configured
  unless ($backend_type) {
    $backend_type = ($self->mode eq 'development') ? 'sqlite' : 'dynamodb';
    warn sprintf(
"[Persistence] No explicit backend configured, using mode-based default: %s\n",
      $backend_type);
  }
  else {
    warn sprintf("[Persistence] Using explicitly configured backend: %s\n",
      $backend_type);
  }

  if ($backend_type eq 'sqlite') {
    require Game::EvonyTKR::Service::SQLitePersistence;
    return Game::EvonyTKR::Service::SQLitePersistence->new(
      config => $persistence_config);
  }
  elsif ($backend_type eq 'dynamodb') {
    require Game::EvonyTKR::Service::DynamoDBPersistence;
    my $aws_config = $config->{aws} || {};
    return Game::EvonyTKR::Service::DynamoDBPersistence->new(
      config     => $persistence_config,
      aws_config => $aws_config,
    );
  }
  else {
    die sprintf(
"Unknown persistence backend type: %s (expected 'sqlite' or 'dynamodb')\n",
      $backend_type);
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

Game::EvonyTKR::Service::Persistence - Configurable persistence factory

=head1 DESCRIPTION

Provides different persistence backends based on configuration:

B<Backend Selection Priority:>
1. Explicit config: persistence.backend = 'sqlite' | 'dynamodb' (CDK-generated)
2. Mode fallback: development → sqlite, production → dynamodb

B<Configuration Example:>
  persistence:
    backend: dynamodb
    dynamodb_table: evonytkr-dev-data
  aws:
    region: us-east-2

This allows:
- Local development: No config, mode=development → SQLite
- Dev stack (EC2): Config backend=dynamodb, mode=development → DynamoDB
- Prod stack (EC2): Config backend=dynamodb, mode=production → DynamoDB

=cut
