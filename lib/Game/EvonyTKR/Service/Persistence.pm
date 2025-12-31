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

  # Determine backend: explicit config > mode-based fallback
  my $backend_type = $persistence_config->{backend};

  # Fall back to mode-based selection if no explicit backend configured
  unless ($backend_type) {
    $backend_type = 'postgresql';    # Default to PostgreSQL for all modes
    warn sprintf(
      "[Persistence] No explicit backend configured, using default: %s\n",
      $backend_type);
  }

  if ($backend_type eq 'postgresql') {
    require Game::EvonyTKR::Service::PostgreSQLPersistence;
    return Game::EvonyTKR::Service::PostgreSQLPersistence->new(
      config => $persistence_config);
  }
  else {
    croak(sprintf(
      "Unknown persistence backend type: %s "
        . "(expected 'postgresql', 'sqlite', or 'dynamodb')\n",
      $backend_type
    ));
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
1. Explicit config: persistence.backend = 'postgresql' | 'sqlite' | 'dynamodb'
2. Default fallback: postgresql (for all modes)

B<Supported Backends:>
- postgresql: PostgreSQL database (recommended, default)
- sqlite: SQLite file database (legacy, development only)
- dynamodb: AWS DynamoDB (legacy, being phased out)

B<Configuration Example:>
  persistence:
    backend: postgresql
    postgresql_dsn: 'postgresql:///evonytkr_app_data'

This allows:
- Local development: No config → PostgreSQL (local instance)
- Dev stack (EC2): No config → PostgreSQL (local instance)
- Prod stack (EC2): No config → PostgreSQL (local instance)

B<Legacy Configuration:>
  # SQLite (development only)
  persistence:
    backend: sqlite
    sqlite_db_path: './evonytkr.db'

  # DynamoDB (being phased out)
  persistence:
    backend: dynamodb
    dynamodb_table: evonytkr-dev-data
  aws:
    region: us-east-2

=cut
