package Game::EvonyTKR::Loader::Covenants;
use v5.42.0;
use utf8::all;

use Mooish::Base -standard;
with 'WebFramework::Role::Logger';
with 'Game::EvonyTKR::Role::Common';

use experimental qw(signatures);
use Path::Tiny;
use YAML::PP;
require Game::EvonyTKR::Model::Covenant;

has data_dir => (
  is       => 'ro',
  required => 1,
);

has generals_loader => (
  is       => 'ro',
  required => 1,
);

has covenants => (
  is      => 'rw',
  default => sub { {} },
);

sub load_all {
  my ($self) = @_;

  my $dir = path($self->data_dir);
  unless ($dir->exists && $dir->is_dir) {
    $self->logger->error("Covenants data directory not found: $dir");
    return 0;
  }

  my @yaml_files = $dir->children(qr/\.ya?ml$/);
  $self->logger->info(
    sprintf("Found %d covenant files to load", scalar @yaml_files));

  my $loaded = 0;
  my @failed_files;

  for my $file (@yaml_files) {
    eval {
      my $data = YAML::PP->new(
        schema       => [qw/ + Perl /],
        yaml_version => ['1.2', '1.1'],
      )->load_string($file->slurp_utf8);

      # The covenant name should be in the YAML
      unless ($data->{name}) {
        push @failed_files,
          { file => "$file", error => "No 'name' field in YAML" };
        $self->logger->error("!!! YAML LOAD FAILED !!! $file - no name field");
        return;
      }

      # Get the primary general object
      my $primary_general = $self->generals_loader->get_general($data->{name});
      unless ($primary_general) {
        push @failed_files,
          {
          file  => "$file",
          error => "Primary general '$data->{name}' not found"
          };
        $self->logger->error(
"!!! YAML LOAD FAILED !!! Cannot find primary general '$data->{name}' for covenant in $file"
        );
        return;
      }

      # Create covenant with the primary general object
      my $covenant =
        Game::EvonyTKR::Model::Covenant->from_hash($data, $primary_general);
      if ($covenant) {
        # Debug: Check if primary is set
        $self->logger->debug(sprintf(
          "Created covenant for '%s', primary is %s",
          $data->{name}, $covenant->primary ? 'defined' : 'NOT DEFINED'
        ));

        # Store using normalized key for case-insensitive lookup
        my $normalized_key = $self->normalize($data->{name});
        $self->covenants->{$normalized_key} = $covenant;
        $loaded++;
        $self->logger->debug(
          "Loaded covenant: $data->{name} (key: $normalized_key)");
      }
      else {
        push @failed_files,
          { file => "$file", error => "Failed to create model from hash" };
        $self->logger->error(
          "!!! YAML LOAD FAILED !!! Could not create covenant from $file");
      }
    };
    if ($@) {
      my $error = $@;
      push @failed_files, { file => "$file", error => $error };
      $self->logger->error("!!! YAML LOAD FAILED !!! File: $file");
      $self->logger->error("!!! YAML ERROR: $error");
    }
  }

  # Report summary of failures prominently
  if (@failed_files) {
    $self->logger->error("=" x 60);
    $self->logger->error("!!! COVENANTS LOADER: "
        . scalar(@failed_files)
        . " FILE(S) FAILED TO LOAD !!!");
    for my $failure (@failed_files) {
      $self->logger->error("  - $failure->{file}");
      $self->logger->error("    Error: $failure->{error}");
    }
    $self->logger->error("=" x 60);
    warn sprintf(
      "COVENANTS LOADER: %d file(s) failed to load! Check logs for details.\n",
      scalar(@failed_files));
  }

  $self->logger->info("Loaded $loaded covenants");
  return $loaded;
}

sub get_covenant {
  my ($self, $covenant_name) = @_;
  my $normalized_key = $self->normalize($covenant_name);
  return $self->covenants->{$normalized_key};
}

sub list_covenants {
  my ($self) = @_;
  return [sort keys %{ $self->covenants }];
}

1;

__END__

=head1 NAME

Game::EvonyTKR::Loader::Covenants - Load covenant data from YAML files

=head1 SYNOPSIS

    my $loader = Game::EvonyTKR::Loader::Covenants->new(
        data_dir => 'share/collections/data/covenants'
    );
    $loader->load_all();
    my $covenant = $loader->get_covenant('Artemisia I');

=head1 DESCRIPTION

In-memory loader for covenant data. Loads all covenant YAML files from the
data directory and makes them available via simple accessor methods.

=cut
