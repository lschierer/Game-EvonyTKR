package Game::EvonyTKR::Loader::Specialties;
use v5.42.0;
use utf8::all;
use Mooish::Base -standard;
with 'WebFramework::Role::Logger';
use experimental qw(signatures);
use Path::Tiny;
use YAML::PP;
require Game::EvonyTKR::Model::Specialty;

has data_dir => (
  is       => 'ro',
  required => 1,
);

has specialties => (
  is      => 'rw',
  default => sub { {} },
);

sub load_all {
  my ($self) = @_;

  my $dir = path($self->data_dir);
  unless ($dir->exists && $dir->is_dir) {
    $self->logger->error("Specialty data directory not found: $dir");
    return 0;
  }

  my @yaml_files = $dir->children(qr/\.ya?ml$/);
  $self->logger->info(
    sprintf("Found %d specialty files to load", scalar @yaml_files));

  my $loaded = 0;
  my @failed_files;

  for my $file (@yaml_files) {
    eval {
      my $data = YAML::PP->new(
        schema       => [qw/ + Perl /],
        yaml_version => ['1.2', '1.1'],
      )->load_string($file->slurp_utf8);

      unless ($data->{name}) {
        push @failed_files, { file => "$file", error => "No 'name' field in YAML" };
        $self->logger->error("!!! YAML LOAD FAILED !!! $file - no name field");
        return;
      }

      my $specialty = Game::EvonyTKR::Model::Specialty->from_hash($data);
      $self->specialties->{ $specialty->name } = $specialty;
      $loaded++;
      $self->logger->debug("Loaded specialty: " . $specialty->name);
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
    $self->logger->error("!!! SPECIALTIES LOADER: " . scalar(@failed_files) . " FILE(S) FAILED TO LOAD !!!");
    for my $failure (@failed_files) {
      $self->logger->error("  - $failure->{file}");
      $self->logger->error("    Error: $failure->{error}");
    }
    $self->logger->error("=" x 60);
    warn sprintf("SPECIALTIES LOADER: %d file(s) failed to load! Check logs for details.\n", scalar(@failed_files));
  }

  $self->logger->info("Loaded $loaded specialties");
  return $loaded;
}

sub get_specialty {
  my ($self, $name) = @_;
  return $self->specialties->{$name};
}

sub list_specialties {
  my ($self) = @_;
  return [sort keys %{ $self->specialties }];
}

1;

__END__

=head1 NAME

Game::EvonyTKR::Loader::Specialties - Load specialty data from YAML files

=head1 SYNOPSIS

    my $loader = Game::EvonyTKR::Loader::Specialties->new(
        data_dir => 'share/collections/data/specialties'
    );
    $loader->load_all();
    my $specialty = $loader->get_specialty('Ambush');

=head1 DESCRIPTION

Simplified loader for specialty data. Loads all YAML files on startup
and keeps them in memory. For the PoC, this replaces the complex
Minion job queue + Postgres persistence.

=cut
