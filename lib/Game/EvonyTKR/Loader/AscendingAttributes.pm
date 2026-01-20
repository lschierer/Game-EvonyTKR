package Game::EvonyTKR::Loader::AscendingAttributes;
use v5.42.0;
use utf8::all;

use Mooish::Base -standard;
with 'WebFramework::Role::Logger';
with 'Game::EvonyTKR::Role::Common';

use experimental qw(signatures);
use Path::Tiny;
use YAML::PP;
require Game::EvonyTKR::Model::AscendingAttributes;

has data_dir => (
  is       => 'ro',
  required => 1,
);

has ascending_attributes => (
  is      => 'rw',
  default => sub { {} },
);

sub load_all {
  my ($self) = @_;

  my $dir = path($self->data_dir);
  unless ($dir->exists && $dir->is_dir) {
    $self->logger->error("Ascending attributes data directory not found: $dir");
    return 0;
  }

  my @yaml_files = $dir->children(qr/\.ya?ml$/);
  $self->logger->info(
    sprintf("Found %d ascending attribute files to load", scalar @yaml_files));

  my $loaded = 0;
  my @failed_files;

  for my $file (@yaml_files) {
    eval {
      my $data = YAML::PP->new(
        schema       => [qw/ + Perl /],
        yaml_version => ['1.2', '1.1'],
      )->load_string($file->slurp_utf8);

      # The file name (without extension) is the general name
      my $general_name = $file->basename(qr/\.ya?ml$/);

      # Add general name to data for from_hash
      $data->{general} = $general_name;

      my $aa = Game::EvonyTKR::Model::AscendingAttributes->from_hash($data);

      # Store using normalized key for case-insensitive lookup
      my $normalized_key = $self->normalize($general_name);
      $self->ascending_attributes->{$normalized_key} = $aa;
      $loaded++;
      $self->logger->debug(
        "Loaded ascending attributes for: $general_name (key: $normalized_key)"
      );
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
    $self->logger->error("!!! ASCENDING ATTRIBUTES LOADER: " . scalar(@failed_files) . " FILE(S) FAILED TO LOAD !!!");
    for my $failure (@failed_files) {
      $self->logger->error("  - $failure->{file}");
      $self->logger->error("    Error: $failure->{error}");
    }
    $self->logger->error("=" x 60);
    warn sprintf("ASCENDING ATTRIBUTES LOADER: %d file(s) failed to load! Check logs for details.\n", scalar(@failed_files));
  }

  $self->logger->info("Loaded $loaded ascending attributes");
  return $loaded;
}

sub get_for_general {
  my ($self, $general_name) = @_;
  my $normalized_key = $self->normalize($general_name);
  return $self->ascending_attributes->{$normalized_key};
}

sub list_generals {
  my ($self) = @_;
  return [sort keys %{ $self->ascending_attributes }];
}

1;

__END__

=head1 NAME

Game::EvonyTKR::Loader::AscendingAttributes - Load ascending attributes data from YAML files

=head1 SYNOPSIS

    my $loader = Game::EvonyTKR::Loader::AscendingAttributes->new(
        data_dir => 'share/collections/data/ascending attributes'
    );
    $loader->load_all();
    my $aa = $loader->get_for_general('Aethelflaed');

=head1 DESCRIPTION

Simplified loader for ascending attributes data. These are not displayed
as standalone pages but rather injected into General detail pages.

=cut
