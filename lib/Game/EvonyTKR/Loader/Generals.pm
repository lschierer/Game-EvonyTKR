package Game::EvonyTKR::Loader::Generals;
use v5.42.0;
use utf8::all;
use Mooish::Base -standard;
with 'WebFramework::Role::Logger';
with 'Game::EvonyTKR::Role::Common';
use experimental qw(signatures);
use Path::Tiny;
use YAML::PP;
require Game::EvonyTKR::Model::General;

has data_dir => (
  is       => 'ro',
  required => 1,
);

has generals => (
  is      => 'rw',
  default => sub { {} },
);

sub load_all {
  my ($self) = @_;

  my $dir = path($self->data_dir);
  unless ($dir->exists && $dir->is_dir) {
    $self->logger->error("Generals data directory not found: $dir");
    return 0;
  }

  my @yaml_files = $dir->children(qr/\.ya?ml$/);
  $self->logger->info(
    sprintf("Found %d general files to load", scalar @yaml_files));

  my $loaded = 0;
  for my $file (@yaml_files) {
    eval {
      my $data = YAML::PP->new(
        schema       => [qw/ + Perl /],
        yaml_version => ['1.2', '1.1'],
      )->load_string($file->slurp_utf8);

      # The file name (without extension) is the general name
      my $general_name = $file->basename(qr/\.ya?ml$/);

  # Only set name if not already in YAML (prefer YAML name for correct encoding)
      $data->{name} = $general_name unless exists $data->{name};

      my $general = Game::EvonyTKR::Model::General->from_hash($data);
      if ($general) {
        # Store using normalized key for case-insensitive lookup
        my $normalized_key = $self->normalize($general_name);
        $self->generals->{$normalized_key} = $general;
        $loaded++;
        $self->logger->debug(
          "Loaded general: $general_name (key: $normalized_key)");
      }
      else {
        $self->logger->error("Failed to create general from $file");
      }
    };
    if ($@) {
      $self->logger->error("Failed to load $file: $@");
    }
  }

  $self->logger->info("Loaded $loaded generals");
  return $loaded;
}

sub get_general {
  my ($self, $general_name) = @_;
  my $normalized_key = $self->normalize($general_name);
  return $self->generals->{$normalized_key};
}

sub list_generals {
  my ($self) = @_;
  return [sort keys %{ $self->generals }];
}

1;

__END__

=head1 NAME

Game::EvonyTKR::Loader::Generals - Load generals data from YAML files

=head1 SYNOPSIS

    my $loader = Game::EvonyTKR::Loader::Generals->new(
        data_dir => 'share/collections/data/generals'
    );
    $loader->load_all();
    my $general = $loader->get_general('Aethelflaed');

=head1 DESCRIPTION

In-memory loader for general data. Loads all general YAML files from the
data directory and makes them available via simple accessor methods.

=cut
