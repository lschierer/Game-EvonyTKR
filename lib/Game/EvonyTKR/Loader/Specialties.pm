package Game::EvonyTKR::Loader::Specialties;
use v5.42.0;
use utf8::all;
use Moo;
use experimental qw(signatures);
use Path::Tiny;
use YAML::PP;
use Log::Log4perl qw(get_logger);
require Game::EvonyTKR::Model::Specialty;

has logger => (
  is => 'ro',
  lazy => 1,
  default => sub { get_logger(__PACKAGE__) }
);

has data_dir => (
  is => 'ro',
  required => 1,
);

has specialties => (
  is => 'rw',
  default => sub { {} },
);

sub load_all {
  my ($self) = @_;

  my $dir = path($self->data_dir);
  unless ($dir->exists && $dir->is_dir) {
    $self->logger->error("Specialty data directory not found: $dir");
    return;
  }

  my @yaml_files = $dir->children(qr/\.ya?ml$/);
  $self->logger->info(sprintf("Found %d specialty files to load", scalar @yaml_files));

  my $loaded = 0;
  for my $file (@yaml_files) {
    eval {
      my $data = YAML::PP->new(
        schema => [qw/ + Perl /],
        yaml_version => ['1.2', '1.1'],
      )->load_string($file->slurp_utf8);

      unless ($data->{name}) {
        $self->logger->warn("Skipping $file - no name field");
        return;
      }

      my $specialty = Game::EvonyTKR::Model::Specialty->from_hash($data);
      $self->specialties->{$specialty->name} = $specialty;
      $loaded++;
      $self->logger->debug("Loaded specialty: " . $specialty->name);
    };
    if ($@) {
      $self->logger->error("Failed to load $file: $@");
    }
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
  return [sort keys %{$self->specialties}];
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
