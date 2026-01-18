package Game::EvonyTKR::Loader::Glossary;
use v5.42.0;
use utf8::all;
use Mooish::Base -standard;
with 'WebFramework::Role::Logger';
with 'WebFramework::Role::Markdown';

use Path::Tiny qw(path);
use YAML::PP;

use Game::EvonyTKR::Model::Glossary;

has app_config => (
  is      => 'ro',
  default => sub { {} },
);

=head1 NAME

Game::EvonyTKR::Loader::Glossary - Load glossary terms from YAML files at startup

=head1 SYNOPSIS

    my $loader = Game::EvonyTKR::Loader::Glossary->new(
        data_dir => 'share/collections/Glossary',
    );
    my $count = $loader->load_all();

    # Get all terms
    my $terms = $loader->get_all_terms();

    # Get available letters for navigation
    my $letters = $loader->get_available_letters();

=head1 DESCRIPTION

Loads glossary terms from YAML files at application startup.
Each YAML file contains terms starting with a specific letter.

=cut

has data_dir => (
  is       => 'ro',
  required => 1,
);

# All loaded terms
has terms => (
  is      => 'rw',
  default => sub { [] },
);

# Terms indexed by first letter
has terms_by_letter => (
  is      => 'rw',
  default => sub { {} },
);

# Available letters (for navigation)
has available_letters => (
  is      => 'rw',
  default => sub { [] },
);

# Statistics
has stats => (
  is      => 'rw',
  default => sub {
    {
      total_terms => 0,
      files_loaded => 0,
    };
  },
);

sub load_all ($self) {
  $self->logger->info("Loading glossary terms from " . $self->data_dir);

  my $data_path = path($self->data_dir);

  unless ($data_path->is_dir) {
    $self->logger->error("Glossary directory not found: " . $self->data_dir);
    return 0;
  }

  my @yaml_files = $data_path->children(qr/\.ya?ml$/);
  @yaml_files = grep { $_->basename !~ /schema/ } @yaml_files;

  $self->logger->info(sprintf("Found %d glossary YAML files", scalar(@yaml_files)));

  my $yp = YAML::PP->new();
  my @all_terms;
  my %by_letter;
  my %letters_seen;
  my $files_loaded = 0;

  foreach my $file (sort @yaml_files) {
    $self->logger->debug("Processing $file");

    my $data = eval { $yp->load_file($file->stringify) };
    if ($@) {
      $self->logger->error("Failed to parse $file: $@");
      next;
    }

    next unless $data->{glossary};

    foreach my $term_data (@{ $data->{glossary} }) {
      # Render the definition as markdown
      my $rendered_def = '';
      if ($term_data->{definition}) {
        $rendered_def = $self->markdown_string_to_html($term_data->{definition});
      }

      my $term = Game::EvonyTKR::Model::Glossary->new(
        term          => $term_data->{term},
        definition    => $term_data->{definition}    // '',
        rendered_def  => $rendered_def,
        synonyms      => $term_data->{synonyms}      // [],
        related_terms => $term_data->{related_terms} // [],
        examples      => $term_data->{examples}      // [],
        owner         => $term_data->{owner}         // '',
        status        => $term_data->{status}        // 'approved',
      );

      push @all_terms, $term;

      # Index by first letter
      my $letter = $term->first_letter;
      $letters_seen{$letter} = 1;
      $by_letter{$letter} //= [];
      push @{ $by_letter{$letter} }, $term;
    }

    $files_loaded++;
  }

  # Sort terms within each letter
  foreach my $letter (keys %by_letter) {
    $by_letter{$letter} = [sort @{ $by_letter{$letter} }];
  }

  # Sort available letters
  my @sorted_letters = sort keys %letters_seen;

  $self->terms(\@all_terms);
  $self->terms_by_letter(\%by_letter);
  $self->available_letters(\@sorted_letters);
  $self->stats({
    total_terms  => scalar(@all_terms),
    files_loaded => $files_loaded,
  });

  $self->logger->info(sprintf(
    "Loaded %d glossary terms from %d files",
    scalar(@all_terms), $files_loaded
  ));

  return scalar(@all_terms);
}

=head2 get_all_terms

Returns arrayref of all glossary terms, sorted alphabetically.

=cut

sub get_all_terms ($self) {
  return [sort @{ $self->terms }];
}

=head2 get_available_letters

Returns arrayref of letters that have terms (for navigation).

=cut

sub get_available_letters ($self) {
  return $self->available_letters;
}

=head2 get_terms_for_letter

Returns arrayref of terms starting with the given letter.

=cut

sub get_terms_for_letter ($self, $letter) {
  return $self->terms_by_letter->{uc($letter)} // [];
}

=head2 find_term

Find a term by name (case-insensitive, also checks synonyms).

=cut

sub find_term ($self, $search) {
  foreach my $term (@{ $self->terms }) {
    return $term if $term->matches_search($search);
  }
  return undef;
}

=head2 term_count

Returns the total number of glossary terms loaded.

=cut

sub term_count ($self) {
  return $self->stats->{total_terms};
}

1;

__END__

=head1 AUTHOR

Game::EvonyTKR Development Team

=head1 SEE ALSO

L<Game::EvonyTKR::Model::Glossary>

=cut
