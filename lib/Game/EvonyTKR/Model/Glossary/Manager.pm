use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Game::EvonyTKR::Shared::Constants;
require Game::EvonyTKR::Model::Glossary;
require Data::Printer;
require Path::Tiny;
require YAML::PP;
use namespace::clean;

class Game::EvonyTKR::Model::Glossary::Manager :
  isa(Game::EvonyTKR::Shared::Constants) {
  use Carp;
  use List::Util qw(first);

  field $SourceDir : param;
  field $terms : reader //= {};
  field $terms_by_letter : reader //= {};

  method importAll($glossaryDir = undef) {
    $glossaryDir //= $SourceDir->child('collections/Glossary');
    
    $self->logger->info("Starting import of glossary from $glossaryDir");
    
    unless (-d $glossaryDir) {
      $self->logger->warn("Glossary directory $glossaryDir does not exist");
      return;
    }

    # Find all YAML files in the glossary directory
    my @yaml_files = $glossaryDir->children(qr/\.ya?ml$/);
    
    foreach my $file (@yaml_files) {
      $self->logger->debug("Processing glossary file: $file");
      $self->_import_file($file);
    }
    
    $self->_build_letter_index();
    
    my $term_count = scalar keys %$terms;
    $self->logger->info("Glossary import complete. Loaded $term_count terms.");
  }

  method _import_file($file) {
    my $yaml = YAML::PP->new();
    
    my $success = eval {
      $self->logger->debug("Loading YAML from $file");
      my $data = $yaml->load_file($file);
      $self->logger->debug("YAML loaded successfully: " . Data::Printer::np($data));
      
      unless ($data && $data->{glossary} && ref($data->{glossary}) eq 'ARRAY') {
        $self->logger->warn("Invalid glossary structure in $file");
        return 0;
      }
      
      $self->logger->debug("Processing " . scalar(@{$data->{glossary}}) . " terms from $file");
      
      foreach my $term_data (@{$data->{glossary}}) {
        $self->logger->debug("Creating term object for: " . ($term_data->{term} // 'UNDEFINED'));
        
        my $term_obj = Game::EvonyTKR::Model::Glossary->new(
          term => $term_data->{term},
          definition => $term_data->{definition},
          synonyms => $term_data->{synonyms} // [],
          related_terms => $term_data->{related_terms} // [],
          examples => $term_data->{examples} // [],
          owner => $term_data->{owner} // '',
          status => $term_data->{status} // 'approved',
        );
        
        # Store by term name (case-insensitive key)
        my $key = lc($term_data->{term});
        $terms->{$key} = $term_obj;
        
        $self->logger->debug("Successfully loaded term: " . $term_data->{term});
      }
      
      return 1; # Explicit success
    };
    
    if (!$success) {
      my $error = $@ || "Unknown error";
      $self->logger->error("Failed to load glossary file $file: $error");
    }
  }

  method _build_letter_index() {
    $terms_by_letter = {};
    
    foreach my $term_obj (values %$terms) {
      my $letter = $term_obj->first_letter();
      $terms_by_letter->{$letter} //= [];
      push @{$terms_by_letter->{$letter}}, $term_obj;
    }
    
    # Sort terms within each letter
    foreach my $letter (keys %$terms_by_letter) {
      $terms_by_letter->{$letter} = [
        sort { lc($a->term) cmp lc($b->term) } @{$terms_by_letter->{$letter}}
      ];
    }
  }

  method getAll() {
    return [sort { lc($a->term) cmp lc($b->term) } values %$terms];
  }

  method getByLetter($letter) {
    $letter = uc($letter);
    return $terms_by_letter->{$letter} // [];
  }

  method getDefinitionForTerm($search_term) {
    # First try exact match on term name
    my $key = lc($search_term);
    return $terms->{$key} if exists $terms->{$key};
    
    # Then search through synonyms
    foreach my $term_obj (values %$terms) {
      return $term_obj if $term_obj->matches_search($search_term);
    }
    
    return undef;
  }

  method getByName($term_name) {
    return $self->getDefinitionForTerm($term_name);
  }

  method searchTerms($query) {
    my @results;
    my $lc_query = lc($query);
    
    foreach my $term_obj (values %$terms) {
      # Match in term name
      if (index(lc($term_obj->term), $lc_query) >= 0) {
        push @results, $term_obj;
        next;
      }
      
      # Match in definition
      if (index(lc($term_obj->definition), $lc_query) >= 0) {
        push @results, $term_obj;
        next;
      }
      
      # Match in synonyms
      foreach my $synonym (@{$term_obj->synonyms}) {
        if (index(lc($synonym), $lc_query) >= 0) {
          push @results, $term_obj;
          last;
        }
      }
    }
    
    return [sort { lc($a->term) cmp lc($b->term) } @results];
  }

  method getAvailableLetters() {
    return [sort keys %$terms_by_letter];
  }
}

1;
