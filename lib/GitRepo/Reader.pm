use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;

class GitRepo::Reader {
  our $VERSION = '0.00.1';
  require Path::Tiny;
  use Carp;
  use DateTime;
  require Date::Manip;
  use Git::Repository;
  require Git::Repository::Log::Iterator;
  require Path::Tiny;
  use List::Util     qw(uniq);
  use List::AllUtils qw( uniqstr );
  use HTML::Entities qw(encode_entities);
  require Log::Log4perl;

  field $source_dir : param : reader //= './';
  field $git_repo : reader;
  field $logger : reader = Log::Log4perl->get_logger(__PACKAGE__);

  field $oldest  = 0;
  field $authors = {};

  ADJUST {
    # do not assume that we were given Path::Tiny objects for $source_dir

    $source_dir = Path::Tiny::path($source_dir);
    if (!$source_dir->is_dir()) {
      croak("source_dir $source_dir is not a directory");
    }

    # Initialize Git repository
    $git_repo = Git::Repository->new(work_tree => $source_dir->stringify);
  }

  method find_copyright_range {

    if (not $oldest) {
      my @args = qw(--full-history );
      my $iter = Git::Repository::Log::Iterator->new(@args);

      while (my $log = $iter->next) {

        my $current   = new Date::Manip::Date;
        my $timestamp = $log->author_gmtime;
        $logger->debug("retrieved timestamp $timestamp from the log");
        if (not $oldest) {
          $logger->info("setting initial time to $timestamp");
          $oldest = $timestamp;
        }
        elsif ($timestamp < $oldest) {
          $logger->debug("$timestamp is older than $oldest");
          $oldest = $timestamp;
        }
      }
    }

    # this is a separate test because the while loop might not actually succeed
    if ($oldest) {
      my $dt = DateTime->from_epoch(epoch => $oldest,);
      $logger->info($dt->year . " is the oldest year in the repo.");
      return $dt;
    }
    return 0;
  }

  method find_authors {
    if (not scalar keys %{$authors}) {
      my @args = qw(--full-history);
      my $iter = Git::Repository::Log::Iterator->new(@args);

      my $rootDir = $git_repo->git_dir();
      $rootDir = Path::Tiny::path($rootDir);
      my $mailmap_file = $rootDir->parent->child(".mailmap");
      my $mailmap;

      if ($mailmap_file->is_file()) {
        $mailmap = 1;
        $logger->info("mailmap '$mailmap_file' found");
      }
      else {
        $mailmap = 0;
        $logger->warn("No mailmap file at $mailmap_file");
      }

      while (my $log = $iter->next) {
        $logger->debug("inspecting " . $log->author_name);
        my $email = $log->author_email;
        my $name  = $log->author_name;
        if ($mailmap) {
          my $mm_check =
            $git_repo->run('check-mailmap', sprintf('%s <%s>', $name, $email));
          if ($mm_check && $mm_check !~ /^fatal:/) {
            $logger->debug("mm_check is '$mm_check'");
            if ($mm_check =~ /^(.*?)\s*<([^>]+)>/) {
              $name  = $1;
              $email = $2;
            }
          }
        }
        if (!exists $authors->{$name}) {
          $logger->debug("adding '$name'");
          $authors->{$name} = {
            name  => $name,
            email => $email,
          };
        }
      }
    }

    # test again because the while loop might have found something
    # but it also might not have
    if (scalar keys %{$authors}) {
      my @result;
      foreach my $key (keys %{$authors}) {
        my $name  = $authors->{$key}->{name};
        my $email = $authors->{$key}->{email};
        push @result, sprintf('%s <%s>', $name, $email);
      }
      return join(', ', @result);
    }
    return '';
  }

}
1;

__END__
#ABSTRACT: A caching module to read history and other meta data from a git repository
