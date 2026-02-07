use v5.42.0;
use utf8::all;
use lib 'lib';

package Module::Build::Custom;
use base qw(Module::Build);    # Inherit from Module::Build
use File::Find::Rule;

sub rscan_dir {
  print 'using custom rscan_dir';

  my ($self, $dir, $pattern) = @_;    # Don't use shift @_ here
  my @result;
  local $_;

  return \@result unless ($dir && -d $dir);

  my $rule = File::Find::Rule->new;

  # Skip the infrastructure dir as in your original logic
  # Prune and discard unwanted directories
  $rule->or(
    $rule->new->directory->name('infrastructure')->prune->discard,
    $rule->new->directory->name('node_modules')->prune->discard,
    $rule->new    # Continue for everything else
  );

  if ($pattern) {
    if (ref($pattern) eq 'Regexp' || !ref($pattern)) {
      $rule->name($pattern);
    }
    elsif (ref($pattern) eq 'CODE') {
      $rule->exec($pattern);
    }
  }

  my @files = $rule->in($dir);
  return \@files;
  return \@result;
}

sub find_pm_files {
  my $self = shift;
  # Use your custom rscan_dir to find .pm files in lib/
  my $files = $self->rscan_dir('lib', qr/\.pm$/);
  return { map { $_ => $_ } @$files };
}

sub find_pod_files {
  my $self  = shift;
  my $files = $self->rscan_dir('lib', qr/\.pod$/);
  return { map { $_ => $_ } @$files };
}

sub ACTION_manifest {
  my $self = shift;
  require ExtUtils::Manifest;

  print "Generating MANIFEST using File::Find::Rule...\n";

  # Find ALL files in project root, applying your exclusion rules
  my $files = $self->rscan_dir('.');

  # Manually write the MANIFEST file to bypass ExtUtils::Manifest's scan
  open my $fh, '>', 'MANIFEST' or die "Can't write MANIFEST: $!";
  foreach my $file (sort @$files) {
    # Clean up the path (remove ./ prefix)
    $file =~ s{^\./}{};
    print $fh "$file\n";
  }
  close $fh;
}

1;
__END__
