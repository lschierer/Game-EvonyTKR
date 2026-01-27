package Game::EvonyTKR::Role::Common;
use v5.42.0;
use experimental qw(class);
use utf8::all;
use Moo::Role;
require Data::Printer;
require Unicode::CaseFold;
require X500::DN;
require X500::RDN;
use List::AllUtils     qw(min max uniq none all );
use UUID               qw(uuid5);
use Unicode::CaseFold  qw(fc);
use Unicode::Normalize qw(NFKD);
use Carp;

has collection_dir => (
  is      => 'ro',
  lazy    => 1,
  default => sub {
    my $home = Mojo::Home->new->detect('Game::EvonyTKR');
    return $home->child('share/collections/data');
  }
);

sub normalize ($self, $name) {
  my $dn = Encode::is_utf8($name) ? $name : Encode::decode_utf8($name);
  my $nn = fc(NFKD($dn));
  $nn =~ s/[’''‛`´]/'/g;
  $nn =~ s/[""‟]/"/g;      # Quotes
  return lc($nn);
}

# Generic hydrator:
#  - $list_cb:   sub ($app) -> arrayref of expected names
#  - $fetch_cb:  sub ($name) -> object or undef
#  - $state_ref: hashref-like storage (use 'state %cache' in the caller)
#  - returns hashref of hydrated objects keyed by normalized name
sub _hydrate_from_list ($c, $app, $list_cb, $fetch_cb, $state_ref,
  $sig_state_ref) {

  my $_norm = sub ($c, $name) {
    my $k = lc($c->normalize($name) // '');
    return $k;
  };

  my $names = $list_cb->($app) // [];
  # Build a cheap signature of "what should exist"
  my $sig = join "\0", sort map { $_norm->($c, $_) } @$names;

  # If signature unchanged, we’re fully up to date
  return $state_ref if defined $$sig_state_ref && $$sig_state_ref eq $sig;

  # Otherwise, only fetch missing ones
  for my $name (@$names) {
    my $key = $_norm->($c, $name);

    next if exists $state_ref->{$key};
    if (my $obj = $fetch_cb->($name)) {
      $state_ref->{$key} = $obj;
    }
  }

  # If we now cover the full set, bump signature
  my $have_all =
    (@$names == scalar grep { exists $state_ref->{ $_norm->($c, $_) } }
      @$names);
  $$sig_state_ref = $sig if $have_all;

  return $state_ref;
}

has globalDN => (
  is      => 'ro',
  lazy    => 1,
  default => sub {
    return X500::DN->new(
      X500::RDN->new('OU' => 'EvonyTKR'),
      X500::RDN->new('OU' => 'Game'),
      X500::RDN->new('OU' => 'module'),
      X500::RDN->new('dc' => 'Perl'),
      X500::RDN->new('dc' => 'org'),
    );
  }
);

has UUID5_base => (
  is      => 'ro',
  lazy    => 1,
  default => sub {
    my ($self) = @_;
    my $ns_base = uuid5(dns => 'perl.org');
    return uuid5($ns_base, $self->globalDN->getX500String());
  }
);

sub normalizeSpecialtyLevels ($self, @specialties) {
  my @normalized = @specialties;

  # Ensure we have exactly 4 specialties
  if (scalar @normalized != 4) {
    $self->logger->warn("Expected 4 specialties, got "
        . scalar @normalized
        . ". Padding with defaults.");
    while (scalar @normalized < 4) {
      push @normalized, 'gold';
    }
    @normalized = @normalized[0 .. 3] if scalar @normalized > 4;
  }

  # Validate and normalize each specialty level
  foreach my $index (0 .. 3) {
    if (none { $_ eq $normalized[$index] } $self->SpecialtyLevelValues->@*) {
      $self->logger->warn(
"Invalid specialty level at index $index: $normalized[$index], using default"
      );
      $normalized[$index] = 'gold';
    }
  }

  # Apply the specialty 4 rule
  my $all_gold = all { $_ eq 'gold' } @normalized[0 .. 2];

  if ($all_gold && $normalized[3] eq 'none') {
    $self->logger->warn(
"When specialties 1-3 are all gold, specialty 4 cannot be 'none'. Setting to gold."
    );
    $normalized[3] = 'gold';
  }

  if (!$all_gold && $normalized[3] ne 'none') {
    $self->logger->warn(
"When specialties 1-3 are not all gold, specialty 4 must be 'none'. Setting to none."
    );
    $normalized[3] = 'none';
  }

  return @normalized;
}

1;
__END__
