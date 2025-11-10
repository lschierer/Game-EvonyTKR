use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require JSON::PP;
require Mojo::JSON;
require X500::DN;
require X500::RDN;
require Unicode::CaseFold;

package Game::EvonyTKR::Role::Common {
  use Mojo::Base -role, -signatures;
  use Carp;
  use UUID               qw(uuid5);
  use Unicode::CaseFold  qw(fc);
  use Unicode::Normalize qw(NFKD);

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
      $k =~ s/ /_/g;
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

  has 'globalDN' => sub {
    return X500::DN->new(
      X500::RDN->new('OU' => 'EvonyTKR'),
      X500::RDN->new('OU' => 'Game'),
      X500::RDN->new('OU' => 'module'),
      X500::RDN->new('dc' => 'Perl'),
      X500::RDN->new('dc' => 'org'),
    );
  };

  has 'UUID5_base' => sub ($self) {
    my $ns_base = uuid5(dns => 'perl.org');
    return uuid5($ns_base, $self->globalDN->getX500String());
  };
}
1;
__END__
