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
  use UUID qw(uuid5);
  use Unicode::CaseFold qw(fc);
  use Unicode::Normalize qw(NFKD);

  sub normalize ($self, $name) {
    my $dn = Encode::is_utf8($name) ? $name : Encode::decode_utf8($name);
    my $nn = fc(NFKD($dn));
    $nn =~ s/[’''‛`´]/'/g;
    $nn =~ s/[""‟]/"/g;      # Quotes
    return $nn;
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
