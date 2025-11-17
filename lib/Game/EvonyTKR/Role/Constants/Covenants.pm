use v5.42.0;
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require Hash::Util;

package Game::EvonyTKR::Role::Constants::Covenants {
  use Mojo::Base -role, -signatures;
  use Const::Fast;
  use Carp;

  has 'CovenantCategories' => sub ($self) {
    const my %tmp => (
      None          => 0,
      War           => 1,
      Cooperation   => 2,
      Peace         => 3,
      Faith         => 4,
      Honor         => 5,
      Civilization  => 6,
    );
    return \%tmp;
  };

  has 'CovenantCategoryValues' => sub ($self) {
    my $cats = $self->CovenantCategories;
    # Sort keys by their numeric value, return lowercase
    my @ordered = sort { $cats->{$a} <=> $cats->{$b} } keys %$cats;
    return [ map { lc $_ } @ordered ];
  };

  has 'CovenantCategoryLabels' => sub ($self) {
    my $cats = $self->CovenantCategories;
    # Same order, but keep original capitalization
    my @ordered = sort { $cats->{$a} <=> $cats->{$b} } keys %$cats;
    return \@ordered;
  };
}
1;
__END__
