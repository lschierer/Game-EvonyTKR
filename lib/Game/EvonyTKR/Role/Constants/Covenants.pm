use v5.42.0;
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require Hash::Util;

package Game::EvonyTKR::Role::Constants::Covenants {
  use Moo::Role;
  use Const::Fast;
  use Carp;

  has 'CovenantCategories' => (
    is => 'ro',
    lazy => 1,
    default => sub ($self) {
      return {
        None         => 0,
        War          => 1,
        Cooperation  => 2,
        Peace        => 3,
        Faith        => 4,
        Honor        => 5,
        Civilization => 6,
      };
    }
  );

  has 'CovenantCategoryValues' => (
    is => 'ro',
    lazy => 1,
    default => sub ($self) {
      my $cats = $self->CovenantCategories;
      # Sort keys by their numeric value, return lowercase
      my @ordered = sort { $cats->{$a} <=> $cats->{$b} } keys %$cats;
      return [map { lc $_ } @ordered];
    }
  );

  has 'CovenantCategoryLabels' => (
    is => 'ro',
    lazy => 1,
    default => sub ($self) {
      my $cats = $self->CovenantCategories;
      # Same order, but keep original capitalization
      my @ordered = sort { $cats->{$a} <=> $cats->{$b} } keys %$cats;
      return \@ordered;
    }
  );

  sub checkCovenantLevel ($self, $proposedLevel) {
    unless (defined($proposedLevel) && length($proposedLevel)) {
      $self->logger->error("Invalid proposed level!!! $proposedLevel");
      return 0;
    }
    my $check = {};
    foreach my $key ($self->CovenantCategoryValues->@*) {
      $check->{$key} = 1;
    }
    return exists $check->{$proposedLevel};
  }
}
1;
__END__
