package Game::EvonyTKR::Model::Buff::Summarizer::Pair;
use v5.42.0;
use utf8::all;
require Data::Printer;
require Game::EvonyTKR::Model::Buff::Value;
require Game::EvonyTKR::Service::Conflicts;
use Mojo::Base 'Game::EvonyTKR::Model::Buff::Summarizer',                    -signatures;
use List::AllUtils qw(first any all none uniq);
use Carp;
use diagnostics;

has pair => undef;

has pairBuffValues => sub {
  {
    'Ground Troops' =>
      { 'March Size' => 0, 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
    'Mounted Troops' =>
      { 'March Size' => 0, 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
    'Ranged Troops' =>
      { 'March Size' => 0, 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
    'Siege Machines' =>
      { 'March Size' => 0, 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
    'Overall' =>
      { 'March Size' => 0, 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
  };
};

has pairDebuffValues => sub {
  {
    'Ground Troops'  => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
    'Mounted Troops' => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
    'Ranged Troops'  => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
    'Siege Machines' => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
    'Overall'        => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
  };
};

sub updateBuffs ($self) {
  unless($self->pair &&
    ref($self->pair) &&
    blessed($self->pair) &&
    $self->pair->isa('Game::EvonyTKR::Model::General::Pair')
  ){
    $self->logger->error(sprintf('%s requires a Game::EvonyTKR::Model::General::Pair', __PACKAGE__));
    return;
  }

  # Calculate primary buffs
  $self->general($self->pair->primary);
  $self->SUPER::updateBuffs();
  foreach my $troopType (keys %{ $self->buffValues }) {
    foreach my $attribute (keys %{ $self->buffValues->{$troopType} }) {
      $self->pairBuffValues->{$troopType}->{$attribute} +=
        $self->buffValues->{$troopType}->{$attribute};
    }
  }

  # Calculate secondary buffs
  $self->general($self->pair->secondary);
  $self->isPrimary(0);
  $self->SUPER::updateBuffs();
  foreach my $troopType (keys %{ $self->buffValues }) {
    foreach my $attribute (keys %{ $self->buffValues->{$troopType} }) {
      $self->pairBuffValues->{$troopType}->{$attribute} +=
        $self->buffValues->{$troopType}->{$attribute};
    }
  }
}

# TODO: do the same with updateDebuffs as I just did with updateBuffs

# Override getGenericBookValue to check compatibility with both generals in pair
sub _getGenericBookValue_impl ($self, $attribute, $troopType) {
  # For pairs, a book must be compatible with BOTH generals
  # Check primary (same_side=1) and secondary (same_side=0 for partial conflicts)

  my $primary_value = $self->SUPER::_getGenericBookValue_impl($attribute, $troopType);

  # TODO: Check if book has partial conflict with secondary that would disable secondary's builtin
  # For now, just return primary value
  # Full implementation needs to:
  # 1. Identify which book(s) contributed to primary_value
  # 2. Check each against secondary with same_side => 0
  # 3. Exclude books that have partial conflicts
  # 4. A pair gets *6* books, not *3*.
  #    We need to add in the value from the additional 3.

  return $primary_value;
}


1;
__END__
