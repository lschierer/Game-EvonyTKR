use v5.42.0;
use experimental qw(class);
use utf8::all;
require Data::Printer;
require Game::EvonyTKR::Model::General::Pair;

class Game::EvonyTKR::Model::General::Pair::Manager :
  isa(Game::EvonyTKR::Shared::Constants) {
  use overload
    'bool'     => sub { $_[0]->_isTrue() },
    'fallback' => 0;

  field $rootManager    : param;
  field $generalManager : param;
  field $conflicts     = $rootManager->conflictDetector;
  field $pairs_by_type = {};    # e.g., { Mounted => [ [genA, genB], ... ] }

  method get_pairs_by_type($type) {
    my $pairs = [];

    if (exists $pairs_by_type->{$type}) {
      $pairs = $pairs_by_type->{$type} // [];
    }
    else {
      $self->logger->error("invalid type $type requested");
    }

    return [@{$pairs}];    # shallow copy to protect internal structure
  }

  method get_pair_types() {
    return sort keys %{$pairs_by_type};
  }

  method build_pairs($primary) {
     my $generals = $generalManager->get_all_generals();
     my %initial_counts;
     foreach my $type (keys %{$pairs_by_type}) {
       my $tc = scalar @{ $pairs_by_type->{$type} };
       $initial_counts{$type} = $tc;
     }

      foreach
        my $secondary (sort { $a->name cmp $b->name } values %{$generals}) {
        next if $primary->name eq $secondary->name;
        $self->logger->debug(sprintf(
          'testing if %s and %s conflict.',
          $primary->name, $secondary->name
        ));
        $primary->populateBuiltInBook($rootManager->bookManager);
        $secondary->populateBuiltInBook($rootManager->bookManager);
        next
          unless $conflicts->are_generals_compatible($primary, $secondary);

        $self->logger->debug(sprintf(
          'no conflict, testing %s and %s for common type.',
          $primary->name, $secondary->name
        ));
        my $primary_types     = $primary->type   // [];
        my $secondary_types   = $secondary->type // [];
        my %primary_types_map = map { $_ => 1 } @$primary_types;
        my @common = grep { $primary_types_map{$_} } @$secondary_types;
        @common = sort @common;
        next unless @common;
        $self->logger->debug(sprintf(
          '%s and %s pair based on %s',
          $primary->name, $secondary->name, Data::Printer::np(@common)
        ));
        my $pair = Game::EvonyTKR::Model::General::Pair->new(
          primary   => $primary,
          secondary => $secondary,
        );
        $pair->setRootManager($rootManager);

        for my $t (@common) {
          push @{ $pairs_by_type->{$t} }, $pair;
        }
      }

    my $total_added = 0;
    foreach my $type (keys %{$pairs_by_type}) {
      my $tc = scalar @{ $pairs_by_type->{$type} } // 0;
      my $delta = $tc - ($initial_counts{$type} //0);
      $total_added += $delta;
      $self->logger->debug(sprintf('general %s has %s pairs for type %s',
      $primary->name, $delta, $type));
    }
    $self->logger->info(sprintf('there are %s pairs for %s', $total_added, $primary->name));

  }
}
