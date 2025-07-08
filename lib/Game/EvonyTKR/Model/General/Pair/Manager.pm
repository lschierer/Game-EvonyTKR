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

  field $rootManager : param;
  field $generalManager : param;
  field $conflictGroupManager : param;
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

  method build_pairs() {
    my $generals     = $generalManager->get_all_generals();
    my $conflicts    = $conflictGroupManager;
    my $generalCount = scalar keys %{$generals};
    $self->logger->debug("building pairs out of $generalCount");
    foreach my $primary (values %{$generals}) {
      foreach my $secondary (values %{$generals}) {
        next if $primary->name eq $secondary->name;
        $self->logger->debug(sprintf(
          'testing if %s and %s conflict.',
          $primary->name, $secondary->name
        ));
        next
          unless $conflicts->are_generals_compatible($primary->name,
          $secondary->name);

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
    }
    my $pairTypeCount = scalar keys %{$pairs_by_type};
    $self->logger->info("returning $pairTypeCount sets of pairs.");
    $self->logger->debug(
      "returning " . join(', ', keys %{$pairs_by_type}) . " types of pairs.");
    foreach my $type (keys %{$pairs_by_type}) {
      my $tc = scalar @{ $pairs_by_type->{$type} };
      $self->logger->debug("returning $tc pairs for $type");
    }

  }
}
