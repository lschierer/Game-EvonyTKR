use v5.40.0;
use experimental qw(class);
use utf8::all;
require JSON::PP;
require Scalar::Util;

class Game::EvonyTKR::Model::General::Pair : isa(Game::EvonyTKR::Model::Data) {
  use overload
    '""'       => \&as_string,
    'fallback' => 1;

  field $primary : reader : param;
  field $secondary : reader : param;
  field $rootManager : reader = undef;

  #computed fields;

  field $marchbuff : reader = 0;

  field $attackbuff : reader = 0;

  method setRootManager ($nm) {
    if (Scalar::Util::blessed $nm eq 'Game::EvonyTKR::Model::EvonyTKR::Manager')
    {
      $rootManager = $nm;
      return 1;
    }
    return 0;
  }

  # some parameters unused currently, but adding them for future growth
  method updateBuffs (
    $generalType          = '',
    $primaryAscending     = 'red5',
    $primarySpeciality1   = 'gold',
    $primarySpeciality2   = 'gold',
    $primarySpeciality3   = 'gold',
    $primarySpeciality4   = 'gold',
    $secondarySpeciality1 = 'gold',
    $secondarySpeciality2 = 'gold',
    $secondarySpeciality3 = 'gold',
    $secondarySpeciality4 = 'gold',
    $keepLevel            = 40,
    $primaryLevel         = 45,
    $secondaryLevel       = 45,
  ) {
    $self->updateMarchSize(
      $primaryAscending,     $primarySpeciality1,   $primarySpeciality2,
      $primarySpeciality3,   $primarySpeciality4,   $secondarySpeciality1,
      $secondarySpeciality2, $secondarySpeciality3, $secondarySpeciality4,
      $keepLevel,
    );
    $self->updateAttackIncrease(
      $generalType,          $primaryAscending,     $primarySpeciality1,
      $primarySpeciality2,   $primarySpeciality3,   $primarySpeciality4,
      $secondarySpeciality1, $secondarySpeciality2, $secondarySpeciality3,
      $secondarySpeciality4, $keepLevel,
    );
  }

  # some parameters unused currently, but adding them for future growth
  method updateMarchSize (
    $primaryAscending     = 'None',
    $primarySpeciality1   = 'none',
    $primarySpeciality2   = 'none',
    $primarySpeciality3   = 'none',
    $primarySpeciality4   = 'none',
    $secondarySpeciality1 = 'none',
    $secondarySpeciality2 = 'none',
    $secondarySpeciality3 = 'none',
    $secondarySpeciality4 = 'none',
    $keepLevel            = 40,
  ) {
    $self->logger->debug(
      "Primary book name: " . ($primary->builtInBookName // 'undefined'));
    $self->logger->debug(
      "Secondary book name: " . ($secondary->builtInBookName // 'undefined'));

    # set $marchbuff to 0 to ensure this function is idempotent
    $marchbuff = 0;

    if ($rootManager->generalConflictGroupManager->is_book_compatible(
      'March Size Increase',
      $primary->name
    )) {
      if ($rootManager->generalConflictGroupManager->is_book_compatible(
        'March Size Increase',
        $secondary->name
      )) {
        $marchbuff += 12;
      }
    }

    if (not defined $primary->builtInBook
      && length($primary->builtInBookName) > 0) {
      $self->logger->trace("Root manager book manager: "
          . (defined $rootManager->bookManager ? 'defined' : 'undefined'));
      if (defined $rootManager) {
        $self->logger()
          ->debug("requesting populateBuiltInBook for " . $primary->name());
        $primary->populateBuiltInBook($rootManager->bookManager);
      }
      else {
        $self->logger->logcroak("No Root Manager Available");
      }
    }
    if (not defined $secondary->builtInBook
      && length($secondary->builtInBookName) > 0) {
      $self->logger->trace("Root manager book manager: "
          . (defined $rootManager->bookManager ? 'defined' : 'undefined'));
      if (defined $rootManager) {
        $self->logger()
          ->debug("requesting populateBuiltInBook for " . $secondary->name());
        $secondary->populateBuiltInBook($rootManager->bookManager);
      }
      else {
        $self->logger->logcroak("No Root Manager Available");
      }
    }

    for my $book ($primary->builtInBook(), $secondary->builtInBook()) {
      if ( defined $book
        && blessed($book)
        && blessed($book) eq 'Game::EvonyTKR::Model::Book') {
        $self->logger->debug("adding buffs to pair for book " . $book->name);
        my $bv = $book->get_buffs('March Size Capacity');
        $self->logger->trace(sprintf(
          'found %s in %s buffs for %s/%s.',
          $bv, $book->name, $primary->name, $secondary->name,
        ));
        $marchbuff += $bv;
      }
      else {
        $self->logger->error(
          "cannot update marchbuff with book " . Data::Printer::np($book));
      }
    }

    my @specialityNames  = @{ $primary->specialityNames };
    my @specialityLevels = (
      $primarySpeciality1, $primarySpeciality2,
      $primarySpeciality3, $primarySpeciality4
    );
    foreach my $sn_index (0 .. $#specialityNames) {
      my $sn = $specialityNames[$sn_index];
      my $sl = lc($specialityLevels[$sn_index]);
      $self->logger->debug(
        "processing primary " . $primary->name . " $sn at level $sl");
      my $speciality = $rootManager->specialityManager->getSpeciality($sn);
      if ($speciality) {
        $self->logger->debug(
          sprintf('checking %s for marchbuff', $speciality->name));

        my $sv = $speciality->get_buffs_at_level($sl, 'March Size Capacity');
        $self->logger->debug("retrieved $sv as total for level $sl "
            . $speciality->name
            . " as part of "
            . $primary->name);
        $marchbuff += $sv;

      }
      else {
        $self->logger->error(sprintf(
          'cannot retrieve speciality %s for %s', $sn, $primary->name,
        ));
      }
    }

    @specialityNames  = @{ $secondary->specialityNames };
    @specialityLevels = (
      $secondarySpeciality1, $secondarySpeciality2,
      $secondarySpeciality3, $secondarySpeciality4
    );
    foreach my $sn_index (0 .. $#specialityNames) {
      my $sn = $specialityNames[$sn_index];
      my $sl = lc($specialityLevels[$sn_index]);
      $self->logger->debug(
        "processing secondary " . $secondary->name . " $sn at level $sl");
      my $speciality = $rootManager->specialityManager->getSpeciality($sn);
      if ($speciality) {
        $self->logger->debug(
          sprintf('checking %s for marchbuff', $speciality->name));

        my $sv = $speciality->get_buffs_at_level($sl, 'March Size Capacity');
        $self->logger->debug("retrieved $sv as total for level $sl "
            . $speciality->name
            . " as part of "
            . $secondary->name);
        $marchbuff += $sv;

      }
      else {
        $self->logger->error(sprintf(
          'cannot retrieve speciality %s for %s',
          $sn, $secondary->name,
        ));
      }
    }

    my $aa = $rootManager->ascendingAttributesManager->getAscendingAttributes(
      $primary->name);
    if ($aa) {
      $self->logger->debug("retrieved ascending buffs for " . $primary->name);
      my $av =
        $aa->get_buffs_at_level($primaryAscending, 'March Size Capacity');
      $self->logger->debug(sprintf(
'%s Ascending Attributes has March Size Capacity buffs with total %s at level %s',
        $primary->name, $av, $primaryAscending,
      ));
      $marchbuff += $av;
    }
    else {
      $self->logger->error(
        "cannot find Ascending Attributes for " . $primary->name);
    }

  }

  method updateAttackIncrease (
    $generalType           = '',
    $primaryAscending      = 'None',
    $primarySpeciality1    = 'none',
    $primarySpeciality2    = 'none',
    $primarySpeciality3    = 'none',
    $primarySpeciality4    = 'none',
    $secondarySpeciality1  = 'none',
    $secondarySpeciality2  = 'none',
    $secondarySpeciality3  = 'none',
    $secondarySpeciality4  = 'none',
    $keepLevel             = 40,
    $primaryLevel          = 45,
    $allowedBuffActivation = [],
  ) {
    $self->logger->debug(
      "Primary book name: " . ($primary->builtInBookName // 'undefined'));
    $self->logger->debug(
      "Secondary book name: " . ($secondary->builtInBookName // 'undefined'));

    # set $total to 0 to ensure this function is idempotent
    my $total = 0;

    if (
      $generalType =~ /Mounted/i
      && $rootManager->generalConflictGroupManager->is_book_compatible(
        'Mounted Attack Increase',
        $primary->name
      )
    ) {
      if ($rootManager->generalConflictGroupManager->is_book_compatible(
        'Mounted Troop Attack',
        $secondary->name
      )) {
        $total += 20;
      }
    }

    if (
      $generalType =~ /Ground/i
      && $rootManager->generalConflictGroupManager->is_book_compatible(
        'Ground Troop Attack',
        $primary->name
      )
    ) {
      if ($rootManager->generalConflictGroupManager->is_book_compatible(
        'Ground Troop Attack',
        $secondary->name
      )) {
        $total += 20;
      }
    }

    if (
      $generalType =~ /Ranged/i
      && $rootManager->generalConflictGroupManager->is_book_compatible(
        'Ranged Troop Attack',
        $primary->name
      )
    ) {
      if ($rootManager->generalConflictGroupManager->is_book_compatible(
        'Ranged Troop Attack',
        $secondary->name
      )) {
        $total += 20;
      }
    }

    if (
      $generalType =~ /Siege/i
      && $rootManager->generalConflictGroupManager->is_book_compatible(
        'Siege Machine Attack',
        $primary->name
      )
    ) {
      if ($rootManager->generalConflictGroupManager->is_book_compatible(
        'Siege Machine Attack',
        $secondary->name
      )) {
        $total += 20;
      }
    }

    if (not defined $primary->builtInBook
      && length($primary->builtInBookName) > 0) {
      $self->logger->trace("Root manager book manager: "
          . (defined $rootManager->bookManager ? 'defined' : 'undefined'));
      if (defined $rootManager) {
        $self->logger()
          ->debug("requesting populateBuiltInBook for " . $primary->name());
        $primary->populateBuiltInBook($rootManager->bookManager);
      }
      else {
        $self->logger->logcroak("No Root Manager Available");
      }
    }
    if (not defined $secondary->builtInBook
      && length($secondary->builtInBookName) > 0) {
      $self->logger->trace("Root manager book manager: "
          . (defined $rootManager->bookManager ? 'defined' : 'undefined'));
      if (defined $rootManager) {
        $self->logger()
          ->debug("requesting populateBuiltInBook for " . $secondary->name());
        $secondary->populateBuiltInBook($rootManager->bookManager);
      }
      else {
        $self->logger->logcroak("No Root Manager Available");
      }
    }

    for my $book ($primary->builtInBook(), $secondary->builtInBook()) {
      if ( defined $book
        && blessed($book)
        && blessed($book) eq 'Game::EvonyTKR::Model::Book') {
        $self->logger->debug("adding buffs to pair for book " . $book->name);
        my $bv = $book->get_buffs('Attack', $generalType);
        $self->logger->trace(sprintf(
          'found %s in %s buffs for %s/%s.',
          $bv, $book->name, $primary->name, $secondary->name,
        ));
        $total += $bv;
      }
      else {
        $self->logger->error(
          "cannot update Attack with book " . Data::Printer::np($book));
      }
    }

    my @specialityNames  = @{ $primary->specialityNames };
    my @specialityLevels = (
      $primarySpeciality1, $primarySpeciality2,
      $primarySpeciality3, $primarySpeciality4
    );
    foreach my $sn_index (0 .. $#specialityNames) {
      my $sn = $specialityNames[$sn_index];
      my $sl = lc($specialityLevels[$sn_index]);
      $self->logger->debug(
        "processing primary " . $primary->name . " $sn at level $sl");
      my $speciality = $rootManager->specialityManager->getSpeciality($sn);
      if ($speciality) {
        $self->logger->debug(
          sprintf('checking %s for total', $speciality->name));

        my $sv = $speciality->get_buffs_at_level($sl, 'Attack', $generalType);
        $self->logger->debug("retrieved $sv as total for level $sl "
            . $speciality->name
            . " as part of "
            . $primary->name);
        $total += $sv;

      }
      else {
        $self->logger->error(sprintf(
          'cannot retrieve speciality %s for %s', $sn, $primary->name,
        ));
      }
    }

    @specialityNames  = @{ $secondary->specialityNames };
    @specialityLevels = (
      $secondarySpeciality1, $secondarySpeciality2,
      $secondarySpeciality3, $secondarySpeciality4
    );
    foreach my $sn_index (0 .. $#specialityNames) {
      my $sn = $specialityNames[$sn_index];
      my $sl = lc($specialityLevels[$sn_index]);
      $self->logger->debug(
        "processing secondary " . $secondary->name . " $sn at level $sl");
      my $speciality = $rootManager->specialityManager->getSpeciality($sn);
      if ($speciality) {
        $self->logger->debug(
          sprintf('checking %s for total', $speciality->name));

        my $sv = $speciality->get_buffs_at_level($sl, 'Attack', $generalType);
        $self->logger->debug("retrieved $sv as total for level $sl "
            . $speciality->name
            . " as part of "
            . $secondary->name);
        $total += $sv;

      }
      else {
        $self->logger->error(sprintf(
          'cannot retrieve speciality %s for %s',
          $sn, $secondary->name,
        ));
      }
    }

    my $aa = $rootManager->ascendingAttributesManager->getAscendingAttributes(
      $primary->name);
    if ($aa) {
      $self->logger->debug("retrieved ascending buffs for " . $primary->name);
      my $av =
        $aa->get_buffs_at_level($primaryAscending, 'Attack', $generalType);
      $self->logger->debug(sprintf(
        '%s Ascending Attributes has Attack buffs with total %s at level %s',
        $primary->name, $av, $primaryAscending,
      ));
      $total += $av;
    }
    else {
      $self->logger->error(
        "cannot find Ascending Attributes for " . $primary->name);
    }
    $attackbuff = $total;
  }

  # Method to convert to hash
  method to_hash {
    return {
      primary   => $primary,
      secondary => $secondary,
      # Add any other useful properties you might need
    };
  }

  # Method for JSON serialization
  method TO_JSON {
    return $self->to_hash();
  }

  # Stringification method using JSON
  method as_string {
    my $json =
      JSON::PP->new->utf8->pretty->canonical(1)
      ->allow_blessed(1)
      ->convert_blessed(1)
      ->encode($self->to_hash());
    return $json;
  }

}
1;
