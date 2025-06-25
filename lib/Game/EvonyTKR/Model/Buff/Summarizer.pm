use v5.40.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require Game::EvonyTKR::Model::Buff::Value;
require JSON::PP;

class Game::EvonyTKR::Model::Buff::Summarizer :
  isa(Game::EvonyTKR::Model::Data) {
  # PODNAME: Game::EvonyTKR::Model::Buff
  use List::AllUtils qw( any none );
  use namespace::autoclean;
  use Types::Common qw( t );
  use Carp;
  use File::FindLib 'lib';
  use overload
    '""'       => \&as_string,
    '.'        => \&concat,
    'fallback' => 0;

  our $VERSION = 'v0.30.0';

  # Input parameters
  field $rootManager : param;
  field $general : param;
  field $isPrimary : reader : param  //= 1;
  field $targetType : reader : param //= '';

  # these are needed now
  field $ascendingLevel : reader : param //= 'red5';
  field $speciality1 : reader : param    //= 'gold';
  field $speciality2 : reader : param    //= 'gold';
  field $speciality3 : reader : param    //= 'gold';
  field $speciality4 : reader : param    //= 'gold';
  field $covenantLevel : reader : param  //= 'Civilization';

  # these are anticipated that I will need them in the future.
  field $generalLevel : reader : param   //= 45;
  field $includePassive : reader : param //= 1;
  field $keepLevel : reader : param      //= 40;

# Output values - stored in hashes for easier access
# Output values - stored in hashes for easier access with troop type granularity
  field $buffValues : reader = {
    'Ground Troops' =>
      { 'March Size Capacity' => 0, 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
    'Mounted Troops' =>
      { 'March Size Capacity' => 0, 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
    'Ranged Troops' =>
      { 'March Size Capacity' => 0, 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
    'Siege Machines' =>
      { 'March Size Capacity' => 0, 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
    'Overall' =>
      { 'March Size Capacity' => 0, 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
  };

  field $debuffValues : reader = {
    'Ground Troops'  => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
    'Mounted Troops' => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
    'Ranged Troops'  => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
    'Siege Machines' => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
  };

  # Accessor methods for backward compatibility
  method marchIncrease() {
    my $type = $self->_getTroopTypeForTarget();
    return $buffValues->{$type}->{'March Size Capacity'};
  }

  method attackIncrease() {
    my $type = $self->_getTroopTypeForTarget();
    return $buffValues->{$type}->{'Attack'};
  }

  method defenseIncrease() {
    my $type = $self->_getTroopTypeForTarget();
    return $buffValues->{$type}->{'Defense'};
  }

  method hpIncrease() {
    my $type = $self->_getTroopTypeForTarget();
    return $buffValues->{$type}->{'HP'};
  }

  # Granular accessor methods for wall generals and other specialized needs
  method groundMarchIncrease() {
    return $buffValues->{'Ground Troops'}->{'March Size Capacity'};
  }

  method groundAttackIncrease() {
    return $buffValues->{'Ground Troops'}->{'Attack'};
  }

  method groundDefenseIncrease() {
    return $buffValues->{'Ground Troops'}->{'Defense'};
  }
  method groundHpIncrease() { return $buffValues->{'Ground Troops'}->{'HP'}; }

  method mountedMarchIncrease() {
    return $buffValues->{'Mounted Troops'}->{'March Size Capacity'};
  }

  method mountedAttackIncrease() {
    return $buffValues->{'Mounted Troops'}->{'Attack'};
  }

  method mountedDefenseIncrease() {
    return $buffValues->{'Mounted Troops'}->{'Defense'};
  }
  method mountedHpIncrease() { return $buffValues->{'Mounted Troops'}->{'HP'}; }

  method rangedMarchIncrease() {
    return $buffValues->{'Ranged Troops'}->{'March Size Capacity'};
  }

  method rangedAttackIncrease() {
    return $buffValues->{'Ranged Troops'}->{'Attack'};
  }

  method rangedDefenseIncrease() {
    return $buffValues->{'Ranged Troops'}->{'Defense'};
  }
  method rangedHpIncrease() { return $buffValues->{'Ranged Troops'}->{'HP'}; }

  method siegeMarchIncrease() {
    return $buffValues->{'Siege Machines'}->{'March Size Capacity'};
  }

  method siegeAttackIncrease() {
    return $buffValues->{'Siege Machines'}->{'Attack'};
  }

  method siegeDefenseIncrease() {
    return $buffValues->{'Siege Machines'}->{'Defense'};
  }
  method siegeHpIncrease() { return $buffValues->{'Siege Machines'}->{'HP'}; }

  method overallMarchIncrease() {
    return $buffValues->{'Overall'}->{'March Size Capacity'};
  }

  method overallAttackIncrease() {
    return $buffValues->{'Overall'}->{'Attack'};
  }

  method overallDefenseIncrease() {
    return $buffValues->{'Overall'}->{'Defense'};
  }
  method overallHpIncrease() { return $buffValues->{'Overall'}->{'HP'}; }

  method reducegroundattack() {
    return $debuffValues->{'Ground Troops'}->{'Attack'};
  }

  method reducegrounddefense() {
    return $debuffValues->{'Ground Troops'}->{'Defense'};
  }
  method reducegroundhp() { return $debuffValues->{'Ground Troops'}->{'HP'}; }

  method reducemountedattack() {
    return $debuffValues->{'Mounted Troops'}->{'Attack'};
  }

  method reducemounteddefense() {
    return $debuffValues->{'Mounted Troops'}->{'Defense'};
  }
  method reducemountedhp() { return $debuffValues->{'Mounted Troops'}->{'HP'}; }

  method reducerangedattack() {
    return $debuffValues->{'Ranged Troops'}->{'Attack'};
  }

  method reducerangeddefense() {
    return $debuffValues->{'Ranged Troops'}->{'Defense'};
  }
  method reducerangedhp() { return $debuffValues->{'Ranged Troops'}->{'HP'}; }

  method reducesiegeattack() {
    return $debuffValues->{'Siege Machines'}->{'Attack'};
  }

  method reducesiegedefense() {
    return $debuffValues->{'Siege Machines'}->{'Defense'};
  }
  method reducesiegehp() { return $debuffValues->{'Siege Machines'}->{'HP'}; }

  # Helper method to determine troop type from targetType
  method _getTroopTypeForTarget() {
    if ($targetType =~ /Ground/i) {
      return 'Ground Troops';
    }
    elsif ($targetType =~ /Mounted/i) {
      return 'Mounted Troops';
    }
    elsif ($targetType =~ /Ranged/i) {
      return 'Ranged Troops';
    }
    elsif ($targetType =~ /Siege/i) {
      return 'Siege Machines';
    }
    else {
      return 'Overall';
    }
  }

  # Main update methods
  method updateBuffs() {
    my @troopTypes = (
      'Ground Troops',
      'Mounted Troops',
      'Ranged Troops',
      'Siege Machines',
      'Overall'
    );

    foreach my $troopType (@troopTypes) {
      foreach my $attribute (keys %{ $buffValues->{$troopType} }) {
        $buffValues->{$troopType}->{$attribute} =
          $self->updateBuff($attribute, 'Overall', $troopType);
      }
    }
  }

  method updateDebuffs() {
    foreach my $troopType (keys %$debuffValues) {
      foreach my $attribute (keys %{ $debuffValues->{$troopType} }) {
        $debuffValues->{$troopType}->{$attribute} =
          $self->updateDebuff($attribute, $troopType);
      }
    }
  }

  # Filter buff conditions based on activation type
  method filterBuffConditions($activationType) {
    my @buffConditions = $self->buffConditionValues->values->@*;

    # Create a mapping of activation types to filter patterns
    my %activationFilters = (
      'PvM'         => qr/(Defend|Defense|Reinforc|City)/,
      'Overall'     => qr/(Monsters|Defend|Defense|Reinforc|City|Mayor)/,
      'Attacking'   => qr/(Monsters|Defend|Defense|Reinforc|City|Mayor)/,
      'Reinforcing' =>
        qr/(Monsters|Mayor|Rally|attack|Marching|When the Main)/i,
      'Wall'  => qr/(Monsters|Mayor|Rally|attack|Marching|Reinfor|Outside)/i,
      'Mayor' => qr/(Mayor|In Main City)/,
    );

    if (exists $activationFilters{$activationType}) {
      if ($activationType eq 'Mayor') {
        # For Mayor, we keep only matching conditions
        @buffConditions =
          grep {/$activationFilters{$activationType}/} @buffConditions;
      }
      else {
        # For others, we filter out matching conditions
        @buffConditions =
          grep { !/$activationFilters{$activationType}/ } @buffConditions;
      }
    }
    else {
      $self->logger->warn(
        sprintf('activationType %s is not handled. Using Overall',
          $activationType)
      );
      return $self->filterBuffConditions('Overall');
    }

    return \@buffConditions;
  }

  # Filter debuff conditions based on activation type
  method filterDebuffConditions($activationType) {
    my @debuffConditions = $self->debuffConditionValues->values->@*;

    if ($activationType ne 'PvM') {
      @debuffConditions = grep { $_ ne "Reduces Monster" } @debuffConditions;
    }

    if ($activationType eq 'PvM') {
      @debuffConditions = grep { !/Enemy/ } @debuffConditions;
    }

    return \@debuffConditions;
  }

  # Core buff calculation method
  method updateBuff(
    $attribute,
    $activationType = 'Overall',
    $troopType = 'Overall'
  ) {
    my $total          = 0;
    my $buffConditions = $self->filterBuffConditions($activationType);

    # Add standard skill book value if applicable
    if ($isPrimary) {
      my $standardSkill = $self->getStandardSkillValue($attribute, $troopType);
      $total += $standardSkill;
      $self->logger->debug(
"adding standard skillbook value $standardSkill for attribute $attribute and troop type $troopType."
      );
    }

    # Add buff values from various sources
    $total +=
      $self->summarize_from_sources($attribute, $troopType, $buffConditions);

    $self->logger->info("returning $attribute total for $troopType: $total");
    return $total;
  }

  # Get standard skill book value
  method getStandardSkillValue($attribute, $troopType) {
    if (
      $attribute eq 'March Size Capacity'
      && $rootManager->generalConflictGroupManager->is_book_compatible(
        'March Size Increase',
        $general->name
      )
    ) {
      return 12;
    }
    elsif ($attribute =~ /(Attack|Defense|HP)/) {
      my $tt = $troopType =~ s/ Troops$//r;    # Remove " Troops" suffix
      if (
        $troopType ne 'Overall'
        && $rootManager->generalConflictGroupManager->is_book_compatible(
          "$tt $attribute Increase",
          $general->name
        )
      ) {
        return 25;
      }
    }
    return 0;
  }

  # Core debuff calculation method
  method updateDebuff($attribute, $debuffType, $activationType = 'Overall') {
    my $total = 0;

    if (!$self->allowedBuffActivation->check($activationType)) {
      $self->logger->error(sprintf(
        'activation type must be one of %s not %s.',
        join(", ", $self->allowedBuffActivation->values->@*),
        $activationType,
      ));
      return $total;
    }

    my $buffConditions   = $self->filterBuffConditions($activationType);
    my $debuffConditions = $self->filterDebuffConditions($activationType);

    if (!scalar(@$debuffConditions)) {
      $self->logger->error("Debuff MUST have debuffConditions.");
      return 0;
    }

    # Add debuff values from various sources
    $total +=
      $self->summarize_from_sources($attribute, $debuffType, $buffConditions,
      $debuffConditions);

    $self->logger->info("returning $attribute total: $total");
    return $total;
  }

  # Summarize buff values from all sources
  method summarize_from_sources($attribute, $summaryType, $buffConditions,
    $debuffConditions = []) {
    my $total = 0;

    # Book buffs
    $total += $self->summarize_book_for_attribute($attribute, $summaryType,
      $buffConditions, $debuffConditions);

    # Covenant buffs
    $total += $self->summarize_covenant_for_attribute($attribute, $summaryType,
      $buffConditions, $debuffConditions);

    # Speciality buffs
    $total +=
      $self->summarize_specialities_for_attribute($attribute, $summaryType,
      $buffConditions, $debuffConditions);

    # Ascending attribute buffs (primary only)
    if ($isPrimary) {
      $total += $self->summarize_ascendingAttributes_for_attribute($attribute,
        $summaryType, $buffConditions, $debuffConditions);
    }

    return $total;
  }

  # Source-specific summarization methods
  method summarize_book_for_attribute(
    $attribute,
    $summaryType      = $targetType,
    $buffConditions   = [],
    $debuffConditions = []
  ) {
    my $total = 0;
    $self->logger->debug($general->name
        . " book name: "
        . ($general->builtInBookName // 'undefined'));

    # Ensure book is loaded
    if (not defined $general->builtInBook
      && length($general->builtInBookName) > 0) {
      if (defined $rootManager) {
        $self->logger()
          ->debug("requesting populateBuiltInBook for " . $general->name());
        $general->populateBuiltInBook($rootManager->bookManager);
      }
      else {
        $self->logger->logcroak("No Root Manager Available");
      }
    }

    my $book = $general->builtInBook();
    if ($book) {
      $self->logger->debug("adding buffs for book " . $book->name);
      my $bv = $book->get_buffs($attribute, $summaryType, $buffConditions,
        $debuffConditions);
      $self->logger->trace(sprintf(
        'found %s in %s %s buffs for %s.',
        $bv, $book->name, $attribute, $general->name
      ));
      $total += $bv;
    }
    else {
      $self->logger->error(
        "cannot update total with book " . Data::Printer::np($book));
    }

    $self->logger->debug(sprintf(
      'returning %s as book total for attribute "%s" with "%s" and "%s"',
      $total,                      $attribute,
      join(",", @$buffConditions), join(", ", @$debuffConditions),
    ));
    return $total;
  }

  method summarize_covenant_for_attribute(
    $attribute,
    $summaryType      = $targetType,
    $buffConditions   = [],
    $debuffConditions = []
  ) {
    my $total = 0;

    my $covenant = $rootManager->covenantManager->getCovenant($general->name);
    if ($covenant) {
      $self->logger->debug("Found covenant for "
          . $general->name
          . " now processing at level $covenantLevel for attribute $attribute."
      );

      my $cv = $covenant->get_buffs_at_level(
        $covenantLevel,  $attribute, $summaryType,
        $buffConditions, $debuffConditions
      );
      $self->logger->debug(
"retrieved $cv as total $attribute for level $covenantLevel of covenant for "
          . $general->name);
      $total += $cv;
    }

    $self->logger->debug(sprintf(
      'returning %s as covenant total for attribute "%s" with "%s" and "%s"',
      $total,                      $attribute,
      join(",", @$buffConditions), join(", ", @$debuffConditions),
    ));
    return $total;
  }

  method summarize_specialities_for_attribute(
    $attribute,
    $summaryType      = $targetType,
    $buffConditions   = [],
    $debuffConditions = []
  ) {
    my $total           = 0;
    my @specialityNames = @{ $general->specialityNames };
    my @specialityLevels =
      ($speciality1, $speciality2, $speciality3, $speciality4);

    foreach my $sn_index (0 .. $#specialityNames) {
      my $sn = $specialityNames[$sn_index];
      my $sl = lc($specialityLevels[$sn_index]);
      $self->logger->debug(
        "processing " . $general->name . " $sn at level $sl");

      my $speciality = $rootManager->specialityManager->getSpeciality($sn);
      if ($speciality) {
        $self->logger->debug(
          sprintf('checking %s for %s', $speciality->name, $attribute));
        my $sv = $speciality->get_buffs_at_level($sl, $attribute, $summaryType,
          $buffConditions, $debuffConditions);
        $self->logger->debug("retrieved $sv as total $attribute for level $sl "
            . $speciality->name
            . " as part of "
            . $general->name);
        $total += $sv;
      }
      else {
        $self->logger->error(
          sprintf('cannot retrieve speciality %s for %s', $sn, $general->name));
      }
    }

    $self->logger->debug(sprintf(
      'returning %s as speciality total for attribute %s with "%s" and "%s"',
      $total,                      $attribute,
      join(",", @$buffConditions), join(", ", @$debuffConditions),
    ));
    return $total;
  }

  method summarize_ascendingAttributes_for_attribute(
    $attribute,
    $summaryType      = $targetType,
    $buffConditions   = [],
    $debuffConditions = []
  ) {
    my $total = 0;

    my $aa = $rootManager->ascendingAttributesManager->getAscendingAttributes(
      $general->name);
    if ($aa) {
      $self->logger->debug(
        "retrieved ascendingAttribute buffs for " . $general->name);
      my $av = $aa->get_buffs_at_level(
        $ascendingLevel, $attribute, $summaryType,
        $buffConditions, $debuffConditions
      );
      $self->logger->debug(sprintf(
        '%s Ascending Attributes has %s buffs with total %s at level %s',
        $general->name, $attribute, $av, $ascendingLevel,
      ));
      $total += $av;
    }
    else {
      $self->logger->error(
        "cannot find Ascending Attributes for " . $general->name);
    }

    $self->logger->debug(sprintf(
'returning %s as Ascending Attributes total for attribute "%s" with "%s" and "%s"',
      $total,                      $attribute,
      join(",", @$buffConditions), join(", ", @$debuffConditions),
    ));
    return $total;
  }
  };
1;

__END__

#ABSTRACT: This is a helper class to allow consumers of the class to take arrays of Game::EvonyTKR::Model::Buff objects and summarize them
