use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require Game::EvonyTKR::Model::Buff::Value;
require JSON::PP;

class Game::EvonyTKR::Model::Buff::Summarizer :
  isa(Game::EvonyTKR::Shared::Constants) {
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
  field $isPrimary : reader : param      //= 1;
  field $targetType : reader : param     //= '';
  field $activationType : reader : param //= 'Overall';

  # these are needed now
  field $ascendingLevel : reader : param //= 'red5';
  field $specialty1 : reader : param     //= 'gold';
  field $specialty2 : reader : param     //= 'gold';
  field $specialty3 : reader : param     //= 'gold';
  field $specialty4 : reader : param     //= 'gold';
  field $covenantLevel : reader : param  //= 'Civilization';

  # these are anticipated that I will need them in the future.
  field $generalLevel : reader : param   //= 45;
  field $includePassive : reader : param //= 1;
  field $keepLevel : reader : param      //= 40;

# Output values - stored in hashes for easier access
  field $buffValues : reader = {
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

  field $debuffValues : reader = {
    'Ground Troops'  => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
    'Mounted Troops' => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
    'Ranged Troops'  => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
    'Siege Machines' => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
    'Overall'        => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
  };

  method getBuffForTypeAndKey ($tt, $key) {
    # normalize $tt
    $tt = $self->_getTroopTypeForTarget($tt);
    if (exists $buffValues->{$tt}) {
      if (exists $buffValues->{$tt}->{$key}) {
        return $buffValues->{$tt}->{$key};
      }
      $self->logger->error("$key is not a valid key for buffValues at $tt");
    }
    $self->logger->error("$tt is not a valid troop type for buffValues");
    return 0;
  }

  method getDebuffForTypeAndKey ($tt, $key) {
    # normalize $tt
    $tt = $self->_getTroopTypeForTarget($key);
    if (exists $debuffValues->{$tt}) {
      if (exists $debuffValues->{$tt}->{$key}) {
        return $debuffValues->{$tt}->{$key};
      }
      $self->logger->error("$key is not a valid key for debuffValues at $tt");
    }
    $self->logger->error("$tt is not a valid troop type for debuffValues");
    return 0;
  }

  # Helper method to determine troop type from targetType
  method _getTroopTypeForTarget($tt = $targetType) {
    if ($tt =~ /Ground/i) {
      return 'Ground Troops';
    }
    elsif ($tt =~ /Mounted/i) {
      return 'Mounted Troops';
    }
    elsif ($tt =~ /Ranged/i) {
      return 'Ranged Troops';
    }
    elsif ($tt =~ /Siege/i) {
      return 'Siege Machines';
    }
    else {
      return 'Overall';
    }
  }

  # Main update methods
  method updateBuffs() {

    $self->logger->info(sprintf(
'updateBuffs called for %s with isPrimary "%s" targetType "%s" activationType "%s", general set to %s %s %s %s %s %s',
      $general->name,  $isPrimary,  $targetType, $activationType,
      $ascendingLevel, $specialty1, $specialty2, $specialty3,
      $specialty4,     $covenantLevel,
    ));

    foreach my $troopType (keys %$buffValues) {
      foreach my $attribute (keys %{ $buffValues->{$troopType} }) {
        $buffValues->{$troopType}->{$attribute} =
          $self->updateBuff($attribute, $troopType);
      }
    }
    $self->logger->info(
      "returning buffs for " . $general->name . Data::Printer::np($buffValues));
  }

  method updateDebuffs() {
    if (!$general) {
      $self->logger->logcroak("NO GENERAL ASSIGNED FOR " . blessed($self));
    }
    $self->logger->info(sprintf(
'updateDebuffs called for %s with isPrimary "%s" targetType "%s" activationType "%s", general set to %s %s %s %s %s %s',
      $general->name,  $isPrimary,  $targetType, $activationType,
      $ascendingLevel, $specialty1, $specialty2, $specialty3,
      $specialty4,     $covenantLevel,
    ));

    foreach my $troopType (keys %$debuffValues) {
      foreach my $attribute (keys %{ $debuffValues->{$troopType} }) {
        $debuffValues->{$troopType}->{$attribute} =
          $self->updateDebuff($attribute, $troopType);
      }
    }
    $self->logger->info("returning debuffs for"
        . $general->name
        . Data::Printer::np($debuffValues));
  }

  # Filter buff conditions based on activation type
  method filterBuffConditions() {
    my @buffConditions = keys %{ $self->BuffConditionValues };

    # Create a mapping of activation types to filter patterns
    my %activationFilters = (
      'PvM' => [
        'Against Monsters',
        'Attacking',
        'brings a dragon',
        'brings a spiritual beast',
        'leading the army',
        'Marching',
        'When Rallying',
        'you own the General',
      ],
      'Overall' => [
        'brings a dragon',
        'brings a spiritual beast',
        'Marching',
        'When Rallying',
        'you own the General',
        'leading the army',
      ],
      'Attacking' => [
        'Attacking',
        'brings a dragon',
        'brings a spiritual beast',
        'brings dragon or beast to attack',
        'dragon to the attack',
        'leading the army to attack',
        'Marching',
        'When Rallying',
        'you own the General',
        'leading the army',
      ],
      'Reinforcing' => [
        'brings a dragon',
        'brings a spiritual beast',
        'Defending',
        'Marching',
        'Reinforcing',
        'When Defending Outside The Main City',
        'In Main City',
        'In City',
        'you own the General',
        'leading the army',
      ],
      'Wall' => [
        'brings a dragon',
        'brings a spiritual beast',
        'Defending',
        'When City Mayor for this SubCity',
        'In Main City',
        'In City',
        'When the Main Defense General',
        'you own the General',
        'leading the army',
      ],
      'Mayor' => [
        'When City Mayor for this SubCity',
        'In Main City', 'In City',
        'you own the General',
        'leading the army',
      ],
    );

    if (exists $activationFilters{$activationType}) {
      my $allowed = $activationFilters{$activationType};

      my %allowed = map { $_ => 1 } @$allowed;

      my @filtered = grep { $allowed{$_} } @buffConditions;

      $self->logger->debug(
            "Filtering buff conditions for $activationType: allowed = ["
          . join(', ', @$allowed)
          . "] → result = ["
          . join(', ', @filtered)
          . "]");

      return \@filtered;
    }
    else {
      $self->logger->warn(
        sprintf('activationType %s is not handled. Using Overall',
          $activationType)
      );
      $activationType = 'Overall';
      return $self->filterBuffConditions();
    }
  }

  # Filter debuff conditions based on activation type
  method filterDebuffConditions() {
    my @debuffConditions = @{ $self->DebuffConditionValues };

    if ($activationType ne 'PvM') {
      @debuffConditions = ("Enemy");
    }

    if ($activationType eq 'PvM') {
      @debuffConditions = ("Monsters");
    }

    return \@debuffConditions;
  }

  # Core buff calculation method
  method updateBuff($attribute, $buffType) {

    my $total          = 0;
    my $buffConditions = $self->filterBuffConditions();

    # Add standard skill book value if applicable
    if ($isPrimary) {
      my $standardSkill = $self->getStandardSkillValue($attribute, $buffType);
      $total += $standardSkill;
      $self->logger->debug(
"adding standard skillbook value $standardSkill for attribute $attribute and buff type $buffType."
      );
    }

    # Add buff values from various sources
    $total +=
      $self->summarize_from_sources($attribute, $buffType, $buffConditions);

    $self->logger->debug("returning $attribute total for $buffType: $total");
    return $total;
  }

  # Get standard skill book value
  method getStandardSkillValue($attribute, $troopType) {
    my $total = 0;
    my $tt    = $troopType =~ s/ Troops$//r;    # Remove " Troops" suffix
    if (
      $attribute eq 'March Size'
      && $rootManager->generalConflictGroupManager->is_book_compatible(
        'March Size Increase',
        $general->name
      )
    ) {
      $total += 12;
    }
    elsif ($attribute =~ /(Attack|Defense|HP)/) {

      if (
        $troopType ne 'Overall'
        && $rootManager->generalConflictGroupManager->is_book_compatible(
          "$tt $attribute Increase",
          $general->name
        )
      ) {
        $total += 25;
      }
    }
    if ($activationType eq 'PvM') {
      if ($attribute =~ /(Attack|Defense|HP)/) {
        if ($troopType ne 'Overall') {
          if ($rootManager->generalConflictGroupManager->is_book_compatible(
            "Monster $tt $attribute Increase",
            $general->name
          )) {
            $total += 45;
          }
        }
      }
    }
    return $total;
  }

  # Core debuff calculation method
  method updateDebuff($attribute, $debuffType) {
    my $total = 0;

    my $buffConditions   = $self->filterBuffConditions();
    my $debuffConditions = $self->filterDebuffConditions();

    if (!scalar(@$debuffConditions)) {
      $self->logger->error("Debuff MUST have debuffConditions.");
      return 0;
    }

    # Add debuff values from various sources
    $total +=
      $self->summarize_from_sources($attribute, $debuffType, $buffConditions,
      $debuffConditions);

    $self->logger->debug("returning $attribute total for $debuffType: $total");
    return $total;
  }

  # Summarize buff values from all sources
  method summarize_from_sources($attribute, $summaryType, $buffConditions,
    $debuffConditions = []) {
    my $total = 0;

# Determine if we're doing buff or debuff matching based on whether debuffConditions were passed
    my $matching_type = (scalar @$debuffConditions > 0) ? 'debuff' : 'buff';

    # Book buffs
    $total += $self->summarize_book_for_attribute(
      $attribute,        $summaryType, $buffConditions,
      $debuffConditions, $matching_type
    );

    $self->logger->info(
"summarize_from_sources has $total after summarize_book for $attribute/$summaryType"
    );

    # Covenant buffs
    $total += $self->summarize_covenant_for_attribute(
      $attribute,        $summaryType, $buffConditions,
      $debuffConditions, $matching_type
    );

    $self->logger->info(
"summarize_from_sources has $total after summarize_covenant for $attribute/$summaryType"
    );

    # Specialty buffs
    $total += $self->summarize_specialties_for_attribute(
      $attribute,        $summaryType, $buffConditions,
      $debuffConditions, $matching_type
    );

    $self->logger->info(
"summarize_from_sources has $total after summarize_specialties for $attribute/$summaryType"
    );

    # Ascending attribute buffs (primary only)
    if ($isPrimary && $general->ascending) {
      $total += $self->summarize_ascendingAttributes_for_attribute(
        $attribute,        $summaryType, $buffConditions,
        $debuffConditions, $matching_type
      );

      $self->logger->info(
"summarize_from_sources has $total after summarize_ascendingAttributes for $attribute/$summaryType"
      );
    }

    return $total;
  }

  # Source-specific summarization methods
  method summarize_book_for_attribute(
    $attribute,
    $summaryType      = $targetType,
    $buffConditions   = [],
    $debuffConditions = [],
    $matching_type    = 'buff'
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
      my $bv = $book->get_buffs(
        $attribute,      $matching_type, $summaryType,
        $buffConditions, $debuffConditions
      );
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
    $debuffConditions = [],
    $matching_type    = 'buff'
  ) {
    my $total = 0;

    my $covenant = $rootManager->covenantManager->getCovenant($general->name);
    if ($covenant) {
      $self->logger->debug("Found covenant for "
          . $general->name
          . " now processing at level $covenantLevel for attribute $attribute."
      );

      my $cv = $covenant->get_buffs_at_level(
        $covenantLevel, $attribute,      $matching_type,
        $summaryType,   $buffConditions, $debuffConditions
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

  method summarize_specialties_for_attribute(
    $attribute,
    $summaryType      = $targetType,
    $buffConditions   = [],
    $debuffConditions = [],
    $matching_type    = 'buff'
  ) {
    my $total           = 0;
    my @specialtyNames  = @{ $general->specialtyNames };
    my @specialtyLevels = ($specialty1, $specialty2, $specialty3, $specialty4);

    foreach my $sn_index (0 .. $#specialtyNames) {
      my $sn = $specialtyNames[$sn_index];
      my $sl = lc($specialtyLevels[$sn_index]);
      $self->logger->debug(
        "processing " . $general->name . " $sn at level $sl");

      my $specialty = $rootManager->specialtyManager->getSpecialty($sn);
      if ($specialty) {
        $self->logger->debug(
          sprintf('checking %s for %s', $specialty->name, $attribute));
        my $sv = $specialty->get_buffs_at_level($sl, $attribute, $matching_type,
          $summaryType, $buffConditions, $debuffConditions);
        $self->logger->debug("retrieved $sv as total $attribute for level $sl "
            . $specialty->name
            . " as part of "
            . $general->name);
        $total += $sv;
      }
      else {
        $self->logger->error(
          sprintf('cannot retrieve specialty %s for %s', $sn, $general->name));
      }
    }

    $self->logger->debug(sprintf(
      'returning %s as specialty total for attribute %s with "%s" and "%s"',
      $total,                      $attribute,
      join(",", @$buffConditions), join(", ", @$debuffConditions),
    ));
    return $total;
  }

  method summarize_ascendingAttributes_for_attribute(
    $attribute,
    $summaryType      = $targetType,
    $buffConditions   = [],
    $debuffConditions = [],
    $matching_type    = 'buff'
  ) {
    my $total = 0;

    my $aa = $rootManager->ascendingAttributesManager->getAscendingAttributes(
      $general->name);
    if ($aa) {
      $self->logger->debug(
        "retrieved ascendingAttribute buffs for " . $general->name);
      my $av = $aa->get_buffs_at_level(
        $ascendingLevel, $attribute,        $summaryType,
        $buffConditions, $debuffConditions, $matching_type
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
