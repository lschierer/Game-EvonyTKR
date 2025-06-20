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
  my $debug = 1;

  field $rootManager : param;
  field $general : param;
  field $isPrimary : reader : param      //= 1;
  field $targetType : reader : param     //= '';
  field $ascendingLevel : reader : param //= 'red5';
  field $speciality1 : reader : param    //= 'gold';
  field $speciality2 : reader : param    //= 'gold';
  field $speciality3 : reader : param    //= 'gold';
  field $speciality4 : reader : param    //= 'gold';
  field $generalLevel : reader : param   //= 45;
  field $covenantLevel : reader : param  //= 'Civilization';
  field $includePassive : reader : param //= 1;
  field $keepLevel : reader : param      //= 40;

  # output values
  field $marchIncrease : reader       = 0;
  field $attackIncrease : reader      = 0;
  field $defenseIncrease : reader     = 0;
  field $hpIncrease : reader          = 0;
  field $reducegroundattack : reader  = 0;
  field $reducerangedattack : reader  = 0;
  field $reducemountedattack : reader = 0;
  field $reducesiegeattack : reader   = 0;
  field $reducegroundhp : reader      = 0;

  # some parameters unused currently, but adding them for future growth
  method updateBuffs () {
    $marchIncrease   = $self->updateMarchSize();
    $attackIncrease  = $self->updateAttackIncrease();
    $defenseIncrease = $self->updateDefenseIncrease();
    $hpIncrease      = $self->updateHPIncrease();
  }

  method updateDebuffs () {
    $reducegroundattack = $self->updateAttackDebuff('Ground Troops');
    $reducegroundhp     = $self->updateHPDebuff('Ground Troops');
  }

  # some parameters unused currently, but adding them for future growth
  method updateMarchSize () {
    return $self->updateBuff('March Size Capacity');
  }

  method updateAttackIncrease () {
    return $self->updateBuff('Attack',);
  }

  method updateDefenseIncrease () {
    return $self->updateBuff('Defense',);
  }

  method updateHPIncrease () {
    return $self->updateBuff('HP');
  }

  method updateAttackDebuff ($debuffType, $activationType = 'Overall') {
    return $self->updateDebuff('Attack', $debuffType, $activationType);
  }

  method updateDefenseDebuff ($debuffType, $activationType = 'Overall') {
    return $self->updateDebuff('Defense', $debuffType, $activationType);
  }

  method updateHPDebuff ($debuffType, $activationType = 'Overall') {
    return $self->updateDebuff('HP', $debuffType, $activationType);
  }

  method updateBuff ($attribute, $activationType = 'Overall') {
    # set $total to 0 to ensure this function is idempotent
    my $total = 0;

    my @buffConditions = $self->buffConditionValues->values->@*;
# filter them based on $activationType;
# do *not* use word boundary conditionals to match both words like 'City' and 'SubCity'
    if ($activationType eq 'PvM') {
      @buffConditions =
        grep { !/(Defend|Defense|Reinforc|City)/ } @buffConditions;
    }
    elsif ($activationType eq 'Overall' || $activationType eq 'Attacking') {
      @buffConditions =
        grep { !/(Monsters|Defend|Defense|Reinforc|City|Mayor)/ }
        @buffConditions;
    }
    elsif ($activationType eq 'Reinforcing') {
      @buffConditions =
        grep { !/(Monsters|Mayor|Rally|attack|Marching|When the Main)/i }
        @buffConditions;
    }
    elsif ($activationType eq 'Wall') {
      @buffConditions =
        grep { !/(Monsters|Mayor|Rally|attack|Marching|Reinfor|Outside)/i }
        @buffConditions;
    }
    elsif ($activationType eq 'Mayor') {
      @buffConditions = grep {/(Mayor|In Main City)/} @buffConditions;
    }
    else {
      $self->logger->warn(
        sprintf('activationType %s is not handled. Using Overall',
          $activationType)
      );
      return $self->updateBuff($attribute, 'Overall');
    }

    my $standardSkill = 0;
    if (
      $attribute eq 'March Size Capacity'
      && $rootManager->generalConflictGroupManager->is_book_compatible(
        'March Size Increase',
        $general->name
      )
    ) {
      $standardSkill = 12;
    }
    elsif ($attribute =~ /(Attack|Defense|HP)/) {
      for my $tt (qw(Mounted Ground Ranged Siege)) {
        if (
          $targetType =~ /$tt/i
          && $rootManager->generalConflictGroupManager->is_book_compatible(
            "$tt $attribute Increase",
            $general->name
          )
        ) {
          $standardSkill = 25;
        }
      }
    }
    if ($isPrimary) {
      $self->logger->debug(
"adding standard skillbook value $standardSkill for attribute $attribute."
      );
      $total += $standardSkill;
    }

    my $bv = $self->summarize_book_for_attribute($attribute, $targetType,
      \@buffConditions);
    $total += $bv;

    my $cv = $self->summarize_covenant_for_attribute($attribute, $targetType,
      \@buffConditions);
    $total += $cv;

    my $sv =
      $self->summarize_specialities_for_attribute($attribute, $targetType,
      \@buffConditions);
    $total += $sv;

    if ($isPrimary) {
      my $av = $self->summarize_ascendingAttributes_for_attribute($attribute,
        $targetType, \@buffConditions);
      $total += $av;
    }
    $self->logger->info("returning $attribute total: $total");
    return $total;
  }

  method updateDebuff ($attribute, $debuffType, $activationType = 'Overall') {
    my $total = 0;

    if (!$self->allowedBuffActivation->check($activationType)) {
      $self->logger->error(sprintf(
        'activation type must be one of %s not %s.',
        join(", ", $self->allowedBuffActivation->values->@*),
        $activationType,
      ));
      return $total;
    }

    for my $tt (qw(Mounted Ground Ranged Siege)) {
      if (
           $isPrimary
        && $debuffType =~ /$tt/i
        && $rootManager->generalConflictGroupManager->is_book_compatible(
          "Enemy $tt $attribute",
          $general->name
        )
      ) {
        $total += 0;    # see note in README.
      }
    }

    my @buffConditions   = $self->buffConditionValues->values->@*;
    my @debuffConditions = $self->debuffConditionValues->values->@*;
# filter them based on $activationType;
# do *not* use word boundary conditionals to match both words like 'City' and 'SubCity'
    if ($activationType eq 'PvM') {
      @buffConditions =
        grep { !/(Defend|Defense|Reinforc|City)/ } @buffConditions;
      @debuffConditions = grep { !/Enemy/ } @debuffConditions;
    }
    elsif ($activationType eq 'Overall' || $activationType eq 'Attacking') {
      @buffConditions =
        grep { !/(Monsters|Defend|Defense|Reinforc|City|Mayor)/ }
        @buffConditions;
      @debuffConditions = grep { $_ ne "Reduces Monster" } @debuffConditions;
    }
    elsif ($activationType eq 'Reinforcing') {
      @buffConditions =
        grep { !/(Monsters|Mayor|Rally|attack|Marching|When the Main)/i }
        @buffConditions;
      @debuffConditions = grep { $_ ne "Reduces Monster" } @debuffConditions;
    }
    elsif ($activationType eq 'Wall') {
      @buffConditions =
        grep { !/(Monsters|Mayor|Rally|attack|Marching|Reinfor|Outside)/i }
        @buffConditions;
      @debuffConditions = grep { $_ ne "Reduces Monster" } @debuffConditions;
    }
    elsif ($activationType eq 'Mayor') {
      @buffConditions   = grep {/(Mayor|In Main City)/} @buffConditions;
      @debuffConditions = grep { $_ ne "Reduces Monster" } @debuffConditions;
    }
    else {
      $self->logger->warn(
        sprintf('activationType %s is not handled. Using Overall',
          $activationType)
      );
      return $self->updateAttackDebuff($debuffType, 'Overall');
    }

    if (!scalar(@debuffConditions)) {
      $self->logger->error("Debuff MUST have debuffConditions.");
      return 0;
    }

    my $bv = $self->summarize_book_for_attribute($attribute, $debuffType,
      \@buffConditions, \@debuffConditions);
    $total += $bv;

    my $cv = $self->summarize_covenant_for_attribute($attribute, $debuffType,
      \@buffConditions, \@debuffConditions);
    $total += $cv;

    my $sv =
      $self->summarize_specialities_for_attribute($attribute, $debuffType,
      \@buffConditions, \@debuffConditions);
    $total += $sv;

    if ($isPrimary) {
      my $av = $self->summarize_ascendingAttributes_for_attribute($attribute,
        $debuffType, \@buffConditions, \@debuffConditions);
      $total += $av;
    }

    $self->logger->info("returning $attribute total: $total");

    return $total;
  }

  method summarize_book_for_attribute (
    $attribute,
    $summaryType      = $targetType,
    $buffConditions   = [],
    $debuffConditions = []
  ) {
    my $total = 0;
    $self->logger->debug($general->name
        . " book name: "
        . ($general->builtInBookName // 'undefined'));

    if (not defined $general->builtInBook
      && length($general->builtInBookName) > 0) {
      $self->logger->trace("Root manager book manager: "
          . (defined $rootManager->bookManager ? 'defined' : 'undefined'));
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
      $total,                         $attribute,
      join(",", $buffConditions->@*), join(", ", $debuffConditions->@*),
    ));
    return $total;
  }

  method summarize_covenant_for_attribute (
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
      'returning %s as convenant total for attribute "%s" with "%s" and "%s"',
      $total,                         $attribute,
      join(",", $buffConditions->@*), join(", ", $debuffConditions->@*),
    ));
    return $total;
  }

  method summarize_specialities_for_attribute (
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
        $self->logger->error(sprintf(
          'cannot retrieve speciality %s for %s', $sn, $general->name,
        ));
      }
    }
    $self->logger->debug(sprintf(
      'returning %s as speciality total for attribute %s with "%s" and "%s"',
      $total,                         $attribute,
      join(",", $buffConditions->@*), join(", ", $debuffConditions->@*),
    ));
    return $total;
  }

  method summarize_ascendingAttributes_for_attribute (
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
      $total,                         $attribute,
      join(",", $buffConditions->@*), join(", ", $debuffConditions->@*),
    ));
    return $total;
  }

  };
1;

__END__

#ABSTRACT: This is a helper class to allow consumers of the class to take arrays of Game::EvonyTKR::Model::Buff objects and summarize them
