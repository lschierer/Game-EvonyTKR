use v5.42.0;
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require Game::EvonyTKR::Model::Buff::Value;
require Game::EvonyTKR::Model::General::Conflict::Book;
require JSON::PP;

package Game::EvonyTKR::Model::Buff::Summarizer {
  use Mojo::Base 'Game::EvonyTKR::Model::Base',                    -signatures;
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::BuffConstants', -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::GeneralConstants', -role;
  use List::AllUtils qw(first any all none uniq);
  use Carp;
  use overload
    '""'       => \&as_string,
    '.'        => \&concat,
    'fallback' => 0;

  our $VERSION = 'v0.30.0';

  has 'bc' => sub { Game::EvonyTKR::Model::General::Conflict::Book->new() };

  # Input parameters
  has 'general';
  has 'books' => sub { [] };
  has 'covenant';
  has 'ascendingAttributes';
  has 'isPrimary'      => 1;
  has 'targetType'     => '';
  has 'activationType' => 'Overall';
  has 'ascendingLevel' => 'red5';
  has 'specialty1'     => 'gold';
  has 'specialty2'     => 'gold';
  has 'specialty3'     => 'gold';
  has 'specialty4'     => 'gold';
  has 'covenantLevel'  => 'Civilization';
  has 'generalLevel'   => 45;
  has 'includePassive' => 1;
  has 'keepLevel'      => 40;

  # Output values
  has 'buffValues' => sub {
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

  has 'debuffValues' => sub {
    {
      'Ground Troops'  => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Mounted Troops' => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Ranged Troops'  => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Siege Machines' => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Overall'        => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
    };
  };

  sub updateBuffs ($self) {
    $self->logger->info(sprintf(
      'updateBuffs called for %s with isPrimary "%s" '
        . 'targetType "%s" activationType "%s", general set to %s %s %s %s %s %s',
      $self->general->name,  $self->isPrimary,      $self->targetType,
      $self->activationType, $self->ascendingLevel, $self->specialty1,
      $self->specialty2,     $self->specialty3,     $self->specialty4,
      $self->covenantLevel,
    ));

    if (!$self->general->can_afford_ascending_level($self->ascendingLevel)) {
      $self->logger->warn("requsted level '"
          . $self->ascendingLevel
          . "' is higher than "
          . $self->general->stars);
      $self->ascendingLevel('none');
    }

    foreach my $troopType (keys %{ $self->buffValues }) {
      foreach my $attribute (keys %{ $self->buffValues->{$troopType} }) {
        $self->buffValues->{$troopType}->{$attribute} =
          $self->updateBuff($attribute, $troopType);
      }
    }
    $self->logger->info("returning buffs for "
        . $self->general->name
        . Data::Printer::np($self->buffValues));
  }

  sub updateDebuffs ($self) {
    if (!$self->general) {
      $self->logger->error("NO GENERAL ASSIGNED FOR " . blessed($self));
      return;
    }
    $self->logger->info(sprintf(
      'updateDebuffs called for %s with isPrimary "%s" '
        . 'targetType "%s" activationType "%s", general set to %s %s %s %s %s %s',
      $self->general->name,  $self->isPrimary,      $self->targetType,
      $self->activationType, $self->ascendingLevel, $self->specialty1,
      $self->specialty2,     $self->specialty3,     $self->specialty4,
      $self->covenantLevel,
    ));

    if (!$self->general->can_afford_ascending_level($self->ascendingLevel)) {
      $self->ascendingLevel('none');
    }

    foreach my $troopType (keys %{ $self->debuffValues }) {
      foreach my $attribute (keys %{ $self->debuffValues->{$troopType} }) {
        $self->debuffValues->{$troopType}->{$attribute} =
          $self->updateDebuff($attribute, $troopType);
      }
    }
    $self->logger->info("returning debuffs for"
        . $self->general->name
        . Data::Printer::np($self->debuffValues));
  }

  sub filterBuffConditions ($self) {
    my @buffConditions = keys %{ $self->BuffConditionValues };

    my %activationFilters = (
      'PvM' => [
        'Against Monsters',
        'Attacking',
        'Marching',
        'When Rallying',
        'brings a dragon',
        'brings a spiritual beast',
        'brings dragon or beast to attack',
        'dragon to the attack',
        'leading the army to attack',
        'leading the army',
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
        'Marching',
        'When Rallying',
        'brings a dragon',
        'brings a spiritual beast',
        'brings dragon or beast to attack',
        'dragon to the attack',
        'leading the army to attack',
        'leading the army',
        'you own the General',
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

    if (exists $activationFilters{ $self->activationType }) {
      my $allowed  = $activationFilters{ $self->activationType };
      my %allowed  = map  { $_ => 1 } @$allowed;
      my @filtered = grep { $allowed{$_} } @buffConditions;

      $self->logger->debug("Filtering buff conditions for "
          . $self->activationType
          . ": allowed = ["
          . join(', ', @$allowed)
          . "] → result = ["
          . join(', ', @filtered)
          . "]");

      return \@filtered;
    }
    else {
      $self->logger->warn(
        sprintf('activationType %s is not handled. Using Overall',
          $self->activationType)
      );
      $self->activationType('Overall');
      return $self->filterBuffConditions();
    }
  }

  sub filterDebuffConditions ($self) {
    my @debuffConditions = @{ $self->DebuffConditionValues };

    if ($self->activationType ne 'PvM') {
      @debuffConditions = ("Enemy");
    }

    if ($self->activationType eq 'PvM') {
      @debuffConditions = ("Monsters");
    }

    return \@debuffConditions;
  }

  sub updateBuff ($self, $attribute, $buffType) {
    my $total          = 0;
    my $buffConditions = $self->filterBuffConditions();

    if ($self->isPrimary) {
      my $standardSkill = $self->getStandardSkillValue($attribute, $buffType);
      $total += $standardSkill;
      $self->logger->debug(sprintf(
        'adding standard skillbook value %s '.
        'for attribute %s  and buff type %s',
        $standardSkill, $attribute, $buffType
      ));
    }

    $total +=
      $self->summarize_from_sources($attribute, $buffType, $buffConditions);

    $self->logger->debug("returning $attribute total for $buffType: $total");
    return $total;
  }

  sub getStandardSkillValue ($self, $attribute, $troopType) {
    my $total = 0;
    my $tt    = $troopType =~ s/ Troops$//r;

    if ($attribute eq 'March Size') {
      my $MS = first { $_->name =~ /March Size/ } $self->books->@*;
      if (
        defined($MS)
        && $self->bc->is_general_and_book_compatible(
          $self->general, $MS, { same_side => 1, }
        )
      ) {
        $total += 12;
      }
    }
    elsif ($attribute =~ /(Attack|Defense|HP)/) {
      if ($troopType ne 'Overall') {
        my $btt = $tt;
        $btt =~ s/(Ranged|Ground|Mounted)/$1 Troop/;
        $btt =~ s/Siege Machines/Siege Machine/;
        my $book = first { $_->name =~ /^$btt $attribute$/ && $_->level == 4 }
          $self->books->@*;

        if (
          $book
          && $self->bc->is_general_and_book_compatible(
            $self->general, $book, { same_side => 1, }
          )
        ) {
          $total += 25;
        }
        elsif (!defined($book)) {
          $self->logger->error(sprintf(
            'no book found for "%s" from %s',
            "Level 4 $btt $attribute",
            join ', ', map { sprintf('"%s"', $_->name) } $self->books->@*
          ));
        }
      }
    }

    if ($self->activationType eq 'PvM') {
      if ($attribute =~ /(Attack|Defense|HP)/) {
        if ($troopType ne 'Overall') {
          my $btt = $tt =~ s/(Ranged|Ground|Mounted)/$1 Troop/r;
          $btt = $tt =~ s/Siege Machines/Siege Machine/r;
          my $book =
            first {
            $_->name =~ /$btt $attribute Against Monster/ && $_->level == 4
            } $self->books->@*;

          if (
            $book
            && $self->bc->is_general_and_book_compatible(
              $self->general, $book, { same_side => 1, }
            )
          ) {
            $total += 45;
          }
          elsif (!defined($book)) {
            $self->logger->error(sprintf('no book found for %s',
              "Level 4 $btt $attribute Against Monster"));
          }
        }
      }
    }
    return $total;
  }

  sub updateDebuff ($self, $attribute, $debuffType) {
    my $total            = 0;
    my $buffConditions   = $self->filterBuffConditions();
    my $debuffConditions = $self->filterDebuffConditions();

    if (!scalar(@$debuffConditions)) {
      $self->logger->error("Debuff MUST have debuffConditions.");
      return 0;
    }

    $total +=
      $self->summarize_from_sources($attribute, $debuffType, $buffConditions,
      $debuffConditions);

    $self->logger->debug("returning $attribute total for $debuffType: $total");
    return $total;
  }

  sub summarize_from_sources ($self, $attribute, $summaryType, $buffConditions,
    $debuffConditions = []) {
    my $total         = 0;
    my $matching_type = (scalar @$debuffConditions > 0) ? 'debuff' : 'buff';

    $total += $self->summarize_book_for_attribute(
      $attribute,        $summaryType, $buffConditions,
      $debuffConditions, $matching_type
    );

    $self->logger->info(
          "summarize_from_sources has $total after summarize_book "
        . "for $attribute/$summaryType");

    $total += $self->summarize_covenant_for_attribute(
      $attribute,        $summaryType, $buffConditions,
      $debuffConditions, $matching_type
    );

    $self->logger->info(sprintf(
    'summarize_from_sources has %s after '.'summarize_covenant for %s/%s',
    $total, $attribute, $summaryType));

    $total += $self->summarize_specialties_for_attribute(
      $attribute,        $summaryType, $buffConditions,
      $debuffConditions, $matching_type
    );

    $self->logger->info(sprintf(
    'summarize_from_sources has %s after '.'summarize_specialties for %s/%s',
    $total, $attribute, $summaryType));

    if ($self->isPrimary && $self->general->ascending) {
      $total += $self->summarize_ascendingAttributes_for_attribute(
        $attribute,        $summaryType, $buffConditions,
        $debuffConditions, $matching_type
      );

      $self->logger->info(sprintf(
      'summarize_from_sources has %s after '.'summarize_ascendingAttributes for %s/%s',
      $total, $attribute, $summaryType));
    }

    return $total;
  }

  sub summarize_book_for_attribute (
    $self, $attribute,
    $summaryType      = undef,
    $buffConditions   = [],
    $debuffConditions = [],
    $matching_type    = 'buff'
  ) {
    $summaryType //= $self->targetType;
    my $total = 0;
    $self->logger->debug($self->general->name
        . " book name: "
        . ($self->general->builtInBookName // 'undefined'));

    if (not defined $self->general->builtInBook
      && length($self->general->builtInBookName) > 0) {
      $self->logger->error('Book must be loaded first!!');
      return;
    }

    my $book = $self->general->builtInBook();
    if ($book) {
      $self->logger->debug("adding buffs for book " . $book->name);
      my $bv = $book->get_buffs(
        $attribute,      $matching_type, $summaryType,
        $buffConditions, $debuffConditions
      );
      $self->logger->debug(sprintf(
        'found %s in %s %s buffs for %s.',
        $bv, $book->name, $attribute, $self->general->name
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

  sub summarize_covenant_for_attribute (
    $self, $attribute,
    $summaryType      = undef,
    $buffConditions   = [],
    $debuffConditions = [],
    $matching_type    = 'buff'
  ) {
    $summaryType //= $self->targetType;
    my $total = 0;

    if ($self->covenant) {
      $self->logger->debug("Found covenant for "
          . $self->general->name
          . " now processing at level "
          . $self->covenantLevel
          . " for attribute $attribute.");

      my $cv =
        $self->covenant->get_buffs_at_level($self->covenantLevel, $attribute,
        $matching_type, $summaryType, $buffConditions, $debuffConditions);
      $self->logger->debug(sprintf(
        'retrieved %s as total %s for level %s of covenant for %s',
        $cv, $attribute, $self->covenantLevel, $self->general->name
      ));
      $total += $cv;
    }

    $self->logger->debug(sprintf(
      'returning %s as covenant total for attribute "%s" with "%s" and "%s"',
      $total,                      $attribute,
      join(",", @$buffConditions), join(", ", @$debuffConditions),
    ));
    return $total;
  }

  sub summarize_specialties_for_attribute (
    $self, $attribute,
    $summaryType      = undef,
    $buffConditions   = [],
    $debuffConditions = [],
    $matching_type    = 'buff'
  ) {
    $summaryType //= $self->targetType;
    my $total           = 0;
    my @specialtyNames  = @{ $self->general->specialtyNames };
    my @specialtyLevels = (
      $self->specialty1, $self->specialty2,
      $self->specialty3, $self->specialty4
    );
    $self->logger->debug(sprintf(
      '%s summarize_specialties_for_attribute called for "%s" looking for levels '
        . 'sp1: "%s"; sp2: "%s"; sp3: "%s"; sp4: "%s";',
      $matching_type, $self->general->name, $self->specialty1, $self->specialty2,
      $self->specialty3,    $self->specialty4
    ));
    $self->logger->debug(sprintf(
      '%s summarize_specialties_for_attribute for "%s" attribute: "%s"; summaryType: "%s"',
      $matching_type, $self->general->name, $attribute, $summaryType));

    foreach my $sn_index (0 .. $#specialtyNames) {
      my $sn = $specialtyNames[$sn_index];
      my $sl = lc($specialtyLevels[$sn_index]);
      $self->logger->debug(
        "processing " . $self->general->name . " $sn at level $sl");

      my $specialty = $self->general->specialties->[$sn_index];
      if ($specialty) {
        $self->logger->debug(
          sprintf('checking %s for %s', $specialty->name, $attribute));
        my $sv = $specialty->get_buffs_at_level($sl, $attribute, $matching_type,
          $summaryType, $buffConditions, $debuffConditions);
        $self->logger->debug("retrieved $sv as total $attribute for level $sl "
            . $specialty->name
            . " as part of "
            . $self->general->name);
        $total += $sv;
      }
      else {
        $self->logger->error(sprintf(
          'cannot retrieve specialty %s for %s',
          $sn, $self->general->name
        ));
      }
    }

    $self->logger->debug(sprintf(
      'returning %s as specialty total for attribute %s with "%s" and "%s"',
      $total,                      $attribute,
      join(",", @$buffConditions), join(", ", @$debuffConditions),
    ));
    return $total;
  }

  sub summarize_ascendingAttributes_for_attribute (
    $self, $attribute,
    $summaryType      = undef,
    $buffConditions   = [],
    $debuffConditions = [],
    $matching_type    = 'buff'
  ) {
    $summaryType //= $self->targetType;
    my $total = 0;

    my $aa = $self->ascendingAttributes;
    if ($aa) {
      $self->logger->debug(
        "retrieved ascendingAttribute buffs for " . $self->general->name);
      my $av =
        $aa->get_buffs_at_level($self->ascendingLevel, $attribute, $summaryType,
        $buffConditions, $debuffConditions, $matching_type);
      $self->logger->debug(sprintf(
        '%s Ascending Attributes has %s buffs with total %s at level %s',
        $self->general->name, $attribute, $av, $self->ascendingLevel,
      ));
      $total += $av;
    }
    else {
      $self->logger->error(
        "cannot find Ascending Attributes for " . $self->general->name);
    }

    $self->logger->debug(sprintf(
      'returning %s as Ascending Attributes total for '
        . 'attribute "%s" with "%s" and "%s"',
      $total,                      $attribute,
      join(",", @$buffConditions), join(", ", @$debuffConditions),
    ));
    return $total;
  }
}

1;

__END__

#ABSTRACT: This is a helper class to allow consumers of the class to take arrays of Game::EvonyTKR::Model::Buff objects and summarize them
