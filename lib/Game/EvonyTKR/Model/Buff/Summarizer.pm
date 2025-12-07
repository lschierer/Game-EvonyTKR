use v5.42.0;
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require Game::EvonyTKR::Model::Buff::Value;
require Game::EvonyTKR::Service::Conflicts;
require JSON::PP;

package Game::EvonyTKR::Model::Buff::Summarizer {
  use Mojo::Base 'Game::EvonyTKR::Model::Base',                    -signatures;
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::BuffConstants', -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::GeneralConstants', -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::Books',            -role;
  use List::AllUtils qw(first any all none uniq);
  use Carp;
  use diagnostics;

  our $VERSION = 'v0.30.0';

  has bc => sub {
    Game::EvonyTKR::Service::Conflicts->new(
      build_index      => 1,
      asst_has_dragon  => 1,
      asst_has_spirit  => 1,
      allow_wall_buffs => 1,
    );
  };

  # Private instance state (not exposed externally)
  has '_private' => sub { {} };

  # Optional books parameter for testing
  has 'books' => sub {undef};

  # Input parameters
  has 'general';
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

  sub inflate ($self) {
    # Return early if already inflated (per-instance guard)
    return 1 if $self->_private->{is_inflated};

    unless (defined($self->general)
      && ref($self->general)
      && blessed($self->general)
      && $self->general->isa('Game::EvonyTKR::Model::General')) {
      $self->log_error(sprintf('%s requires a general', __PACKAGE__));
      return 0;
    }

    if ($self->general->ascending) {
      $self->general->populateAscendingAttributes();
      unless (
           $self->general->ascendingAttributes
        && ref($self->general->ascendingAttributes)
        && blessed($self->general->ascendingAttributes)
        && $self->general->ascendingAttributes->isa(
          'Game::EvonyTKR::Model::AscendingAttributes')
      ) {
        $self->log_error(sprintf(
          'failed to populate general "%s" in %s',
          $self->general->name, __PACKAGE__
        ));
        return 0;
      }
    }

    $self->general->populateBuiltinBook();
    unless ($self->general->builtInBook
      && ref($self->general->builtInBook)
      && blessed($self->general->builtInBook)
      && $self->general->builtInBook->isa('Game::EvonyTKR::Model::Book')) {
      $self->log_error(sprintf(
        'failed to populate general "%s" in %s',
        $self->general->name, __PACKAGE__
      ));
      return 0;
    }

    $self->general->populateSpecialties();
    unless (
      scalar($self->general->specialties->@*) ==
      scalar($self->general->specialtyNames->@*)) {
      $self->log_error(sprintf(
        'failed to populate general "%s" in %s',
        $self->general->name, __PACKAGE__
      ));
      return 0;
    }
    unless (
      all {
             defined($_)
          && ref($_)
          && blessed($_)
          && $_->isa('Game::EvonyTKR::Model::Specialty')
      } $self->general->specialties->@*
    ) {
      $self->log_error(sprintf(
        'failed to populate general "%s" in %s',
        $self->general->name, __PACKAGE__
      ));
      return 0;
    }

    state $covenant_helper;
    $covenant_helper //= do {
      my $helper = eval { Game::EvonyTKR::Model::Base->new(); };
      if ($@) {
        $self->log_error("Cannot create covenant helper: $@");
        return;
      }
      $helper;
    };

    my $nn = lc($self->normalize($self->general->name));
    if (any { $nn eq lc($self->normalize($_)) }
      $covenant_helper->list_covenants->@*) {
      $self->_private->{covenant} = $covenant_helper->get_covenant($nn);
      unless ($self->_private->{covenant}) {
        $self->log_error(sprintf(
          'failed to populate general "%s" in %s',
          $self->general->name, __PACKAGE__
        ));
        return 0;
      }
    }

    state $books_helper;
    $books_helper //= do {
      my $helper = eval {
        Game::EvonyTKR::Model::Base->new->with_roles(
          'Game::EvonyTKR::Role::Persistence',
          'Game::EvonyTKR::Role::Constants::BuffConstants',
          'Game::EvonyTKR::Role::Constants::GeneralConstants',
          'Game::EvonyTKR::Role::Constants::Books',
          'Game::EvonyTKR::Role::Books',
        );
      };
      if ($@) {
        $self->log_error("Cannot create books helper: $@");
        return;
      }
      $helper;
    };

    unless ($self->targetType) {
      $self->log_error(
        sprintf('targetType is required for %s', __PACKAGE__));
      return 0;
    }

    my $ctt = lc($self->targetType);
    $ctt =~ s/ /_/g;
    $ctt =~ s/(?:machines|troops)/specialist/;

    if ($ctt eq 'mayor') {
      $self->_private->{books} =
        [$books_helper->load_mandatory_skill_books()->@*,];
    }
    else {
      $self->_private->{books} = [
        $books_helper->load_best_skill_books($self->general, $ctt,
          $self->activationType)->@*,
        $books_helper->load_mandatory_skill_books()->@*,
      ];
    }

    $self->log_debug(sprintf(
      'found total book set %s for general "%s" ctt "%s" activationType "%s"',
      join(', ',
        map { sprintf('"%s"', $_->name) } @{ $self->_private->{books} }),
      $self->general->name,
      $ctt,
      $self->activationType,
    ));

    $self->_private->{is_inflated} = 1;
    return 1;
  }

  sub updateBuffs ($self) {
    unless ($self->inflate()) {
      $self->log_logcroak(sprintf('failed to inflate %s', __PACKAGE__));
      return;
    }
    $self->log_info(sprintf(
      'updateBuffs called for %s with isPrimary "%s" '
        . 'targetType "%s" activationType "%s", general set to %s %s %s %s %s %s',
      $self->general->name,  $self->isPrimary,      $self->targetType,
      $self->activationType, $self->ascendingLevel, $self->specialty1,
      $self->specialty2,     $self->specialty3,     $self->specialty4,
      $self->covenantLevel,
    ));

    if (!$self->general->can_afford_ascending_level($self->ascendingLevel)) {
      $self->log_warn("requsted level '"
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
    $self->log_info("returning buffs for "
        . $self->general->name
        . Data::Printer::np($self->buffValues));
  }

  sub updateDebuffs ($self) {
    unless ($self->inflate()) {
      $self->log_logcroak(sprintf('failed to inflate %s', __PACKAGE__));
      return;
    }
    if (!$self->general) {
      $self->log_error("NO GENERAL ASSIGNED FOR " . blessed($self));
      return;
    }
    $self->log_info(sprintf(
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
    $self->log_info("returning debuffs for"
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

      $self->log_debug("Filtering buff conditions for "
          . $self->activationType
          . ": allowed = ["
          . join(', ', @$allowed)
          . "] → result = ["
          . join(', ', @filtered)
          . "]");

      return \@filtered;
    }
    else {
      $self->log_warn(
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
      my $genericBooks = $self->getGenericBookValue($attribute, $buffType);
      $total += $genericBooks;
      $self->log_debug(sprintf(
        'adding generic book value %s ' . 'for attribute %s  and buff type %s',
        $genericBooks, $attribute, $buffType
      ));
    }

    $total +=
      $self->summarize_from_sources($attribute, $buffType, $buffConditions);

    $self->log_debug("returning $attribute total for $buffType: $total");
    return $total;
  }

  sub getGenericBookValue ($self, $attribute, $troopType) {
    # Hook for subclasses (e.g., Pair) to customize book selection
    return $self->_getGenericBookValue_impl($attribute, $troopType);
  }

  sub updateDebuff ($self, $attribute, $debuffType) {
    my $total            = 0;
    my $buffConditions   = $self->filterBuffConditions();
    my $debuffConditions = $self->filterDebuffConditions();

    if (!scalar(@$debuffConditions)) {
      $self->log_error("Debuff MUST have debuffConditions.");
      return 0;
    }

    $total +=
      $self->summarize_from_sources($attribute, $debuffType, $buffConditions,
      $debuffConditions);

    $self->log_debug("returning $attribute total for $debuffType: $total");
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

    $self->log_info(
          "summarize_from_sources has $total after summarize_book "
        . "for $attribute/$summaryType");

    $total += $self->summarize_covenant_for_attribute(
      $attribute,        $summaryType, $buffConditions,
      $debuffConditions, $matching_type
    );

    $self->log_info(sprintf(
      'summarize_from_sources has %s after ' . 'summarize_covenant for %s/%s',
      $total, $attribute, $summaryType
    ));

    $total += $self->summarize_specialties_for_attribute(
      $attribute,        $summaryType, $buffConditions,
      $debuffConditions, $matching_type
    );

    $self->log_info(sprintf(
      'summarize_from_sources has %s after '
        . 'summarize_specialties for %s/%s',
      $total, $attribute, $summaryType
    ));

    if ($self->isPrimary && $self->general->ascending) {
      $total += $self->summarize_ascendingAttributes_for_attribute(
        $attribute,        $summaryType, $buffConditions,
        $debuffConditions, $matching_type
      );

      $self->log_info(sprintf(
        'summarize_from_sources has %s after '
          . 'summarize_ascendingAttributes for %s/%s',
        $total, $attribute, $summaryType
      ));
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
    $self->log_debug($self->general->name
        . " book name: "
        . ($self->general->builtInBookName // 'undefined'));

    if (not defined $self->general->builtInBook
      && length($self->general->builtInBookName) > 0) {
      $self->log_error('Book must be loaded first!!');
      return;
    }

    my $book = $self->general->builtInBook();
    if ($book) {
      $self->log_debug("adding buffs for book " . $book->name);
      my $bv = $book->get_buffs(
        $attribute,      $matching_type, $summaryType,
        $buffConditions, $debuffConditions
      );
      $self->log_debug(sprintf(
        'found %s in %s %s buffs for %s.',
        $bv, $book->name, $attribute, $self->general->name
      ));
      $total += $bv;
    }
    else {
      $self->log_error(
        "cannot update total with book " . Data::Printer::np($book));
    }

    $self->log_debug(sprintf(
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

    my $covenant = $self->_private->{covenant};
    if (defined($covenant) && $covenant->isa('Game::EvonyTKR::Model::Covenant'))
    {
      $self->log_debug("Found covenant for "
          . $self->general->name
          . " now processing at level "
          . $self->covenantLevel
          . " for attribute $attribute.");

      my $cv =
        $covenant->get_buffs_at_level($self->covenantLevel, $attribute,
        $matching_type, $summaryType, $buffConditions, $debuffConditions);
      $self->log_debug(sprintf(
        'retrieved %s as total %s for level %s of covenant for %s',
        $cv, $attribute, $self->covenantLevel, $self->general->name
      ));
      $total += $cv;
    }

    $self->log_debug(sprintf(
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
    $self->log_debug(sprintf(
'%s summarize_specialties_for_attribute called for "%s" looking for levels '
        . 'sp1: "%s"; sp2: "%s"; sp3: "%s"; sp4: "%s";',
      $matching_type,    $self->general->name, $self->specialty1,
      $self->specialty2, $self->specialty3,    $self->specialty4
    ));
    $self->log_debug(sprintf(
'%s summarize_specialties_for_attribute for "%s" attribute: "%s"; summaryType: "%s"',
      $matching_type, $self->general->name, $attribute, $summaryType
    ));

    foreach my $sn_index (0 .. $#specialtyNames) {
      my $sn = $specialtyNames[$sn_index];
      my $sl = lc($specialtyLevels[$sn_index]);
      $self->log_debug(
        "processing " . $self->general->name . " $sn at level $sl");

      my $specialty = $self->general->specialties->[$sn_index];
      if ($specialty) {
        $self->log_debug(
          sprintf('checking %s for %s', $specialty->name, $attribute));
        my $sv = $specialty->get_buffs_at_level($sl, $attribute, $matching_type,
          $summaryType, $buffConditions, $debuffConditions);
        $self->log_debug("retrieved $sv as total $attribute for level $sl "
            . $specialty->name
            . " as part of "
            . $self->general->name);
        $total += $sv;
      }
      else {
        $self->log_error(sprintf(
          'cannot retrieve specialty %s for %s',
          $sn, $self->general->name
        ));
      }
    }

    $self->log_debug(sprintf(
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

    my $aa = $self->general->ascendingAttributes;
    if ($aa) {
      $self->log_debug(
        "retrieved ascendingAttribute buffs for " . $self->general->name);
      my $av =
        $aa->get_buffs_at_level($self->ascendingLevel, $attribute, $summaryType,
        $buffConditions, $debuffConditions, $matching_type);
      $self->log_debug(sprintf(
        '%s Ascending Attributes has %s buffs with total %s at level %s',
        $self->general->name, $attribute, $av, $self->ascendingLevel,
      ));
      $total += $av;
    }
    else {
      $self->log_error(
        "cannot find Ascending Attributes for " . $self->general->name);
    }

    $self->log_debug(sprintf(
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
