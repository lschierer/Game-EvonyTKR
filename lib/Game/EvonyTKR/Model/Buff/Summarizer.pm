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
  use overload
    '""'       => \&as_string,
    '.'        => \&concat,
    'fallback' => 0;

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
  has 'books' => sub { undef };

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
      $self->logger->error(sprintf('%s requires a general', __PACKAGE__));
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
        $self->logger->error(sprintf(
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
      $self->logger->error(sprintf(
        'failed to populate general "%s" in %s',
        $self->general->name, __PACKAGE__
      ));
      return 0;
    }

    $self->general->populateSpecialties();
    unless (
      scalar($self->general->specialties->@*) ==
      scalar($self->general->specialtyNames->@*)) {
      $self->logger->error(sprintf(
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
      $self->logger->error(sprintf(
        'failed to populate general "%s" in %s',
        $self->general->name, __PACKAGE__
      ));
      return 0;
    }

    state $covenant_helper;
    $covenant_helper //= do {
      my $helper = eval {
        Mojo::Base->new->with_roles(
          'Game::EvonyTKR::Role::Logging',
          'Game::EvonyTKR::Role::Common',
          'Game::EvonyTKR::Controller::Role::Covenants'
        );
      };
      if ($@) {
        $self->logger->error("Cannot create covenant helper: $@");
        return;
      }
      $helper;
    };

    my $nn = lc($self->normalize($self->general->name));
    if (any { $nn eq lc($self->normalize($_)) }
      $covenant_helper->list_covenants->@*) {
      $self->_private->{covenant} = $covenant_helper->get_covenant($nn);
      unless ($self->_private->{covenant}) {
        $self->logger->error(sprintf(
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
          'Game::EvonyTKR::Controller::Role::Books',
          'Game::EvonyTKR::Role::Constants::BuffConstants',
          'Game::EvonyTKR::Role::Constants::GeneralConstants',
          'Game::EvonyTKR::Role::Constants::Books',
        );
      };
      if ($@) {
        $self->logger->error("Cannot create books helper: $@");
        return;
      }
      $helper;
    };

    unless ($self->targetType) {
      $self->logger->error(
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

    $self->logger->debug(sprintf(
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
      $self->logger->logcroak(sprintf('failed to inflate %s', __PACKAGE__));
      return;
    }
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
    unless ($self->inflate()) {
      $self->logger->logcroak(sprintf('failed to inflate %s', __PACKAGE__));
      return;
    }
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
      my $genericBooks = $self->getGenericBookValue($attribute, $buffType);
      $total += $genericBooks;
      $self->logger->debug(sprintf(
        'adding generic book value %s ' . 'for attribute %s  and buff type %s',
        $genericBooks, $attribute, $buffType
      ));
    }

    $total +=
      $self->summarize_from_sources($attribute, $buffType, $buffConditions);

    $self->logger->debug("returning $attribute total for $buffType: $total");
    return $total;
  }

  sub getGenericBookValue ($self, $attribute, $troopType) {
    my $total = 0;

    # Special case for March Size - it's universal, not troop-specific
    if ($attribute eq 'March Size') {
      state $books_helper;
      $books_helper //= do {
        my $helper = eval {
          Game::EvonyTKR::Model::Base->new->with_roles(
            'Game::EvonyTKR::Controller::Role::Books',
          );
        };
        if ($@) {
          $self->logger->error("Cannot create books helper: $@");
          return $total;
        }
        $helper;
      };

      my $MS = $books_helper->get_generic_book('March Size', $self->bestLevel);
      if ($MS && $self->bc->is_general_and_book_compatible(
            $self->general, $MS, { same_side => 1 })) {
        $total += $MS->buffs->[0]->value->number;
      }
      return $total;
    }

    # Overall isn't a real troop type - use general's primary type
    # TODO: Handle Overall properly in book selection logic
    if ($troopType eq 'Overall') {
      $troopType = ref($self->general->type) ? $self->general->type->[0] : $self->general->type;
      $troopType =~ s/_specialist$//;
      $troopType =~ s/_/ /g;
      $troopType = ucfirst($troopType) . ' Troops';
    }

    # Get books_helper with BestSkillBooks constants
    state $books_helper;
    $books_helper //= do {
      my $helper = eval {
        Game::EvonyTKR::Model::Base->new->with_roles(
          'Game::EvonyTKR::Controller::Role::Books',
          'Game::EvonyTKR::Role::Constants::Books',
        );
      };
      if ($@) {
        $self->logger->error("Cannot create books helper: $@");
        return $total;
      }
      $helper;
    };

    # Convert troop type to target type key
    my $tt = $troopType =~ s/ Troops$//r;
    my $targetType = lc($tt) . '_specialist';
    $targetType =~ s/siege machines/siege/;
    $targetType =~ s/ /_/g;

    # Determine which book list to use
    my $key = $self->activationType eq 'PvM' ? 'PvM' : 'default';

    # Get the best books for this troop type
    my $book_priorities = $books_helper->BestSkillBooks->{$targetType}->{$key} // {};

    $self->logger->debug(sprintf(
      'getGenericBookValue: attr=%s, troopType=%s, targetType=%s, key=%s, found %d books',
      $attribute, $troopType, $targetType, $key, scalar keys %$book_priorities
    ));

    # Sort books by priority and check until we find 3 compatible ones
    my @sorted_books = sort { $book_priorities->{$a} <=> $book_priorities->{$b} }
                       keys %$book_priorities;

    my $found = 0;
    for my $book_name (@sorted_books) {
      last if $found >= 3;  # Stop after finding 3 compatible books

      # Extract base name (remove "Level X" prefix)
      my $base_name = $book_name =~ s/^Level \d+ //r;
      my $book = $books_helper->get_generic_book($base_name, $self->bestLevel);

      next unless $book;

      # Check if this book provides the attribute we're looking for
      my $provides_attr = 0;
      for my $buff (@{$book->buffs}) {
        if ($buff->attribute eq $attribute &&
            ($buff->targetedType // '') eq $troopType) {
          $provides_attr = 1;
          last;
        }
      }

      next unless $provides_attr;

      # Check compatibility
      my $compat = $self->bc->is_general_and_book_compatible(
            $self->general, $book, { same_side => 1 });

      $self->logger->debug(sprintf(
        'Book %s for %s: provides_attr=%d, compat=%d',
        $book->name, $attribute, $provides_attr, $compat
      ));

      if ($compat) {
        for my $buff (@{$book->buffs}) {
          if ($buff->attribute eq $attribute &&
              ($buff->targetedType // '') eq $troopType) {
            $total += $buff->value->number;
            $self->logger->debug(sprintf(
              'Adding %d from %s, total now %d',
              $buff->value->number, $book->name, $total
            ));
          }
        }
        $found++;
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
      'summarize_from_sources has %s after ' . 'summarize_covenant for %s/%s',
      $total, $attribute, $summaryType
    ));

    $total += $self->summarize_specialties_for_attribute(
      $attribute,        $summaryType, $buffConditions,
      $debuffConditions, $matching_type
    );

    $self->logger->info(sprintf(
      'summarize_from_sources has %s after '
        . 'summarize_specialties for %s/%s',
      $total, $attribute, $summaryType
    ));

    if ($self->isPrimary && $self->general->ascending) {
      $total += $self->summarize_ascendingAttributes_for_attribute(
        $attribute,        $summaryType, $buffConditions,
        $debuffConditions, $matching_type
      );

      $self->logger->info(sprintf(
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

    my $covenant = $self->_private->{covenant};
    if (defined($covenant) && $covenant->isa('Game::EvonyTKR::Model::Covenant'))
    {
      $self->logger->debug("Found covenant for "
          . $self->general->name
          . " now processing at level "
          . $self->covenantLevel
          . " for attribute $attribute.");

      my $cv =
        $covenant->get_buffs_at_level($self->covenantLevel, $attribute,
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
      $matching_type,    $self->general->name, $self->specialty1,
      $self->specialty2, $self->specialty3,    $self->specialty4
    ));
    $self->logger->debug(sprintf(
'%s summarize_specialties_for_attribute for "%s" attribute: "%s"; summaryType: "%s"',
      $matching_type, $self->general->name, $attribute, $summaryType
    ));

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

    my $aa = $self->general->ascendingAttributes;
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
