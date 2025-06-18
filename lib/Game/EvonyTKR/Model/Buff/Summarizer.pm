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
  field $keepLevel : reader : param      //= 40;

  # output values
  field $marchIncrease : reader  = 0;
  field $attackIncrease : reader = 0;

  # some parameters unused currently, but adding them for future growth
  method updateBuffs (

  ) {
    $marchIncrease  = $self->updateMarchSize();
    $attackIncrease = $self->updateAttackIncrease();
  }

  # some parameters unused currently, but adding them for future growth
  method updateMarchSize () {
    my $total = 0;

    if (
      $isPrimary
      && $rootManager->generalConflictGroupManager->is_book_compatible(
        'March Size Increase',
        $general->name
      )
    ) {
      $total += 12;
    }

    my $bv = $self->summarize_book_for_attribute('March Size Capacity');
    $total += $bv;

    my $sv = $self->summarize_specialities_for_attribute('March Size Capacity');
    $total += $sv;

    if ($isPrimary) {
      my $av = $self->summarize_ascendingAttributes_for_attribute(
        'March Size Capacity');
      $total += $av;
    }

    return $total;
  }

  method updateAttackIncrease (

  ) {

    # set $total to 0 to ensure this function is idempotent
    my $total = 0;

    if (
         $isPrimary
      && $targetType =~ /Mounted/i
      && $rootManager->generalConflictGroupManager->is_book_compatible(
        'Mounted Attack Increase',
        $general->name
      )
    ) {

      $total += 25;

    }

    if (
         $isPrimary
      && $targetType =~ /Ground/i
      && $rootManager->generalConflictGroupManager->is_book_compatible(
        'Ground Troop Attack',
        $general->name
      )
    ) {

      $total += 25;

    }

    if (
         $isPrimary
      && $targetType =~ /Ranged/i
      && $rootManager->generalConflictGroupManager->is_book_compatible(
        'Ranged Troop Attack',
        $general->name
      )
    ) {

      $total += 25;

    }

    if (
         $isPrimary
      && $targetType =~ /Siege/i
      && $rootManager->generalConflictGroupManager->is_book_compatible(
        'Siege Machine Attack',
        $general->name
      )
    ) {

      $total += 25;

    }

    my $bv = $self->summarize_book_for_attribute('Attack');
    $total += $bv;

    my $sv = $self->summarize_specialities_for_attribute('Attack');
    $total += $sv;

    if ($isPrimary) {
      my $av = $self->summarize_ascendingAttributes_for_attribute('Attack');
      $total += $av;
    }

    return $total;
  }

  method summarize_book_for_attribute ($attribute) {
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

      $self->logger->debug("adding buffs to pair for book " . $book->name);
      my $bv = $book->get_buffs($attribute, $targetType);
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
    return $total;
  }

  method summarize_specialities_for_attribute ($attribute) {
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

        my $sv = $speciality->get_buffs_at_level($sl, $attribute, $targetType);
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
    return $total;
  }

  method summarize_ascendingAttributes_for_attribute ($attribute) {
    my $total = 0;

    my $aa = $rootManager->ascendingAttributesManager->getAscendingAttributes(
      $general->name);
    if ($aa) {
      $self->logger->debug(
        "retrieved ascendingAttribute buffs for " . $general->name);
      my $av = $aa->get_buffs_at_level($ascendingLevel, $attribute, $targetType);
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

    return $total;
  }

  };
1;

__END__

#ABSTRACT: This is a helper class to allow consumers of the class to take arrays of Game::EvonyTKR::Model::Buff objects and summarize them
