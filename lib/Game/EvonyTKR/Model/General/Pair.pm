use v5.40.0;
use experimental qw(class);
use utf8::all;
require JSON::PP;
require Scalar::Util;

class Game::EvonyTKR::Model::General::Pair :isa(Game::EvonyTKR::Model::Data) {
  use overload
    '""' => \&as_string,
    'fallback' => 1;

  field $primary : reader : param;
  field $secondary : reader : param;
  field $rootManager :reader = undef;

  #computed fields;

  field $marchbuff :reader = 0;

  method setRootManager ( $nm ) {
    if (Scalar::Util::blessed $nm eq 'Game::EvonyTKR::Model::EvonyTKR::Manager' ) {
      $rootManager = $nm;
      return 1;
    }
    return 0;
  }

  #parameters unused currently, but adding them for future growth
  method updateBuffs (
    $primaryAscending       = 'None',
    $primarySpeciality1     = 'none',
    $primarySpeciality2     = 'none',
    $primarySpeciality3     = 'none',
    $primarySpeciality4     = 'none',
    $secondarySpeciality1   = 'none',
    $secondarySpeciality2   = 'none',
    $secondarySpeciality3   = 'none',
    $secondarySpeciality4   = 'none',
    $keepLevel              = 40,
    $primaryLevel           = 45,
    $secondaryLevel         = 45,
  ){
    $self->updateMarchSize();
  }

  #parameters unused currently, but adding them for future growth
  method updateMarchSize (
  $primaryAscending       = 'None',
  $primarySpeciality1     = 'none',
  $primarySpeciality2     = 'none',
  $primarySpeciality3     = 'none',
  $primarySpeciality4     = 'none',
  $secondarySpeciality1   = 'none',
  $secondarySpeciality2   = 'none',
  $secondarySpeciality3   = 'none',
  $secondarySpeciality4   = 'none',
  $keepLevel              = 40,
  ) {
    if(not defined $primary->builtInBook && length($primary->builtInBookName) > 0){
      if(defined $rootManager) {
        $self->logger()->debug("requesting populateBuiltInBook for " . $primary->name());
        $primary->populateBuiltInBook( $rootManager->bookManager );
      } else {
        $self->logger->logcroak("No Root Manager Available");
      }
    }
    if(not defined $secondary->builtInBook && length($secondary->builtInBookName) > 0){
      if(defined $rootManager) {
        $self->logger()->debug("requesting populateBuiltInBook for " . $secondary->name());
        $secondary->populateBuiltInBook( $rootManager->bookManager );
      } else {
        $self->logger->logcroak("No Root Manager Available");
      }
    }


    for my $book ($primary->builtInBook(), $secondary->builtInBook() ) {
      if(defined $book && blessed($book) && blessed($book) eq 'Game::EvonyTKR::Model::Book' ) {
        $self->logger->debug("adding buffs to pair for book " . $book->name);
        my $buffs = $book->buff;
        $self->logger->trace(sprintf('found %s buffs in %s for %s.',
          scalar @{$buffs},
          $book->name,
          $primary->name,
        ));
        for my $buff ( @{ $buffs }) {
          if($buff->attribute eq 'March Size Capacity') {
            $marchbuff += $buff->value->number();
          }
        }
      } else {
        $self->logger->error("cannot update marchbuff with book ". Data::Printer::np($book));
      }
    }

  }
  # Method to convert to hash
  method to_hash {
      return {
          primary => $primary,
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
      my $json = JSON::PP->new->utf8->pretty->canonical(1)->allow_blessed(1)->convert_blessed(1)->encode($self->to_hash());
      return $json;
  }

}
1;
