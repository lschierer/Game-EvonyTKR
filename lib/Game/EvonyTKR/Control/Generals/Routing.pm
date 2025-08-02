use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require Path::Tiny;
require Game::EvonyTKR::Model::General;
require Game::EvonyTKR::Model::General::Manager;
require Game::EvonyTKR::Model::General::Pair;
require Game::EvonyTKR::Model::General::Pair::Manager;
use namespace::clean;

class Game::EvonyTKR::Control::Generals::Routing :
  isa(Game::EvonyTKR::Shared::Constants) {
  use Carp;

  field $validRoutes : reader;
  field $debug : param //= 0;

  ADJUST {
    $validRoutes = {};
    $self->get_valid_routes;
  }

  method all_valid_routes() {
    return values %$validRoutes;
  }

  method get_routes_for_uiTarget ($uiTarget) {
    $self->logger->debug("looking for routes for $uiTarget");
    my @results;
    my $slug = $self->_slugify($uiTarget);
    $self->logger->debug("slug for $uiTarget is $slug");
    foreach my $key (keys %$validRoutes) {
      if ($key =~ /^$slug/) {
        push @results, $validRoutes->{$key};
      }
    }
    return @results;
  }

  method lookup_route ($slug_ui, $slug_buff,) {
    $slug_ui   = $self->_slugify($slug_ui);
    $slug_buff = $self->_slugify($slug_buff);
    my $key = lc("$slug_ui|$slug_buff");
    if (exists $validRoutes->{$key}) {
      return $validRoutes->{$key};
    }
    if ($debug) {
      my @r = $self->all_valid_routes();
      $self->logger->error("$key is not a valid route. Valid routes are "
          . Data::Printer::np($validRoutes));
    }
    else {
      $self->logger->logcroak("$key is not a valid route.");
    }
    return 0;
  }

  method _slugify ($str) {
    $str =~ s/\s+/-/g;
    $str =~ s/[^a-zA-Z0-9\-]//g;
    return lc $str;
  }

  method _ui_target_name ($tt) {
    my $name = $tt =~ s/_/ /gr;
    $name =~ s/(\w)(\w+)( specialist)?/\U$1\L$2 \US\Lpecialists/;
    $name =~ s/Mounted/Cavalry/g;
    $name =~ s/Ground/Infantry/g;
    $name =~ s/Ranged/Archer/g;
    $name =~ s/Officer/Duty/g;
    return $name;
  }

  method general_type_from_ui_target ($uiTarget) {
    my $str = $uiTarget;

    # Undo display substitutions
    $str =~ s/Cavalry/Mounted/g;
    $str =~ s/Infantry/Ground/g;
    $str =~ s/Archer/Ranged/g;
    $str =~ s/Duty/Officer/g;

    # Remove pluralization and formatting
    $str =~ s/\s+Specialists$//i;
    $str = lc $str;
    $str =~ s/\s+/_/g;

    return $str;    # returns e.g. "mounted_specialist"
  }

# the main routes will be dependant on GeneralKeys and AllowedBuffActivationValues
# specialties, skill books, and so on, while important, are essentially ancillary
# information to support generals.
  method get_valid_routes() {
    foreach my $buffActivation ($self->AllowedBuffActivationValues->@*) {
      foreach my $tt ($self->GeneralKeys->@*) {
        next if $buffActivation eq 'Officer' && $tt ne 'officer';
        next if $buffActivation eq 'Mayor'   && $tt ne 'mayor';
        next
          if $buffActivation =~ /(?:Overall|PvM|Attacking|Out City)/
          && $tt !~ /(?:ground|mounted|ranged|siege)/;
        next
          if $buffActivation =~ /(?:Reinforcing|Defense|In City|Wall)/
          && $tt =~ /(?:mayor|officer)/;

        # Generate slugs
        my $uiTarget  = $self->_ui_target_name($tt);
        my $slug_ui   = $self->_slugify($uiTarget);
        my $slug_buff = $self->_slugify($buffActivation);

        # Save valid combo using pipe as a dsv separator
        $validRoutes->{"$slug_ui|$slug_buff"} = {
          generalType    => $tt,
          uiTarget       => $uiTarget,
          buffActivation => $buffActivation,
        };
      }
    }
  }
}
