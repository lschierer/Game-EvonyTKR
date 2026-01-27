package Game::EvonyTKR::Controller::Role::Generals::Routing;
use v5.42.0;
use experimental qw(class);
use utf8::all;
use Moo::Role;
use Carp;
require Data::Printer;
require Path::Tiny;
require Game::EvonyTKR::Model::General;
require Game::EvonyTKR::Model::General::Pair;
use diagnostics;

our $vr;

has validRoutes => (
  is      => 'ro',
  lazy    => 1,
  default => sub ($self) {
    unless ($vr) {
      # assign an initial value so that this does not recurse
      $vr = {};
      $vr = $self->get_valid_routes();
    }
    return $vr;
  },
);

has routing_debug => (
  is      => 'ro',
  default => sub {0},
);

sub all_valid_routes($c) {
  return values $c->validRoutes->%*;
}

sub get_routes_for_uiTarget ($c, $uiTarget) {
  $c->logger->debug("looking for routes for $uiTarget");
  my @results;
  #my $slug = $c->_slugify($uiTarget);
  #$c->logger->debug("slug for $uiTarget is $slug");
  if ($uiTarget =~ /(?:wall|mayor)/i) {
    push @results, $c->validRoutes->{$uiTarget};
  }
  else {
    foreach my $key (keys $c->validRoutes->%*) {
      if ($key =~ /^$uiTarget/) {
        $c->logger->debug("'$key' =~ '$uiTarget' -- good")
          if $c->app->env eq 'development';
        push @results, $c->validRoutes->{$key};
      }
      else {
        $c->logger->debug("'$key' !~ '$uiTarget'")
          if $c->app->env eq 'development';
      }
    }
  }

  $c->logger->debug(sprintf(
    'there are %s routes for ui target "%s"',
    scalar(@results), $uiTarget
  ));
  return @results;
}

# In Game::EvonyTKR::Control::Generals::Routing
sub has_route ($c, $uiTarget, $buffActivation) {
  my $slug_ui   = $c->_slugify($uiTarget);
  my $slug_buff = $c->_slugify($buffActivation);
  my $key       = "$slug_ui|$slug_buff";
  return exists $c->validRoutes->{$key};
}

sub try_lookup_route ($c, $uiTarget, $buffActivation) {
  #my $slug_ui   = $c->_slugify($uiTarget);
  #my $slug_buff = $c->_slugify($buffActivation);
  #my $key       = "$slug_ui|$slug_buff";
  return $c->validRoutes->{"$uiTarget|$buffActivation"}
    ;    # undef if missing; NO croak
}

sub lookup_route ($c, $uiTarget, $buffActivation) {
  #$slug_ui   = $c->_slugify($slug_ui);
  #$slug_buff = $c->_slugify($slug_buff);
  #my $key = lc("$slug_ui|$slug_buff");
  if (exists $c->validRoutes->{"$uiTarget|$buffActivation"}) {
    return $c->validRoutes->{"$uiTarget|$buffActivation"};
  }
  if ($c->routing_debug) {
    my @r = $c->all_valid_routes();
    $c->logger->error(
      "'$uiTarget|$buffActivation' is not a valid route. Valid routes are "
        . Data::Printer::np($c->validRoutes));
  }
  else {
    $c->logger->error("'$uiTarget|$buffActivation' is not a valid route.");
  }
  return undef;    # Return undef instead of croaking - let caller handle it
}

sub _ui_target_name ($c, $tt) {
  my $name = $tt =~ s/_/ /gr;
  $name =~ s/(\w)(\w+)( specialist)?/\U$1\L$2 \US\Lpecialists/;
  $name =~ s/Mounted/Cavalry/g;
  $name =~ s/Ground/Infantry/g;
  $name =~ s/Ranged/Archer/g;
  $name =~ s/Officer/Duty/g;
  return $name;
}

sub general_type_from_ui_target ($c, $uiTarget) {
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
sub get_valid_routes($c) {
  foreach my $buffActivation ($c->AllowedBuffActivationValues->@*) {
    $c->logger->debug(
      sprintf('generating valid routes for buffActivation "%s"',
        $buffActivation)
    );
    foreach my $tt ($c->GeneralKeys->@*) {
      next if $buffActivation eq 'Officer' && $tt ne 'officer';
      next if $buffActivation eq 'Mayor'   && $tt ne 'mayor';
      next
        if $buffActivation =~ /(?:Overall|PvM|Attacking|Out City)/
        && $tt !~ /(?:ground|mounted|ranged|siege)/;
      next
        if $buffActivation =~ /(?:Reinforcing|Defense|In City|Wall)/
        && $tt =~ /(?:mayor|officer)/;

      # Generate slugs
      my $uiTarget = $c->_ui_target_name($tt);
      #my $slug_ui   = $c->_slugify($uiTarget);
      #my $slug_buff = $c->_slugify($buffActivation);

      # Save valid combo using pipe as a dsv separator
      $c->validRoutes->{"$uiTarget|$buffActivation"} = {
        generalType    => $tt,
        uiTarget       => $uiTarget,
        buffActivation => $buffActivation,
      };
    }
  }
  return $c->validRoutes;
}
1;
__END__
