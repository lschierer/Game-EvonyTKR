package Game::EvonyTKR::Controller::Role::Generals::Routing;
use v5.42.0;
use experimental qw(class);
use utf8::all;
use Mojo::Base -role, -strict, -signatures;
use Carp;
require Data::Printer;
require Path::Tiny;
require Game::EvonyTKR::Model::General;
require Game::EvonyTKR::Model::General::Pair;
use diagnostics;

our $vr;

has validRoutes => sub {
  my $c = shift;
  unless ($vr) {
    # assign an initial value so that this does not recurse
    $vr = {};
    $vr = $c->get_valid_routes();
  }
  return $vr;
};

has routing_debug => 0;

sub all_valid_routes($c) {
  return values $c->validRoutes->%*;
}

sub get_routes_for_uiTarget ($c, $uiTarget) {
  $c->log_debug("looking for routes for $uiTarget");
  my @results;
  my $slug = $c->_slugify($uiTarget);
  $c->log_debug("slug for $uiTarget is $slug");
  foreach my $key (keys $c->validRoutes->%*) {
    if ($key =~ /^$slug/) {
      push @results, $c->validRoutes->{$key};
    }
  }
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
  my $slug_ui   = $c->_slugify($uiTarget);
  my $slug_buff = $c->_slugify($buffActivation);
  my $key       = "$slug_ui|$slug_buff";
  return $c->validRoutes->{$key};    # undef if missing; NO croak
}

sub lookup_route ($c, $slug_ui, $slug_buff,) {
  $slug_ui   = $c->_slugify($slug_ui);
  $slug_buff = $c->_slugify($slug_buff);
  my $key = lc("$slug_ui|$slug_buff");
  if (exists $c->validRoutes->{$key}) {
    return $c->validRoutes->{$key};
  }
  if ($c->routing_debug) {
    my @r = $c->all_valid_routes();
    $c->log_error("$key is not a valid route. Valid routes are "
        . Data::Printer::np($c->validRoutes));
  }
  else {
    $c->log_error("$key is not a valid route.");
    croak("$key is not a valid route.");
  }
  return 0;
}

sub _slugify ($c, $str) {
  $str =~ s/\s+/-/g;
  $str =~ s/[^a-zA-Z0-9\-]//g;
  return lc $str;
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
      my $uiTarget  = $c->_ui_target_name($tt);
      my $slug_ui   = $c->_slugify($uiTarget);
      my $slug_buff = $c->_slugify($buffActivation);

      # Save valid combo using pipe as a dsv separator
      $c->validRoutes->{"$slug_ui|$slug_buff"} = {
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
