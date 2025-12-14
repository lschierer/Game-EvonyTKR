#!/usr/bin/env perl
use v5.42.0;
use utf8::all;
use FindBin;
use lib "$FindBin::Bin/../lib";

use Mojolicious::Commands;
use Game::EvonyTKR;

my $app = Game::EvonyTKR->new;

sub walk_routes {
  my ($route, $depth, $parent_path) = @_;
  $depth //= 0;
  $parent_path //= "";

  my $pattern = $route->pattern->unparsed // "";
  my $name = $route->name // "";
  my $to = $route->to // {};
  my $controller = $to->{controller} // "";
  my $action = $to->{action} // "";

  my $full_pattern = $parent_path . $pattern;

  # Skip empty/root patterns
  if ($full_pattern && $full_pattern ne "/" && $full_pattern ne "") {
    printf("%-70s %-35s %s#%s\n",
      $full_pattern,
      $name ? "[$name]" : "",
      $controller,
      $action
    );
  }

  # Recursively walk children
  foreach my $child (@{$route->children}) {
    walk_routes($child, $depth + 1, $full_pattern);
  }
}

say "=" x 140;
say sprintf("%-70s %-35s %s", "ROUTE PATTERN", "NAME", "CONTROLLER#ACTION");
say "=" x 140;
walk_routes($app->routes);
