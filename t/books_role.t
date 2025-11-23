#!/usr/bin/env perl
use v5.42.0;
use utf8::all;
use Test2::V0;
use File::FindLib 'lib';

require Game::EvonyTKR::Role::Constants::Books;

my $obj = Mojo::Base->new->with_roles('Game::EvonyTKR::Role::Constants::Books');

my $books = $obj->BestSkillBooks;

# Test that interpolation worked
like($books->{ground_specialist}->{default}->{'Level 4 Ground Troop Attack'},
     qr/^\d+$/,
     'Level 4 book exists with numeric priority');


# Test that $bestLevel was interpolated correctly
my @keys = keys %{$books->{ground_specialist}->{default}};
ok((grep { /^Level 4 / } @keys), 'Keys contain "Level 4"');

# Test immutability
like(
  dies { $books->{ground_specialist}->{default}->{'Level 4 Ground Troop Attack'} = 999; },
  qr/Modification of a read-only value/,
  'Cannot modify const hash'
);

done_testing();
