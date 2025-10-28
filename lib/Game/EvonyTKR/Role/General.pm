use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require JSON::PP;
require Mojo::JSON;
require Game::EvonyTKR::Model::BasicAttribute;
require Game::EvonyTKR::Model::BasicAttributes;

package Game::EvonyTKR::Role::General {
  use Mojo::Base 'Game::EvonyTKR::Role::Common',                   -signatures;
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::BuffConstants', -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::GeneralConstants',    -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::AscendingAttributes', -role;
  use List::AllUtils qw( any none );
  use UUID           qw(uuid5);
  use namespace::autoclean;
  use Carp;
  use File::FindLib 'lib';

  our $VERSION = 'v0.40.0';
  my $debug = 1;

  sub validate($self) {
    my @errors;
    if (not defined $self->GeneralKeys) {
      $self->logger->logcroak('GeneralKeys is not defined in a General');
    }
    if (not defined $self->type) {
      push @errors,
        sprintf('type must be one of %s', join(', ', @{ $self->GeneralKeys }));
    }
    elsif (not $self->ValidateGeneralType($self->type)) {
      push @errors, 'General Type Failed Validation';
    }

    my @valv;
    map { push @valv, $_ } $self->AscendingAttributeLevelValues();
    map { push @valv, $_ } $self->AscendingAttributeLevelValues(0);
    if (none { $self->stars =~ /$_/ } @valv) {
      push @errors,
        sprintf(
        'stars must be one of %s, not "%s"',
        join(',', @valv),
        $self->stars
        );
    }

    if (@errors) {
      $self->logger->logcroak(join ', ', @errors);
      return;
    }
    return 1;
  }

  sub set_general_id($self) {
    if (ref $self->type) {
      my @ts;
      push @ts, $self->type->@*;
      my $ut = $ts[0];
      $self->logger->debug("using type $ut");
      my $uuid5base = $self->UUID5_Generals->{$ut};
      $self->id = uuid5($uuid5base, $self->name);
    }
    else {
      my $uuid5base = $self->UUID5_Generals->{ $self->type };
      $self->id = uuid5($uuid5base, $self->normalize($self->name));
    }
  }

  sub populateSpecialties ($self, $allSpecialties) {
    my @specialtyNames;
    push @specialtyNames, $self->specialtyNames->@*;
    foreach my $sn_index (0 .. scalar(@specialtyNames)) {
      my $sn = $specialtyNames[$sn_index];
      $self->logger->debug("populating $sn");
      my $specialty = $allSpecialties->{$sn};
      if ($specialty) {
        $self->specialties->[$sn_index] = $specialty;
      }
      else {
        $self->logger->error(sprintf(
          'Missing specialty number %s for general "%s": "%s" ',
          $sn_index, $self->name, $sn
        ));
      }
    }
  }

  sub can_afford_ascending_level($self, $requestedLevel) {
    my @valv;
    map { push @valv, $_ } $self->AscendingAttributeLevelValues();
    map { push @valv, $_ } $self->AscendingAttributeLevelValues(0);
    if (any { $requestedLevel eq $_ } @valv) {
      my %ranks = (
        none    => 0,
        purple1 => 1,
        purple2 => 2,
        purple3 => 3,
        purple4 => 4,
        purple5 => 5,
        red1    => 6,
        red2    => 7,
        red3    => 8,
        red4    => 9,
        red5    => 10
      );
      my $mr = $ranks{ $self->stars };
      my $rr = $ranks{$requestedLevel};
      return $rr <= $mr;
    }
    return 0;
  }

  sub populateBuiltinBook ($self) {
    my ($book, $books_helper, $cache_store);

    eval {
      $books_helper = Mojo::Base->new->with_roles(
        'Game::EvonyTKR::Role::Logger',
        'Game::EvonyTKR::Role::Cache',
        'Game::EvonyTKR::Role::Common',
        'Game::EvonyTKR::Controller::Role::Books'
      );
    } or do {
      $self->logger->error(
        sprintf('eval failed; cannot define book helper: "%s"', $@));
      return;
    };

    eval { $cache_store = $books_helper->create_book_cache(); } or do {
      $self->logger->error(
        sprintf('eval failed; cannot create book cache from helper: "%s"', $@));
      return;
    };

    if (defined($book)) {
      $self->logger->debug(sprintf(
        'fetch returned book "%s" with name "%s" for builtInBookName "%s"',
        blessed($book), $book->can('name') ? $book->name : 'no name method',
        $self->builtInBookName
      ));
    }
    else {
      $self->logger->error(
        sprintf('failed to fetch book for builtin book "%s" from cache',
          $self->builtInBookName)
      );
    }
    $self->builtInBook($book);
    return $self;
  }

  sub from_hash ($self, $hashObject) {
    my $logger = Game::EvonyTKR::Log::Config->logger();

    if (!exists $hashObject->{name}) {
      $self->logger->error('hash object must contain a name attribute.');
      return undef;
    }

    my $g = Game::EvonyTKR::Model::General->new(
      name            => $hashObject->{name},
      type            => $hashObject->{type},
      ascending       => $hashObject->{ascending},
      stars           => $hashObject->{stars},
      builtInBookName => $hashObject->{book},
      specialtyNames  => $hashObject->{specialties},
    );
    unless ($g->validate()) {
      $self->logger->error('Invalid Hash Object.');
      return;
    }

    foreach my $baKey (keys %{ $hashObject->{basic_attributes} }) {
      my $ba = Game::EvonyTKR::Model::BasicAttribute->new(
        attribute_name => $baKey,
        base           => $hashObject->{basic_attributes}->{$baKey}->{base},
        increment => $hashObject->{basic_attributes}->{$baKey}->{increment},
      );
      $g->basicAttributes->setAttribute($baKey, $ba);
    }

    return $g;
  }

}
1;

__END__
