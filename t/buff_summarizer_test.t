#!/usr/bin/env perl
use v5.42.0;
use lib 'lib';
use utf8::all;
use experimental qw(class);
use Test2::V0;
use Log::Log4perl::Level;
use Path::Tiny;
require YAML::PP;
use List::AllUtils qw( first any none all );

# set the environment variables before
# any BEGIN blocks in the modules being tested
BEGIN {
  $ENV{MOJO_MODE}       = 'development';
  $ENV{LOG_DEBUG}       = 0;
  $ENV{TABLE_TERM_SIZE} = 75;
}

# Load required modules
require Data::Printer;
require Game::EvonyTKR::Model::Buff::Summarizer;
require Game::EvonyTKR::Service::Cache;
require Game::EvonyTKR::Shared::Constants;
require Game::EvonyTKR;
require Game::EvonyTKR::Model::Base;
require Game::EvonyTKR::Model::AscendingAttributes;
require Game::EvonyTKR::Model::Covenant;
require Game::EvonyTKR::Model::Book;
require Game::EvonyTKR::Model::Buff;
require Game::EvonyTKR::Model::General;
require Game::EvonyTKR::Model::Specialty;
require YAML::PP;

use Log::Log4perl qw(:levels);

package Test::Package {
  use Mojo::Base 'Game::EvonyTKR::Model::Base';
  use Mojo::Base 'Game::EvonyTKR::Role::Logging',                         -role;
  use Mojo::Base 'Game::EvonyTKR::Controller::Role::AscendingAttributes', -role;
  use Mojo::Base 'Game::EvonyTKR::Controller::Role::Books',               -role;
  use Mojo::Base 'Game::EvonyTKR::Controller::Role::Covenants',           -role;
  use Mojo::Base 'Game::EvonyTKR::Controller::Role::Generals',            -role;
  use Mojo::Base 'Game::EvonyTKR::Controller::Role::Specialties',         -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::BuffConstants',        -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::GeneralConstants',     -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::Books',                -role;
  use List::AllUtils qw( first );

  sub import_generic_books ($self) {
    my $suffixlist = ['.yaml', '.yml'];
    my @books;
    my $collectionDir =
      Mojo::File->new(Mojo::Home->new()->to_string())
      ->child('share/collections/data/');
    my $gbdir = $collectionDir->child('generic books');

    $self->logger->debug(
      sprintf('found %s generic book files', $gbdir->list->size));

    foreach my $book_name ($self->list_generic_books->@*) {
      $self->logger->debug("importing $book_name");
      my $bn = lc($self->normalize($book_name));

      my ($file) = $gbdir->list->sort->grep(sub {
        my $b = lc($self->normalize($_->basename(@$suffixlist)));
        if ($_ =~ m/\.ya?ml$/ && $b eq $bn) {
          return 1;
        }
        return 0;
      })->head(1)->each;

      unless ($file && -f $file) {
        $self->logger->error(
          sprintf('failed to find file for "%s"', $book_name));
        return 0;
      }

      my $data       = Mojo::File->new($file)->slurp('UTF-8');
      my $hashObject = YAML::PP->new(
        schema       => [qw/ + Perl /],
        yaml_version => ['1.2', '1.1'],
      )->load_string($data);
      my $book = Game::EvonyTKR::Model::Book->from_hash($hashObject);
      unless ($book
        && ref($book)
        && blessed($book)
        && $book->isa('Game::EvonyTKR::Model::Book')) {
        $self->logger->error(
          sprintf(
            'failed to create a book with from_hash for "%s"', $book_name
          )
        );
        return 0;
      }
      $self->add_generic_book($book);
      push @books, $book;
    }
    return \@books;
  }

  sub import_builtin_books ($self) {
    my $suffixlist = ['.yaml', '.yml'];
    my @books;
    my $collectionDir =
      Mojo::File->new(Mojo::Home->new()->to_string())
      ->child('share/collections/data/');
    my $bbdir = $collectionDir->child('skill books');

    $self->logger->debug(
      sprintf('found %s builtin book files', $bbdir->list->size));

    foreach my $book_name ($self->list_builtin_books->@*) {
      $self->logger->debug("importing $book_name");
      my $bn = lc($self->normalize($book_name));

      my ($file) = $bbdir->list->sort->grep(sub {
        my $b = lc($self->normalize($_->basename(@$suffixlist)));
        if ($_ =~ m/\.ya?ml$/ && $b eq $bn) {
          return 1;
        }
        return 0;
      })->head(1)->each;

      unless ($file && -f $file) {
        $self->logger->error(
          sprintf('failed to find file for "%s"', $book_name));
        return 0;
      }

      my $data       = Mojo::File->new($file)->slurp('UTF-8');
      my $hashObject = YAML::PP->new(
        schema       => [qw/ + Perl /],
        yaml_version => ['1.2', '1.1'],
      )->load_string($data);
      my $book = Game::EvonyTKR::Model::Book->from_hash($hashObject);
      unless ($book
        && ref($book)
        && blessed($book)
        && $book->isa('Game::EvonyTKR::Model::Book')) {
        $self->logger->error(
          sprintf(
            'failed to create a book with from_hash for "%s"', $book_name
          )
        );
        return 0;
      }
      $self->add_builtin_book($book);
      push @books, $book;
    }
    return \@books;
  }

  sub import_ascendingAttributes ($self) {
    my $suffixlist = ['.yaml', '.yml'];
    my @objects;
    my $collectionDir =
      Mojo::File->new(Mojo::Home->new()->to_string())
      ->child('share/collections/data/');
    my $itemDir = $collectionDir->child('ascending attributes');

    $self->logger->debug(sprintf('found %s files', $itemDir->list->size));

    foreach my $item ($self->list_ascending_attributes->@*) {
      $self->logger->debug("importing $item");
      my $nn = lc($self->normalize($item));

      my ($file) = $itemDir->list->sort->grep(sub {
        my $i = lc($self->normalize($_->basename(@$suffixlist)));
        if ($_ =~ m/\.ya?ml$/ && $i eq $nn) {
          return 1;
        }
        return 0;
      })->head(1)->each;

      unless ($file && -f $file) {
        $self->logger->error(sprintf('failed to find file for "%s"', $item));
        return 0;
      }

      my $data       = Mojo::File->new($file)->slurp('UTF-8');
      my $hashObject = YAML::PP->new(
        schema       => [qw/ + Perl /],
        yaml_version => ['1.2', '1.1'],
      )->load_string($data);
      my $object =
        Game::EvonyTKR::Model::AscendingAttributes->from_hash($hashObject);
      unless ($object
        && ref($object)
        && blessed($object)
        && $object->isa('Game::EvonyTKR::Model::AscendingAttributes')) {
        $self->logger->error(
          sprintf('failed to create an object with from_hash for "%s"', $item));
        return 0;
      }
      $self->add_ascending_attribute($object);
      push @objects, $object;
    }
    return \@objects;
  }

  sub import_generals ($self) {
    my $suffixlist = ['.yaml', '.yml'];
    my @objects;
    my $collectionDir =
      Mojo::File->new(Mojo::Home->new()->to_string())
      ->child('share/collections/data/');
    my $itemDir = $collectionDir->child('generals');

    $self->logger->debug(sprintf('found %s files', $itemDir->list->size));

    foreach my $item ($self->list_generals->@*) {
      $self->logger->debug("importing $item");
      my $nn = lc($self->normalize($item));

      my ($file) = $itemDir->list->sort->grep(sub {
        my $i = lc($self->normalize($_->basename(@$suffixlist)));
        if ($_ =~ m/\.ya?ml$/ && $i eq $nn) {
          return 1;
        }
        return 0;
      })->head(1)->each;

      unless ($file && -f $file) {
        $self->logger->error(sprintf('failed to find file for "%s"', $item));
        return 0;
      }

      my $data       = Mojo::File->new($file)->slurp('UTF-8');
      my $hashObject = YAML::PP->new(
        schema       => [qw/ + Perl /],
        yaml_version => ['1.2', '1.1'],
      )->load_string($data);
      my $object = Game::EvonyTKR::Model::General->from_hash($hashObject);
      unless ($object
        && ref($object)
        && blessed($object)
        && $object->isa('Game::EvonyTKR::Model::General')) {
        $self->logger->error(
          sprintf('failed to create an object with from_hash for "%s"', $item));
        return 0;
      }
      $self->add_general($object);
      push @objects, $object;
    }
    return \@objects;
  }

  sub import_covenants ($self) {
    my $suffixlist = ['.yaml', '.yml'];
    my @objects;
    my $collectionDir =
      Mojo::File->new(Mojo::Home->new()->to_string())
      ->child('share/collections/data/');
    my $itemDir = $collectionDir->child('covenants');

    $self->logger->debug(sprintf('found %s files', $itemDir->list->size));

    foreach my $item ($self->list_covenants->@*) {
      $self->logger->debug("importing $item");
      my $nn = lc($self->normalize($item));

      my ($file) = $itemDir->list->sort->grep(sub {
        my $i = lc($self->normalize($_->basename(@$suffixlist)));
        if ($_ =~ m/\.ya?ml$/ && $i eq $nn) {
          return 1;
        }
        return 0;
      })->head(1)->each;

      unless ($file && -f $file) {
        $self->logger->error(sprintf('failed to find file for "%s"', $item));
        return 0;
      }

      my $data       = Mojo::File->new($file)->slurp('UTF-8');
      my $hashObject = YAML::PP->new(
        schema       => [qw/ + Perl /],
        yaml_version => ['1.2', '1.1'],
      )->load_string($data);
      my $object = Game::EvonyTKR::Model::Covenant->from_hash($hashObject);
      unless ($object
        && ref($object)
        && blessed($object)
        && $object->isa('Game::EvonyTKR::Model::Covenant')) {
        $self->logger->error(
          sprintf('failed to create an object with from_hash for "%s"', $item));
        return 0;
      }
      $self->add_covenant($object);
      push @objects, $object;
    }
    return \@objects;
  }

  sub import_specialties ($self) {
    my $suffixlist = ['.yaml', '.yml'];
    my @objects;
    my $collectionDir =
      Mojo::File->new(Mojo::Home->new()->to_string())
      ->child('share/collections/data/');
    my $itemDir = $collectionDir->child('specialties');

    $self->logger->debug(sprintf('found %s files', $itemDir->list->size));

    foreach my $item ($self->list_specialties->@*) {
      $self->logger->debug("importing $item");
      my $nn = lc($self->normalize($item));

      my ($file) = $itemDir->list->sort->grep(sub {
        my $i = lc($self->normalize($_->basename(@$suffixlist)));
        if ($_ =~ m/\.ya?ml$/ && $i eq $nn) {
          return 1;
        }
        return 0;
      })->head(1)->each;

      unless ($file && -f $file) {
        $self->logger->error(sprintf('failed to find file for "%s"', $item));
        return 0;
      }

      my $data       = Mojo::File->new($file)->slurp('UTF-8');
      my $hashObject = YAML::PP->new(
        schema       => [qw/ + Perl /],
        yaml_version => ['1.2', '1.1'],
      )->load_string($data);
      my $object = Game::EvonyTKR::Model::Specialty->from_hash($hashObject);
      unless ($object
        && ref($object)
        && blessed($object)
        && $object->isa('Game::EvonyTKR::Model::Specialty')) {
        $self->logger->error(
          sprintf('failed to create an object with from_hash for "%s"', $item));
        return 0;
      }
      $self->add_specialty($object);
      push @objects, $object;
    }
    return \@objects;
  }

}

my $testManager = Test::Package->new();
$testManager->logger->info('starting testing.');
isa_ok($testManager, ['Test::Package'], 'Test Package instantiated');

my $generals = $testManager->import_generals();
ok(ref($generals) && scalar(@$generals),
  sprintf('imported %s generals', ref($generals) ? scalar(@$generals) : 0));

my $ascending_attributes = $testManager->import_ascendingAttributes();
ok(
  ref($ascending_attributes) && scalar(@$ascending_attributes),
  sprintf('imported %s ascending_attributes',
    ref($ascending_attributes) ? scalar(@$ascending_attributes) : 0)
);

my $covenants = $testManager->import_covenants();
ok(ref($covenants) && scalar(@$covenants),
  sprintf('imported %s covenants', ref($covenants) ? scalar(@$covenants) : 0));

my $generic_books = $testManager->import_generic_books();
ok(
  ref($generic_books) && scalar(@$generic_books),
  sprintf('imported %s generic books',
    ref($generic_books) ? scalar(@$generic_books) : 0)
);

my $builtin_books = $testManager->import_builtin_books();
ok(
  ref($builtin_books) && scalar(@$builtin_books),
  sprintf('imported %s builtin books',
    ref($builtin_books) ? scalar(@$builtin_books) : 0)
);

my $specialties = $testManager->import_specialties();
ok(
  ref($specialties) && scalar(@$specialties),
  sprintf('imported %s specialties',
    ref($specialties) ? scalar(@$specialties) : 0)
);

subtest 'Populate Ascending Attributes' => sub {
  foreach my $general (@$generals) {
    my $pa = $general->populateAscendingAttributes();

    if ($general->ascending) {
      ok($pa && $pa == 1,
        sprintf('populated ascending attributes for "%s"', $general->name));
    }
    else {
      ok(
        !$pa || $pa == 0,
        sprintf(
          'attempt to populate ascending attributes for "%s" correctly failed',
          $general->name)
      );
    }
  }
  done_testing();
};

subtest 'Populate Builtin Books' => sub {
  foreach my $general (@$generals) {
    my $result = $general->populateBuiltinBook();
    ok($result && $result == 1,
      sprintf('"%s" populated correctly', $general->name));
  }
  done_testing();
};

subtest 'Populate Specialties' => sub {
  foreach my $general (@$generals) {
    my $result = $general->populateSpecialties();
    ok($result && $result == 1,
      sprintf('"%s" populated correctly', $general->name));
  }
  done_testing();
};

# Get Marco Polo general
my $marco_polo = first { $_->name eq 'Marco Polo' } $generals->@*;
ok(defined $marco_polo, "Marco Polo general loaded");

my $aethelflaed = first { $_->name eq 'Aethelflaed' } $generals->@*;
ok(defined $aethelflaed, "Aethelflaed general loaded");

my $Harald = first { $_->name eq 'Harald' } $generals->@*;
ok(defined $Harald, "Harald general loaded");

# Test case: All values set to 'none'
subtest "Marco Polo with all values set to 'none'" => sub {

  my $covenant =
    first { $_->primary->name eq $marco_polo->name } $covenants->@*;
  my $summarizer = Game::EvonyTKR::Model::Buff::Summarizer->new(
    general             => $marco_polo,
    books               => [$generic_books->@*, $builtin_books->@*],
    covenant            => $covenant,
    ascendingAttributes => $marco_polo->ascendingAttributes,
    isPrimary           => 1,
    targetType          => 'mounted_specialist',
    activationType      => 'Attacking',
    ascendingLevel      => 'none',
    covenantLevel       => 'none',
    specialty1          => 'none',
    specialty2          => 'none',
    specialty3          => 'none',
    specialty4          => 'none',
  );

  $summarizer->updateBuffs();
  $summarizer->updateDebuffs();

  # Test Buff values for different troop types
  is(
    $summarizer->buffValues->{'Ground Troops'},
    {
      'March Size' => 12,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Ground troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Mounted Troops'},
    {
      'March Size' => 12,
      'Attack'     => 70,
      'Defense'    => 65,
      'HP'         => 65
    },
    "Mounted troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Ranged Troops'},
    {
      'March Size' => 12,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Ranged troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Siege Machines'},
    {
      'March Size' => 12,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Siege Machines buffs should match expected values"
  );

  # Test Debuff values for different troop types
  is(
    $summarizer->debuffValues,
    {
      'Ground Troops'  => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Mounted Troops' => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Ranged Troops'  => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Siege Machines' => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Overall'        => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
    },
    "No debuffs should be present"
  );
};

# Test case: All values set to 'none'
subtest "Aethelflaed with all values set to 'none'" => sub {

  my $covenant =
    first { $_->primary->name eq $aethelflaed->name } $covenants->@*;
  my $summarizer = Game::EvonyTKR::Model::Buff::Summarizer->new(
    general             => $aethelflaed,
    books               => [$generic_books->@*, $builtin_books->@*],
    covenant            => $covenant,
    ascendingAttributes => $aethelflaed->ascendingAttributes,
    isPrimary           => 1,
    targetType          => 'mounted_specialist',
    activationType      => 'PvM',
    ascendingLevel      => 'none',
    covenantLevel       => 'none',
    specialty1          => 'none',
    specialty2          => 'none',
    specialty3          => 'none',
    specialty4          => 'none',
  );

  $summarizer->updateBuffs();
  $summarizer->updateDebuffs();

  # Test Buff values for different troop types
  is(
    $summarizer->buffValues->{'Ground Troops'},
    {
      'March Size' => 12,
      'Attack'     => 70,
      'Defense'    => 70,
      'HP'         => 70
    },
    "Ground troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Mounted Troops'},
    {
      'March Size' => 12,
      'Attack'     => 80,
      'Defense'    => 125,
      'HP'         => 70
    },
    "Mounted troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Ranged Troops'},
    {
      'March Size' => 12,
      'Attack'     => 70,
      'Defense'    => 70,
      'HP'         => 70
    },
    "Ranged troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Siege Machines'},
    {
      'March Size' => 12,
      'Attack'     => 70,
      'Defense'    => 70,
      'HP'         => 70
    },
    "Siege Machines buffs should match expected values"
  );

  # Test Debuff values for different troop types
  is(
    $summarizer->debuffValues,
    {
      'Ground Troops'  => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Mounted Troops' => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Ranged Troops'  => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Siege Machines' => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Overall'        => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
    },
    "No debuffs should be present"
  );
};

# Additional test cases can be added here for other combinations
# For example:

# Test case: Red1 ascending, all else none
subtest "Marco Polo with Red1 ascending, all else none" => sub {

  my $covenant =
    first { $_->primary->name eq $marco_polo->name } $covenants->@*;

  my $summarizer = Game::EvonyTKR::Model::Buff::Summarizer->new(

    general             => $marco_polo,
    books               => [$generic_books->@*, $builtin_books->@*],
    covenant            => $covenant,
    ascendingAttributes => $marco_polo->ascendingAttributes,
    isPrimary           => 1,
    targetType          => 'mounted_specialist',
    activationType      => 'Attacking',
    ascendingLevel      => 'red1',
    covenantLevel       => 'none',
    specialty1          => 'none',
    specialty2          => 'none',
    specialty3          => 'none',
    specialty4          => 'none',
  );

  $summarizer->updateBuffs();
  $summarizer->updateDebuffs();

  # Test Buff values for different troop types
  is(
    $summarizer->buffValues->{'Ground Troops'},
    {
      'March Size' => 12,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Ground troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Mounted Troops'},
    {
      'March Size' => 12,
      'Attack'     => 95,
      'Defense'    => 65,
      'HP'         => 80
    },
    "Mounted troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Ranged Troops'},
    {
      'March Size' => 12,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Ranged troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Siege Machines'},
    {
      'March Size' => 12,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Siege Machines buffs should match expected values"
  );

  # Test Debuff values for different troop types
  is(
    $summarizer->debuffValues,
    {
      'Ground Troops'  => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Mounted Troops' => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Ranged Troops'  => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Siege Machines' => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Overall'        => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
    },
    "No debuffs should be present"
  );
};

# Test case: Red2 ascending, all else none
subtest "Marco Polo with Red2 ascending, all else none" => sub {

  my $covenant =
    first { $_->primary->name eq $marco_polo->name } $covenants->@*;

  my $summarizer = Game::EvonyTKR::Model::Buff::Summarizer->new(

    general             => $marco_polo,
    books               => [$generic_books->@*, $builtin_books->@*],
    covenant            => $covenant,
    ascendingAttributes => $marco_polo->ascendingAttributes,
    isPrimary           => 1,
    targetType          => 'mounted_specialist',
    activationType      => 'Attacking',
    ascendingLevel      => 'red2',
    covenantLevel       => 'none',
    specialty1          => 'none',
    specialty2          => 'none',
    specialty3          => 'none',
    specialty4          => 'none',
  );

  $summarizer->updateBuffs();
  $summarizer->updateDebuffs();

  # Test Buff values for different troop types
  is(
    $summarizer->buffValues->{'Ground Troops'},
    {
      'March Size' => 12,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Ground troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Mounted Troops'},
    {
      'March Size' => 12,
      'Attack'     => 95,
      'Defense'    => 65,
      'HP'         => 80
    },
    "Mounted troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Ranged Troops'},
    {
      'March Size' => 12,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Ranged troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Siege Machines'},
    {
      'March Size' => 12,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Siege Machines buffs should match expected values"
  );

  # Test Debuff values for different troop types
  is(
    $summarizer->debuffValues,
    {
      'Ground Troops'  => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Mounted Troops' => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Ranged Troops'  => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Siege Machines' => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Overall'        => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
    },
    "No debuffs should be present"
  );
};

# Test case: Red3 ascending, all else none
subtest "Marco Polo with Red3 ascending, all else none" => sub {

  my $covenant =
    first { $_->primary->name eq $marco_polo->name } $covenants->@*;

  my $summarizer = Game::EvonyTKR::Model::Buff::Summarizer->new(

    general             => $marco_polo,
    books               => [$generic_books->@*, $builtin_books->@*],
    covenant            => $covenant,
    ascendingAttributes => $marco_polo->ascendingAttributes,
    isPrimary           => 1,
    targetType          => 'mounted_specialist',
    activationType      => 'Attacking',
    ascendingLevel      => 'red3',
    covenantLevel       => 'none',
    specialty1          => 'none',
    specialty2          => 'none',
    specialty3          => 'none',
    specialty4          => 'none',
  );

  $summarizer->updateBuffs();
  $summarizer->updateDebuffs();

  # Test Buff values for different troop types
  is(
    $summarizer->buffValues->{'Ground Troops'},
    {
      'March Size' => 20,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Ground troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Mounted Troops'},
    {
      'March Size' => 20,
      'Attack'     => 125,
      'Defense'    => 65,
      'HP'         => 80
    },
    "Mounted troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Ranged Troops'},
    {
      'March Size' => 20,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Ranged troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Siege Machines'},
    {
      'March Size' => 20,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Siege Machines buffs should match expected values"
  );

  # Test Debuff values for different troop types
  is(
    $summarizer->debuffValues,
    {
      'Ground Troops'  => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Mounted Troops' => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Ranged Troops'  => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Siege Machines' => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Overall'        => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
    },
    "No debuffs should be present"
  );
};

# Test case: Red4 ascending, all else none
subtest "Marco Polo with Red4 ascending, all else none" => sub {

  my $covenant =
    first { $_->primary->name eq $marco_polo->name } $covenants->@*;

  my $summarizer = Game::EvonyTKR::Model::Buff::Summarizer->new(

    general             => $marco_polo,
    books               => [$generic_books->@*, $builtin_books->@*],
    covenant            => $covenant,
    ascendingAttributes => $marco_polo->ascendingAttributes,
    isPrimary           => 1,
    targetType          => 'mounted_specialist',
    activationType      => 'Attacking',
    ascendingLevel      => 'red4',
    covenantLevel       => 'none',
    specialty1          => 'none',
    specialty2          => 'none',
    specialty3          => 'none',
    specialty4          => 'none',
  );

  $summarizer->updateBuffs();
  $summarizer->updateDebuffs();

  # Test Buff values for different troop types
  is(
    $summarizer->buffValues->{'Ground Troops'},
    {
      'March Size' => 20,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Ground troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Mounted Troops'},
    {
      'March Size' => 20,
      'Attack'     => 125,
      'Defense'    => 65,
      'HP'         => 80
    },
    "Mounted troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Ranged Troops'},
    {
      'March Size' => 20,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Ranged troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Siege Machines'},
    {
      'March Size' => 20,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Siege Machines buffs should match expected values"
  );

  # Test Debuff values for different troop types
  is(
    $summarizer->debuffValues,
    {
      'Ground Troops'  => { 'Attack' => 0, 'Defense' => 10, 'HP' => 10 },
      'Mounted Troops' => { 'Attack' => 0, 'Defense' => 10, 'HP' => 10 },
      'Ranged Troops'  => { 'Attack' => 0, 'Defense' => 0,  'HP' => 0 },
      'Siege Machines' => { 'Attack' => 0, 'Defense' => 0,  'HP' => 0 },
      'Overall'        => { 'Attack' => 0, 'Defense' => 0,  'HP' => 0 },
    },
    "Debuffs match expected values."
  );
};

# Test case: Red5 ascending, all else none
subtest "Marco Polo with Red5 ascending, all else none" => sub {

  my $covenant =
    first { $_->primary->name eq $marco_polo->name } $covenants->@*;
  my $summarizer = Game::EvonyTKR::Model::Buff::Summarizer->new(

    general             => $marco_polo,
    books               => [$generic_books->@*, $builtin_books->@*],
    covenant            => $covenant,
    ascendingAttributes => $marco_polo->ascendingAttributes,
    isPrimary           => 1,
    targetType          => 'mounted_specialist',
    activationType      => 'Attacking',
    ascendingLevel      => 'red5',
    covenantLevel       => 'none',
    specialty1          => 'none',
    specialty2          => 'none',
    specialty3          => 'none',
    specialty4          => 'none',
  );

  $summarizer->updateBuffs();
  $summarizer->updateDebuffs();

  # Test Buff values for different troop types
  is(
    $summarizer->buffValues->{'Ground Troops'},
    {
      'March Size' => 20,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Ground troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Mounted Troops'},
    {
      'March Size' => 20,
      'Attack'     => 165,
      'Defense'    => 90,
      'HP'         => 105
    },
    "Mounted troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Ranged Troops'},
    {
      'March Size' => 20,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Ranged troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Siege Machines'},
    {
      'March Size' => 20,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Siege Machines buffs should match expected values"
  );

  # Test Debuff values for different troop types
  is(
    $summarizer->debuffValues,
    {
      'Ground Troops'  => { 'Attack' => 0, 'Defense' => 10, 'HP' => 10 },
      'Mounted Troops' => { 'Attack' => 0, 'Defense' => 10, 'HP' => 10 },
      'Ranged Troops'  => { 'Attack' => 0, 'Defense' => 0,  'HP' => 0 },
      'Siege Machines' => { 'Attack' => 0, 'Defense' => 0,  'HP' => 0 },
      'Overall'        => { 'Attack' => 0, 'Defense' => 0,  'HP' => 0 },
    },
    "Debuffs match expected values."
  );
};

# Test case: Green 1st specialty, all else none
subtest "Marco Polo with Green 1st specialty, all else none" => sub {

  my $covenant =
    first { $_->primary->name eq $marco_polo->name } $covenants->@*;
  my $summarizer = Game::EvonyTKR::Model::Buff::Summarizer->new(

    general        => $marco_polo,
    isPrimary      => 1,
    targetType     => 'mounted_specialist',
    activationType => 'Attacking',
    ascendingLevel => 'none',
    covenantLevel  => 'none',
    specialty1     => 'green',
    specialty2     => 'none',
    specialty3     => 'none',
    specialty4     => 'none',
  );

  $summarizer->updateBuffs();
  $summarizer->updateDebuffs();

  # Test Buff values for different troop types
  is(
    $summarizer->buffValues->{'Ground Troops'},
    {
      'March Size' => 12,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Ground troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Mounted Troops'},
    {
      'March Size' => 12,
      'Attack'     => 71,
      'Defense'    => 66,
      'HP'         => 65
    },
    "Mounted troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Ranged Troops'},
    {
      'March Size' => 12,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Ranged troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Siege Machines'},
    {
      'March Size' => 12,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Siege Machines buffs should match expected values"
  );

  # Test Debuff values for different troop types
  is(
    $summarizer->debuffValues,
    {
      'Ground Troops'  => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Mounted Troops' => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Ranged Troops'  => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Siege Machines' => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Overall'        => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
    },
    "Debuffs match expected values."
  );
};

# Test case: Blue 1st specialty, all else none
subtest "Marco Polo with Blue 1st specialty, all else none" => sub {

  my $covenant =
    first { $_->primary->name eq $marco_polo->name } $covenants->@*;
  my $summarizer = Game::EvonyTKR::Model::Buff::Summarizer->new(

    general             => $marco_polo,
    books               => [$generic_books->@*, $builtin_books->@*],
    covenant            => $covenant,
    ascendingAttributes => $marco_polo->ascendingAttributes,
    isPrimary           => 1,
    targetType          => 'mounted_specialist',
    activationType      => 'Attacking',
    ascendingLevel      => 'none',
    covenantLevel       => 'none',
    specialty1          => 'blue',
    specialty2          => 'none',
    specialty3          => 'none',
    specialty4          => 'none',
  );

  $summarizer->updateBuffs();
  $summarizer->updateDebuffs();

  # Test Buff values for different troop types
  is(
    $summarizer->buffValues->{'Ground Troops'},
    {
      'March Size' => 12,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Ground troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Mounted Troops'},
    {
      'March Size' => 12,
      'Attack'     => 72,
      'Defense'    => 67,
      'HP'         => 65
    },
    "Mounted troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Ranged Troops'},
    {
      'March Size' => 12,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Ranged troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Siege Machines'},
    {
      'March Size' => 12,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Siege Machines buffs should match expected values"
  );

  # Test Debuff values for different troop types
  is(
    $summarizer->debuffValues,
    {
      'Ground Troops'  => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Mounted Troops' => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Ranged Troops'  => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Siege Machines' => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Overall'        => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
    },
    "Debuffs match expected values."
  );
};

# Test case: Purple 1st specialty, all else none
subtest "Marco Polo with Purple 1st specialty, all else none" => sub {

  my $covenant =
    first { $_->primary->name eq $marco_polo->name } $covenants->@*;
  my $summarizer = Game::EvonyTKR::Model::Buff::Summarizer->new(

    general             => $marco_polo,
    books               => [$generic_books->@*, $builtin_books->@*],
    covenant            => $covenant,
    ascendingAttributes => $marco_polo->ascendingAttributes,
    isPrimary           => 1,
    targetType          => 'mounted_specialist',
    activationType      => 'Attacking',
    ascendingLevel      => 'none',
    covenantLevel       => 'none',
    specialty1          => 'purple',
    specialty2          => 'none',
    specialty3          => 'none',
    specialty4          => 'none',
  );

  $summarizer->updateBuffs();
  $summarizer->updateDebuffs();

  # Test Buff values for different troop types
  is(
    $summarizer->buffValues->{'Ground Troops'},
    {
      'March Size' => 12,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Ground troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Mounted Troops'},
    {
      'March Size' => 12,
      'Attack'     => 74,
      'Defense'    => 69,
      'HP'         => 65
    },
    "Mounted troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Ranged Troops'},
    {
      'March Size' => 12,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Ranged troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Siege Machines'},
    {
      'March Size' => 12,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Siege Machines buffs should match expected values"
  );

  # Test Debuff values for different troop types
  is(
    $summarizer->debuffValues,
    {
      'Ground Troops'  => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Mounted Troops' => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Ranged Troops'  => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Siege Machines' => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Overall'        => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
    },
    "Debuffs match expected values."
  );
};

# Test case: Orange 1st specialty, all else none
subtest "Marco Polo with Orange 1st specialty, all else none" => sub {

  my $covenant =
    first { $_->primary->name eq $marco_polo->name } $covenants->@*;
  my $summarizer = Game::EvonyTKR::Model::Buff::Summarizer->new(

    general             => $marco_polo,
    books               => [$generic_books->@*, $builtin_books->@*],
    covenant            => $covenant,
    ascendingAttributes => $marco_polo->ascendingAttributes,
    isPrimary           => 1,
    targetType          => 'mounted_specialist',
    activationType      => 'Attacking',
    ascendingLevel      => 'none',
    covenantLevel       => 'none',
    specialty1          => 'orange',
    specialty2          => 'none',
    specialty3          => 'none',
    specialty4          => 'none',
  );

  $summarizer->updateBuffs();
  $summarizer->updateDebuffs();

  # Test Buff values for different troop types
  is(
    $summarizer->buffValues->{'Ground Troops'},
    {
      'March Size' => 12,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Ground troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Mounted Troops'},
    {
      'March Size' => 12,
      'Attack'     => 76,
      'Defense'    => 71,
      'HP'         => 65
    },
    "Mounted troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Ranged Troops'},
    {
      'March Size' => 12,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Ranged troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Siege Machines'},
    {
      'March Size' => 12,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Siege Machines buffs should match expected values"
  );

  # Test Debuff values for different troop types
  is(
    $summarizer->debuffValues,
    {
      'Ground Troops'  => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Mounted Troops' => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Ranged Troops'  => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Siege Machines' => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Overall'        => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
    },
    "Debuffs match expected values."
  );
};

# Test case: Gold 1st specialty, all else none
subtest "Marco Polo with Gold 1st specialty, all else none" => sub {

  my $covenant =
    first { $_->primary->name eq $marco_polo->name } $covenants->@*;
  my $summarizer = Game::EvonyTKR::Model::Buff::Summarizer->new(

    general             => $marco_polo,
    books               => [$generic_books->@*, $builtin_books->@*],
    covenant            => $covenant,
    ascendingAttributes => $marco_polo->ascendingAttributes,
    isPrimary           => 1,
    targetType          => 'mounted_specialist',
    activationType      => 'Attacking',
    ascendingLevel      => 'none',
    covenantLevel       => 'none',
    specialty1          => 'gold',
    specialty2          => 'none',
    specialty3          => 'none',
    specialty4          => 'none',
  );

  $summarizer->updateBuffs();
  $summarizer->updateDebuffs();

  # Test Buff values for different troop types
  is(
    $summarizer->buffValues->{'Ground Troops'},
    {
      'March Size' => 12,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Ground troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Mounted Troops'},
    {
      'March Size' => 12,
      'Attack'     => 80,
      'Defense'    => 75,
      'HP'         => 65
    },
    "Mounted troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Ranged Troops'},
    {
      'March Size' => 12,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Ranged troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Siege Machines'},
    {
      'March Size' => 12,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Siege Machines buffs should match expected values"
  );

  # Test Debuff values for different troop types
  is(
    $summarizer->debuffValues,
    {
      'Ground Troops'  => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Mounted Troops' => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Ranged Troops'  => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Siege Machines' => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
      'Overall'        => { 'Attack' => 0, 'Defense' => 0, 'HP' => 0 },
    },
    "Debuffs match expected values."
  );
};

# Test case: Civilization covenant, all else none
subtest "Marco Polo with Civilization covenant, all else none" => sub {

  my $covenant =
    first { $_->primary->name eq $marco_polo->name } $covenants->@*;

  my $summarizer = Game::EvonyTKR::Model::Buff::Summarizer->new(

    general             => $marco_polo,
    books               => [$generic_books->@*, $builtin_books->@*],
    covenant            => $covenant,
    ascendingAttributes => $marco_polo->ascendingAttributes,
    isPrimary           => 1,
    targetType          => 'mounted_specialist',
    activationType      => 'Attacking',
    ascendingLevel      => 'none',
    covenantLevel       => 'Civilization',
    specialty1          => 'none',
    specialty2          => 'none',
    specialty3          => 'none',
    specialty4          => 'none',
  );

  $summarizer->updateBuffs();
  $summarizer->updateDebuffs();

  # Add your expected values here
  # This is a placeholder - you'll need to compute these values by hand
  pass("Civilization covenant test placeholder - add actual assertions");
};

# Test case: All maxed out (Red5, Gold specialties, Civilization covenant)
subtest "Marco Polo with all maxed out" => sub {
  my $covenant =
    first { $_->primary->name eq $marco_polo->name } $covenants->@*;
  my $summarizer = Game::EvonyTKR::Model::Buff::Summarizer->new(

    general             => $marco_polo,
    books               => [$generic_books->@*, $builtin_books->@*],
    covenant            => $covenant,
    ascendingAttributes => $marco_polo->ascendingAttributes,
    isPrimary           => 1,
    targetType          => 'mounted_specialist',
    activationType      => 'Attacking',
    ascendingLevel      => 'red5',
    covenantLevel       => 'civilization',
    specialty1          => 'gold',
    specialty2          => 'gold',
    specialty3          => 'gold',
    specialty4          => 'gold',
  );

  $summarizer->updateBuffs();
  $summarizer->updateDebuffs();

  # Add your expected values here
  # Add your expected values here
  is(
    $summarizer->buffValues->{'Ground Troops'},
    {
      'March Size' => 41,
      'Attack'     => 35,
      'Defense'    => 25,
      'HP'         => 25,
    },
    "Ground troop buffs should match expected values"
  );

  is($summarizer->buffValues->{'Mounted Troops'}->{'March Size'},
    41, "Marco Polo Attacking March Size");
  is($summarizer->buffValues->{'Mounted Troops'}->{'Attack'},
    265, "Marco Polo Attacking Attack");
  is($summarizer->buffValues->{'Mounted Troops'}->{'Defense'},
    120, "Marco Polo Attacking Defense");
  is($summarizer->buffValues->{'Mounted Troops'}->{'HP'},
    155, "Marco Polo Attacking HP");

  is(
    $summarizer->buffValues->{'Ranged Troops'},
    {
      'March Size' => 41,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Ranged troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Siege Machines'},
    {
      'March Size' => 41,
      'Attack'     => 25,
      'Defense'    => 25,
      'HP'         => 25
    },
    "Siege Machines buffs should match expected values"
  );

  is(
    $summarizer->debuffValues->{'Ground Troops'},
    {
      'Attack'  => 0,
      'Defense' => 10,
      'HP'      => 10
    },
    "Ground troop debuffs should match expected values"
  );

  is(
    $summarizer->debuffValues->{'Mounted Troops'},
    {
      'Attack'  => 0,
      'Defense' => 10,
      'HP'      => 10
    },
    "Mounted troop debuffs should match expected values"
  );

  is(
    $summarizer->debuffValues->{'Ranged Troops'},
    {
      'Attack'  => 0,
      'Defense' => 0,
      'HP'      => 0
    },
    "Ranged troop debuffs should match expected values"
  );

  is(
    $summarizer->debuffValues->{'Siege Machines'},
    {
      'Attack'  => 0,
      'Defense' => 0,
      'HP'      => 0
    },
    "Siege Machines debuffs should match expected values"
  );

};

subtest "Aethelflaed with all maxed out" => sub {

  my $covenant =
    first { $_->primary->name eq $aethelflaed->name } $covenants->@*;
  my $summarizer = Game::EvonyTKR::Model::Buff::Summarizer->new(

    general             => $aethelflaed,
    books               => [$generic_books->@*, $builtin_books->@*],
    covenant            => $covenant,
    ascendingAttributes => $aethelflaed->ascendingAttributes,
    isPrimary           => 1,
    targetType          => 'mounted_specialist',
    activationType      => 'PvM',
    ascendingLevel      => 'red5',
    covenantLevel       => 'Civilization',
    specialty1          => 'gold',
    specialty2          => 'gold',
    specialty3          => 'gold',
    specialty4          => 'gold',
  );

  $summarizer->updateBuffs();
  $summarizer->updateDebuffs();

  # Add your expected values here
  is(
    $summarizer->buffValues->{'Ground Troops'},
    {
      'March Size' => 12,
      'Attack'     => 100,
      'Defense'    => 140,
      'HP'         => 115
    },
    "Ground troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Mounted Troops'},
    {
      'March Size' => 12,
      'Attack'     => 155,
      'Defense'    => 205,
      'HP'         => 155
    },
    "Mounted troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Ranged Troops'},
    {
      'March Size' => 12,
      'Attack'     => 70,
      'Defense'    => 110,
      'HP'         => 115
    },
    "Ranged troop buffs should match expected values"
  );

  is(
    $summarizer->buffValues->{'Siege Machines'},
    {
      'March Size' => 12,
      'Attack'     => 70,
      'Defense'    => 110,
      'HP'         => 115
    },
    "Siege Machines buffs should match expected values"
  );

  # Test Debuff values for different troop types
  is(
    $summarizer->debuffValues,
    {
      'Ground Troops'  => { 'Attack' => 25, 'Defense' => 10, 'HP' => 0 },
      'Mounted Troops' => { 'Attack' => 25, 'Defense' => 10, 'HP' => 0 },
      'Ranged Troops'  => { 'Attack' => 25, 'Defense' => 10, 'HP' => 0 },
      'Siege Machines' => { 'Attack' => 25, 'Defense' => 10, 'HP' => 0 },
      'Overall'        => { 'Attack' => 25, 'Defense' => 10, 'HP' => 0 },
    },
    "Aethelflaed's specific debuffs should be present"
  );
};

subtest "Harald with all maxed out" => sub {

  my $covenant   = first { $_->primary->name eq $Harald->name } $covenants->@*;
  my $summarizer = Game::EvonyTKR::Model::Buff::Summarizer->new(

    general             => $Harald,
    books               => [$generic_books->@*, $builtin_books->@*],
    covenant            => $covenant,
    ascendingAttributes => $Harald->ascendingAttributes,
    isPrimary           => 1,
    targetType          => 'mayor',
    activationType      => 'Mayor',
    ascendingLevel      => 'red5',
    covenantLevel       => 'Civilization',
    specialty1          => 'gold',
    specialty2          => 'gold',
    specialty3          => 'gold',
    specialty4          => 'gold',
  );

  $summarizer->updateDebuffs();

  # Add your expected values here
  is(
    $summarizer->debuffValues->{'Ground Troops'},
    { 'Attack' => 20, 'Defense' => 40, 'HP' => 20 },
    "Ground troop debuffs should match expected values"
  );

  is(
    $summarizer->debuffValues->{'Mounted Troops'},
    { 'Attack' => 20, 'Defense' => 40, 'HP' => 20 },
    "Mounted troop debuffs should match expected values"
  );

  is(
    $summarizer->debuffValues->{'Ranged Troops'},
    { 'Attack' => 20, 'Defense' => 40, 'HP' => 20 },
    "Ranged troop debuffs should match expected values"
  );

  is(
    $summarizer->debuffValues->{'Siege Machines'},
    { 'Attack' => 20, 'Defense' => 40, 'HP' => 20 },
    "Siege Machines debuffs should match expected values"
  );

};

#
done_testing();
