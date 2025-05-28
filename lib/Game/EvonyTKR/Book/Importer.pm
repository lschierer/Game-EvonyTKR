use v5.40.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require Path::Tiny;
require Game::EvonyTKR::Buff;
require Game::EvonyTKR::Buff::Value;
require Game::EvonyTKR::Book;
require Game::EvonyTKR::Book::Builtin;
require YAML::PP;

class Game::EvonyTKR::Book::Importer : isa(Game::EvonyTKR::Data) {
# PODNAME: Game::EvonyTKR::Book::Importer
  use List::AllUtils qw( any none );
  use namespace::autoclean;
  use Carp;
  use File::FindLib 'lib';
  use overload
    '""'       => \&TO_JSON,
    'fallback' => 0;

  field $input : reader : param;

  field $books : reader = {}

    ADJUST {
    my $iv = Path::Tiny::path($input);
    if ($iv->exists()) {
      if ($iv->is_dir()) {
        $input = $iv;
        $self->logger->trace("$input is a valid input directory");
      }
      else {
        $self->logger->logcroak("$input is not a directory");
      }
    }
    else {
      $self->logger->logcroak("$input does not exist");
    }
  }

  method importAll {
    my @bookFiles = $input->children(qr/\.yaml\z/);

    my $ypp = YAML::PP->new(
      schema       => [qw/ + Perl /],
      yaml_version => ['1.2', '1.1'],
    );

    my $importedBooks = {};

    foreach my $bookFile (@bookFiles) {
      $bookFile =
        Path::Tiny::path(Encode::decode('utf8', $bookFile->stringify()));
      my $bookData   = $bookFile->slurp_utf8;
      my $bookObject = $ypp->load_string($bookData);

      next unless ref $bookObject eq 'HASH' && $bookObject->{name};

      if (!exists($bookObject->{level})) {
        # This is a builtin book
        my $book = Game::EvonyTKR::Book::Builtin->new(
          name => $bookObject->{name},
          (exists $bookObject->{text} ? (text => $bookObject->{text}) : ()),
        );

        # Process buffs if they exist
        if (exists $bookObject->{buff} && ref $bookObject->{buff} eq 'ARRAY') {
          my @buffs;

          foreach my $buffObject (@{ $bookObject->{buff} }) {
            my $value = Game::EvonyTKR::Buff::Value->new(
              number => $buffObject->{value}->{number},
              unit   => $buffObject->{value}->{unit},
            );

            my $buff = Game::EvonyTKR::Buff->new(
              attribute => $buffObject->{attribute},
              value     => $value,
              (
                exists $buffObject->{class} ? (class => $buffObject->{class})
                : ()
              ),
              (
                exists $buffObject->{condition}
                ? (condition => $buffObject->{condition})
                : ()
              ),
            );

            $book->addBuff($buff);
          }
        }

        $importedBooks->{ $bookObject->{name} } = $book;
      }
    }

    return $importedBooks;
  }
};
1;
