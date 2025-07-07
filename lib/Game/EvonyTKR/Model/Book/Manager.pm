use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Encode;
require Data::Printer;
require Path::Tiny;
require Path::Iterator::Rule;
require YAML::PP;
require Game::EvonyTKR::Model::Book;
require Game::EvonyTKR::Model::Buff;
require Game::EvonyTKR::Model::Buff::Value;
use namespace::clean;

class Game::EvonyTKR::Model::Book::Manager : isa(Game::EvonyTKR::Shared::Data) {
  # PODNAME: Game::EvonyTKR::Model::Book::Manager
  use Carp;
  use overload
    'bool'     => sub { my ($self) = @_; $self->_isTrue() },
    'fallback' => 0;

  field $books = {};

  method get_all_books {
    my $values;
    @{$values} = values %{$books};
    return $values;
  }

  method getBook ($name) {
    if (exists $books->{$name}) {
      my $book = $books->{$name};
      $self->logger->debug(sprintf(
        'getBook for "%s" found book "%s" with %s buffs and type %s',
        $name, $book->name, scalar @{ $book->buff },
        blessed($book)
      ));
      return $book;
    }
    $self->logger->debug("getBook for '$name' found no book");
    return 0;
  }

  method importAll ($SourceDir) {
    $SourceDir = Path::Tiny::path($SourceDir);
    if (!$SourceDir->is_dir()) {
      $self->logger->logcroak(
"Game::EvonyTKR::Model::Book::Manager requires a directory, not $SourceDir"
      );
    }
    my $rule = Path::Iterator::Rule->new();
    $rule->name(qr/\.ya?ml$/);
    $rule->file->nonempty;
    my $iter = $rule->iter(
      $SourceDir,
      {
        follow_symlinks => 0,
        sorted          => 1,
      }
    );
    while (defined(my $file = $iter->())) {
      # work around for UTF8 filenames not importing correctly by default.
      $file = Path::Tiny::path(Encode::decode('utf8', $file));
      $self->logger->debug("Book::Manager importing $file");
      my $basename = $file->basename('.yaml');
      my $name     = $basename;

      my $data   = $file->slurp_utf8;
      my $object = YAML::PP->new(
        schema       => [qw/ + Perl /],
        yaml_version => ['1.2', '1.1'],
      )->load_string($data);
      if (exists $object->{name}) {
        $name = $object->{name};
      }

      $self->logger->debug("Creating book '$name' from file $file");
      $books->{$name} = Game::EvonyTKR::Model::Book->new(
        name => $name,
        text => $object->{text} // '',
      );

      my $buffCount = 0;
      if (exists $object->{buff} && ref($object->{buff}) eq 'ARRAY') {
        $buffCount = scalar @{ $object->{buff} };
      }
      $self->logger->debug("Book '$name' has $buffCount buffs in YAML");

      foreach my $ob (@{ $object->{buff} }) {
        $self->logger->debug("Processing buff for book '$name': "
            . "attribute="
            . ($ob->{attribute} // 'undef')
            . ", number="
            . ($ob->{value}->{number} // 'undef')
            . ", unit="
            . ($ob->{value}->{unit} // 'undef'));

        my $v = Game::EvonyTKR::Model::Buff::Value->new(
          number => abs($ob->{value}->{number}),
          unit   => $ob->{value}->{unit},
        );
        my $b;
        $b = Game::EvonyTKR::Model::Buff->new(
          value     => $v,
          attribute => $ob->{attribute},
        );

        if (exists $ob->{targetedType}) {
          $b->set_target($ob->{targetedType});
        }

        if (exists $ob->{condition}) {
          foreach my $c (@{ $ob->{condition} }) {
            $b->set_condition($c);
          }
        }
        $books->{$name}->addBuff($b);
        $self->logger->debug("Added buff for attribute '"
            . $ob->{attribute}
            . "' to book '$name', now has "
            . scalar @{ $books->{$name}->buff }
            . " buffs");
      }

      $self->logger->debug("Finished importing book '$name' with "
          . scalar @{ $books->{$name}->buff }
          . " buffs");
    }
    my $countImported = scalar keys %$books;
    $self->logger->info(
      "Game::EvonyTKR::Model::Book::Manager imported $countImported books");
    return $countImported;
  }
}
1;
