use v5.40;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require File::Share;
require JSON::PP;
require YAML::PP;
require MIME::Base64;
require Path::Tiny;
require Game::EvonyTKR;
require Game::EvonyTKR::Shared::Constants;
require Game::EvonyTKR::Model::General;

class Game::EvonyTKR::External::Common : isa(Game::EvonyTKR::Shared::Constants)
{
  use Unicode::Normalize;
  use Unicode::CaseFold qw(fc);
  use Encode            qw(is_utf8 decode_utf8 encode_utf8);
  use Carp;

  field $app : param;
  field $tasks : reader = {};
  field $collectionDir =
    Mojo::File->new($app->config('distDir'))->child('collections/data/');

  field $generals : reader = {};
  #there needs to be a reader so child classes can see it.
  field $conflictDetector : reader =
    Game::EvonyTKR::Model::General::Conflict::Book->new(
    build_index      => 1,
    asst_has_dragon  => 1,
    asst_has_spirit  => 1,
    allow_wall_buffs => 1,
    );

  method load_builtinBook ($bookName, $generalName) {
    my $bookDir = $collectionDir->child('skill books');
    my ($bookFile) = grep {
      my $nf = $self->normalize($_->basename('.yaml'));
      my $nn = $self->normalize($bookName);
      $nf eq $nn;
    } $bookDir->list_tree->grep(sub {qr/\.y\{a\}?ml$/})->each;
    unless (defined($bookFile)) {
      $self->logger->ERR(sprintf('no yaml file found for "%s"', $bookName));
      return;
    }
    my $bd  = $bookFile->slurp('UTF-8');
    my $bho = YAML::PP->new(
      schema       => [qw/ + Perl /],
      yaml_version => ['1.2', '1.1'],
    )->load_string($bd);
    my $book = Game::EvonyTKR::Model::Book::Builtin->from_hash($bho);

    unless ($book
      && Scalar::Util::blessed($book)
      && $book->isa('Game::EvonyTKR::Model::Book::Builtin')) {
      $self->logger->ERR(sprintf(
        'failed to import book for file "%s", necessary for general "%s"',
        $bookFile, $generalName
      ));
      return;
    }
    return $book;
  }

  method load_single_general ($generalFile, $index) {
    $self->logger->DEBUG("processing $generalFile, file # $index");
    my $data       = $generalFile->slurp('UTF-8');
    my $hashObject = YAML::PP->new(
      schema       => [qw/ + Perl /],
      yaml_version => ['1.2', '1.1'],
    )->load_string($data);
    my $g = Game::EvonyTKR::Model::General->from_hash($hashObject);
    unless ($g) {
      $self->logger->ERR(sprintf(
        'failed to build general from %s', $generalFile));
      return undef;
    }

    my $bb = $self->load_builtinBook($g->builtInBookName, $g->name);
    unless ($bb) {
      $self->logger->ERR(
        sprintf('No Builtin Book for %s available.', $g->name));
      return;
    }
    $g->set_builtInBook($bb);
    $generals->{ $self->normalize($g->name) } = $g;
    $self->logger->DEBUG(sprintf('imported general %s.', $g->name));
  }

  method load_generals ($taskName) {
    $self->logger->DEBUG(
      sprintf('starting load_generals with %s generals present',
        scalar(keys $generals->%*))
    );
    my $ypp = YAML::PP->new(
      schema       => [qw/ + Perl /],
      yaml_version => ['1.2', '1.1'],
    );

    my $generalsDir   = $collectionDir->child('generals');
    my $expectedTotal = 0;
    $generalsDir->list_tree->grep(sub {qr/\.y\{a\}?ml$/})->each(
      sub($generalFile, $index) {
        $expectedTotal++;
        $self->load_single_general($generalFile, $index);
        $self->logger->DEBUG(sprintf('there are now %s generals loaded',
          scalar(keys $generals->%*)));
      }
    );
    if (scalar(keys $generals->%*) ne $expectedTotal) {
      $self->logger->ERR(sprintf(
        'loaded count %s does not equal expected count %s.',
        scalar(keys $generals->%*),
        $expectedTotal
      ));
    }
    $self->logger->INFO(sprintf(
      'imported %s generals for task %s',
      scalar(keys $generals->%*), $taskName
    ));
  }
}
1;
