use v5.40.0;
use experimental qw(class);
use File::FindLib 'lib';

class Game::EvonyTKR::Web::Model::Generals::Conflicts :
  isa(Game::EvonyTKR::Web::Logger) {
  use Carp;
  use Data::Printer;
  use Devel::Peek;
  use File::Basename;
  use File::ShareDir ':ALL';
  use File::Spec;
  use Util::Any ':all';
  use Game::EvonyTKR::Data;
  use Game::EvonyTKR::General;
  use Game::EvonyTKR::Web::Model::Generals;
  use UUID qw(uuid5);
  use X500::DN;
  use YAML::XS qw{ LoadFile Load };
  use namespace::autoclean;
# PODNAME: Game::EvonyTKR::Web::Model::Generals::Conflicts
# VERSION
  use File::FindLib 'lib';

  field $conflcits : reader;

  field $EvonyData = Game::EvonyTKR::Data->new();

  field $generalModel = Game::EvonyTKR::Web::Model::Generals->new();

  method init() {
    my $yp = YAML::PP::LibYAML->new();

    my $data_location =
      File::Spec->catfile(dist_dir('Game-EvonyTKR'), 'generalConflictGroups');
    while (my $file = glob(File::Spec->catfile($data_location, '*.yaml'))) {
      $self->logger()->trace("start of $file");

      my $cgData = $yp->load_file($file);

      my $name;
      my $filename = basename($file, qr/\.[^.]*/);
      if ($filename eq $cgData->{'name'}) {
        $self->logger()->debug(sprintf('found name %s', $name));
        $name = $1;
      }
      else {
        $self->logger()->error(sprintf(
          '%s has name %s which does not match.',
          $filename, $cgData->{'name'}
        ));
        next;
      }

      my $cg = Game::EvonyTKR::General::Conflicts->new(name => $name);

      my @members = @{ $cgData->{'members'} };
      $self->logger()->debug(sprintf(
        'members are %s, I read in %s.',
        Data::Printer::np $cgData->{'members'},
        Data::Printer::np @members,
      ));

      for my $member (@members) {
        my $general = $generalModel->get_by_id($member);
        if (defined $general) {
          my $classList = blessed $general;
          if (index($classList, 'Game::EvonyTKR::General') != -1) {
            $cg->add_primary_general($general);
          }
          else {
            $self->logger()
              ->error(sprintf(
              'Model returned some %s not a general.', $classList));
          }
        }
        else {
          $self->logger()
            ->warn(sprintf('Model could not find general %s', $member));
        }
      }

      my @books = @{ $cgData->{'books'} };
      $self->logger()->debug(sprintf(
        'books are %s, I read in %s.',
        Data::Printer::np $cgData->{'books'},
        Data::Printer::np @books,
      ));

#todo: similar for loop to add in skill books, once I have a model for them.
# except like others below, books might not exist so check the size of the scalar

      my @others = $cgData->{'others'};
      $self->logger()->debug(sprintf(
        'others are %s, I read in %s.',
        Data::Printer::np $cgData->{'others'},
        Data::Printer::np @others,
      ));

      if (scalar @others > 0) {
        for my $other (@others) {
          my $entry = first { $_->name() eq $other } @$conflcits;
          if (defined $entry) {
            # add both sides of the relationship since I cannot be sure that
            # the other will exist at all when the first is read in
            # the add function ensures no duplicates.
            $entry->add_related_conflict($cg);
            $cg->add_related_conflict($entry);
          }
        }
      }
      push @$conflcits, $cg;
    }
  }

}
1;

__END__

# ABSTRACT: Representation of the set of conflicts between one general and other items within Evony
