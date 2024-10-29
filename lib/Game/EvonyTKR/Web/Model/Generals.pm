use v5.40.0;
use experimental qw(class);
use File::FindLib 'lib';
require Game::EvonyTKR::Data;
use YAML::PP;

class Game::EvonyTKR::Web::Model::Generals : isa(Game::EvonyTKR::Data) {
  use Carp;
  use Data::Printer;
  use Devel::Peek;
  use File::ShareDir ':ALL';
  use File::Spec;
  use Util::Any ':all';
  use Game::EvonyTKR::General;
  use Game::EvonyTKR::General::Ground;
  use Game::EvonyTKR::General::Mounted;
  use Game::EvonyTKR::General::Ranged;
  use Game::EvonyTKR::General::Siege;
  use X500::RDN;
  use UUID qw(uuid5);
  use namespace::autoclean;
# PODNAME: Game::EvonyTKR::Web::Model::General
# VERSION
  use File::FindLib 'lib';

  field $generals : reader;

  ADJUST {
    for my $generalType ($self->GeneralKeys()) {
      $generals->{$generalType} = {};
    }
  }

  method add_general($ng) {
    my $gc  = blessed($ng);
    my @gcl = split(/::/, $gc);
    if ($gcl[2] !~ /general/i) {
      $self->logger()
        ->logwarn(
        "provided object is of type '$gc' not 'Game::EvonyTKR::General '");
      return 0;
    }
    my $name        = $ng->name();
    my $generalType = $ng->generalType();
    my $base = $self->UUID5_Generals()->{$generalType};
    $self->logger()->trace(sprintf('uuidbase for %s is %s', $generalType, $base));
    my $uuid = uuid5($base, $name);

    if (not exists($generals->{$generalType})) {
      $self->logger()->logcroak("$generalType is not already in the hash");
    }
    else {
      if (not exists($generals->{$generalType}->{$uuid})) {
        $generals->{$generalType}->{$uuid} = $ng;

        if (not exists $generals->{$uuid}) {
          $generals->{$uuid} = [$generalType];
        }
        else {
          push @{ $generals->{$uuid} }, $generalType;
        }
        return 1;
      }
    }

    return 0;
  }

  method remove_general($id) {
    my $uv = UUID::version($id);
    if($uv eq 'v5'){
      for my $t (@{$generals->{$id}}) {
        delete $generals->{$t}->{$id};
      }
      delete $generals->{$id};
    }
    else {
      $self->logger()->logerror(sprintf('$id %s is %s, not a UUID5', $id, $uv));
    }
  }

  method get_by_id($id, $type = undef) {
    if(exists $generals->{$id}) {
      if(defined($type)) {
        $self->logger()->trace(sprintf('looking for cached general %s with type %s',
        $id, defined $type ? $type : 'undefined'));
        if (any { $_ =~ /$type/ } @{ $generals->{$id} }) {
          return $generals->{$type}->{$id};
        }
      }
      else {
        if(defined $generals->{$id}) {
          $type = $$generals->{$id};
          return $generals->{$type}->{$id};
        }
      }
    }
    return undef;
  }

  method UUID_for_name($name, $type = undef) {
    if(defined($type)) {
      $self->logger()->trace(sprintf('obtaining base for type %s', $type));
      my %ug = $self->UUID5_Generals();
      $self->logger()->trace(sprintf('UUID5_Generals is %s',
      Data::Printer::np %ug));
      my $base = $ug{$type};
      $self->logger()->trace(sprintf('base for %s is %s', $type, $base));
      return uuid5($base, $name);
    }
  }

  method get_all_by_type($type) {
    if (exists $generals->{$type}) {
      my $g   = $generals->{$type};
      my $gc  = blessed $g;
      my @gcl = split(/::/, $gc);
      if ($gcl[2] =~ /general/i) {
        $self->logger->logwarn(
          "$type refers to a single of general, not a type of general");
        return {};
      }
      else {
        return $g;
      }
    }
  }


  method readFromFile($name) {
    my $ypp      = YAML::PP->new(boolean => 'JSON::PP');
    my $fileName = $name . '.yaml';
    my $generalShare =
      File::Spec->catfile(dist_dir('Game-EvonyTKR'), 'generals');
    my $FileWithPath = File::Spec->catfile($generalShare, $fileName);
    if (-T -s -r $FileWithPath) {
      $self->logger()->debug("$fileName exists as expected");
      my $yamlData = $ypp->load_file($FileWithPath);

      my $leadership =
        $yamlData->{'general'}->{'basic_attributes'}->{'leadership'}->{'base'};
      my $leadership_increment =
        $yamlData->{'general'}->{'basic_attributes'}->{'leadership'}
        ->{'increment'};
      my $attack =
        $yamlData->{'general'}->{'basic_attributes'}->{'attack'}->{'base'};
      my $attack_increment =
        $yamlData->{'general'}->{'basic_attributes'}->{'attack'}->{'increment'};
      my $defense =
        $yamlData->{'general'}->{'basic_attributes'}->{'defense'}->{'base'};
      my $defense_increment =
        $yamlData->{'general'}->{'basic_attributes'}->{'defense'}
        ->{'increment'};
      my $politics =
        $yamlData->{'general'}->{'basic_attributes'}->{'politics'}->{'base'};
      my $politics_increment =
        $yamlData->{'general'}->{'basic_attributes'}->{'politics'}
        ->{'increment'};

      $self->logger()->trace(sprintf(
        'for %s: leadership: %d, li: %d, attack: %d, ai: %d, defense: %d, di: %d, politics: %d, pi: %d',
        $name,              $leadership,       $leadership_increment,
        $attack,            $attack_increment, $defense,
        $defense_increment, $politics,         $politics_increment,
      ));

      my @SpecialityNames = @{ $yamlData->{'general'}->{'specialities'} };
      my @otherBookNames;
      if (exists $yamlData->{'general'}->{'extra'}) {
        push @otherBookNames, @{ $yamlData->{'general'}->{'extra'} };
      }

      $self->logger()->trace(sprintf(
        'ascending for %s is %s',
        $name, $yamlData->{'general'}->{'ascending'},
      ));
      my $ascending = $yamlData->{'general'}->{'ascending'};

      my @generalClassKey;
      my @scoreType = @{ $yamlData->{'general'}->{'type'} };
      for my $gt (
        qw(ground_specialist mounted_specialist ranged_specialist siege_specialist mayor officer )
      ) {
        my $first = first { index($_, $gt) != -1 } @scoreType;
        if (defined $first) {
          push @generalClassKey, $first;
        }
      }

      if (scalar @generalClassKey != scalar @scoreType) {
        $self->logger()->error(sprintf(
          '%s is of unknown general type %s.',
          $yamlData->{'general'}->{'name'},
          Data::Printer::np @scoreType
        ));
      }
      my %constructors = $self->generalClass();
      for (@generalClassKey) {
        if (not defined $constructors{$_}) {
          $self->logger()->error(sprintf(
            'constructor for %s does not exist creating generals for %s',
            $_, $name
          ));
          next;
        }
        else {
          $self->logger()
            ->trace(sprintf('using constructor %s for %s', $_, $name));
        }
        my $general =
          $constructors{$_}->new(name => $yamlData->{'general'}->{'name'},);

        $general->basic_attributes()->attack()->setBase($attack);
        $general->basic_attributes()->attack()->setIncrement($attack_increment);
        $general->basic_attributes()->leadership()->setBase($leadership);
        $general->basic_attributes()->leadership()
          ->setIncrement($leadership_increment);
        $general->basic_attributes()->defense()->setBase($defense);
        $general->basic_attributes()->defense()
          ->setIncrement($defense_increment);
        $general->basic_attributes()->politics()->setBase($politics);
        $general->basic_attributes()->politics()
          ->setIncrement($politics_increment);

        my $bookName = $yamlData->{'general'}->{'book'};
        if (defined $bookName) {
          $self->logger()->trace("primary skill book for $name is $bookName");
          my $sb = Game::EvonyTKR::SkillBook::Special->new(name => $bookName);
          $sb->readFromFile();
          $general->addbuilt_in_book($sb);
        }
        else {
          $self->logger()
            ->logcroak("failed to find primary skill book for $name");
        }

        for (@otherBookNames) {
          my $tbName = $_;
          if ($tbName =~ /$bookName/i) {
            next;
          }
          my $tb = Game::EvonyTKR::SkillBook::Special->new(name => $tbName);
          $tb->readFromFile();
          $general->addAnotherBook($tb);
        }

        for (@SpecialityNames) {
          my $sn  = $_;
          my $tsi = Game::EvonyTKR::Speciality->new(name => $sn,);
          $tsi->readFromFile();
          $self->logger()->trace(sprintf('adding %s to %s', $sn, $name));
          $general->addSpeciality($tsi);
        }

        if ($ascending) {
          $general->ascendingAttributes()->readFromFile($name);
        }

        $general->validation();

        if ($self->add_general($general)) {
          $self->logger()
            ->debug(
            sprintf('created %s', Data::Printer::np $generals->{$name}));

          $self->logger()
            ->debug(sprintf('added %s', Data::Printer::np $generals->{$name}));
        }
      }
      if (scalar $generals->{$name}) {
        $self->logger()->debug("details populated for $name");
        return 1;
      }
    }
    return 0;
  }

}
1;
__END__
# ABSTRACT: store Game::EvonyTKR::General objects in memory for use by Game::EvonyTKR::Web::Controllers
