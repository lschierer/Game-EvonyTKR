use v5.40.0;
use experimental qw(class);
use FindBin;
use lib "$FindBin::Bin/../../../../../lib";

class Game::EvonyTKR::Web::Model::General :isa(Game::EvonyTKR::Web::Logger) {
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
  use Game::EvonyTKR::Data;
  use YAML::XS qw{ LoadFile Load };
  use namespace::autoclean;
# PODNAME: Game::EvonyTKR::Web::Model::General
# VERSION
  use FindBin;
  use lib "$FindBin::Bin/../../../../../lib";

  field $generals :reader;

  field $EvonyData = Game::EvonyTKR::Data->new();

  method add_general($ng) {
    my $gc = blessed($ng);
    my @gcl = split(/::/, $gc);
    if($gcl[2] !~ /general/i ) {
      $self->logger()->logwarn("provided object is of type '$gc' not 'Game::EvonyTKR::General '");
      return 0;
    }
    my $name = $ng->name();
    my $generalType = $ng->generalType();

    if ( not exists( $generals->{ $generalType } ) ) {
      $generals->{ $generalType } = {
        $name => $ng,
      };
      if(not exists $generals->{ $name } ) {
          $generals->{ $name } = [ $generalType ];
        }
        else {
          push @{ $generals->{ $name } }, $generalType;
        }
      return 1;
    }
    else {
      if ( not exists( $generals->{ $generalType }->{ $name } ) ) {
        $generals->{ $generalType }->{ $name } = $ng;

        if(not exists $generals->{ $name } ) {
          $generals->{ $name } = [ $generalType ];
        }
        else {
          push @{ $generals->{ $name } }, $generalType;
        }
        return 1;
      }
    }
    
    return 0;
  }

  method remove_general($rg) {
    my $gc = blessed($rg);
    my @gcl = split(/::/, $gc);
    if($gcl[2] !~ /general/i ) {
      $self->logger()->logwarn("provided object is of type '$gc' not 'Game::EvonyTKR::General '");
      return 0;
    }
    delete $generals->{$rg->name()};
    delete $generals->${$rg->generalType()}->{$rg->name()};
  }

  method get_by_id($name, $type = undef) {
    if(exists $generals->{$name}) {
      if(defined $type ) {
        if( any {$_ =~ $type } @{$generals->{$name}}) {
          return $generals->{$type}->{$name};
        }
      }
      else {
        for my $key ($EvonyData->GeneralKeys()) {
          if(exists $generals->{$key} and exists $generals->{$key}->{$name}) {
            return $generals->{$key}->{$name};
          }
        }
      }
    }
    else {
      my $generalShare =
      File::Spec->catfile(
        File::ShareDir::dist_dir('Game-EvonyTKR'), 'generals');
      my $FileWithPath = File::Spec->catfile($generalShare, "$name.yaml");

      if( -T -s -r $FileWithPath) {
        $self->logger()->trace("found $name.yaml in get_by_id");
        my $ng = Game::EvonyTKR::General->new(
          name  => $name,
        );
        $self->readFromFile($name);
        if($self->add_general($ng)) {
          return $ng;
        }
      }
    }
  }

  method get_all_by_type($type) {
    if(exists $generals->{$type}) {
      my $g = $generals->{$type};
      my $gc = blessed $g;
      my @gcl = split(/::/, $gc);
      if($gcl[2] =~ /general/i ) { 
        $self->logger->logwarn("$type refers to a single of general, not a type of general");
        return {};
      }
      else {
        return $g;
      }
    }
  }



  method readFromFile($name) {
        my $fileName = $name . '.yaml';
        my $generalShare =
          File::Spec->catfile( dist_dir('Game-EvonyTKR'), 'generals' );
        my $FileWithPath = File::Spec->catfile( $generalShare, $fileName );
        if ( -T -s -r $FileWithPath ) {
            $self->logger()->debug("$fileName exists as expected");
            my $yamlData = LoadFile($FileWithPath);
            my $leadership = $yamlData->{'general'}->{'leadership'};
            my $leadership_increment =
              $yamlData->{'general'}->{'leadership_increment'};
            my $attack             = $yamlData->{'general'}->{'attack'};
            my $attack_increment   = $yamlData->{'general'}->{'attack_increment'};
            my $defense            = $yamlData->{'general'}->{'defense'};
            my $defense_increment  = $yamlData->{'general'}->{'defense_increment'};
            my $politics           = $yamlData->{'general'}->{'politics'};
            my $politics_increment = $yamlData->{'general'}->{'politics_increment'};

            $self->logger()->trace(
                sprintf(
'for %s: leadership: %d, li: %d, attack: %d, ai: %d, defense: %d, di: %d, politics: %d, pi: %d',
                    $name,   $leadership,          $leadership_increment,
                    $attack, $attack_increment,    $defense,
                    $defense_increment, $politics, $politics_increment,
                )
            );

            my @SpecialityNames = @{ $yamlData->{'general'}->{'specialities'} };
            my @otherBookNames;
            if ( exists $yamlData->{'general'}->{'extra'} ) {
                push @otherBookNames, @{ $yamlData->{'general'}->{'extra'} };
            }

            $self->logger()
              ->trace(sprintf('ascending for %s is %s', $name, $yamlData->{'general'}->{'ascending'},));
            my $ascending = $yamlData->{'general'}->{'ascending'};

            my @generalClassKey;
            my @scoreType = @{ $yamlData->{'general'}->{'score_as'} };
            if(any {$_ =~ /Ground/ } @scoreType) {
              push @generalClassKey, 'Ground';
            }
            if(any {$_ =~ /Mounted/ }@scoreType) {
              push @generalClassKey, 'Mounted';
            }
            if(any {$_ =~ /Ranged/ } @scoreType) {
              push @generalClassKey, 'Mounted';
            }
            if(any {$_ =~ /Siege/ } @scoreType) {
              push @generalClassKey, 'Mounted';
            }
            if(any {$_ =~ /Mayor/ } @scoreType) {
              push @generalClassKey, 'Mounted';
            }

            if (scalar @generalClassKey != scalar @scoreType) {
              $self->logger()->error(sprintf('%s is of unknown general type %s.',
              $yamlData->{'general'}->{'name'}, Data::Printer::np @scoreType));
            }
            my %constructors = $EvonyData->generalClass();
            for (@generalClassKey) {
              my $general = $constructors{$_}->new(
                name => $yamlData->{'general'}->{'name'},
              );
              
              
              my $bookName = $yamlData->{'general'}->{'book'};
              if ( defined $bookName ) {
                  $self->logger()
                    ->trace("primary skill book for $name is $bookName");
                  my $sb =
                    Game::EvonyTKR::SkillBook::Special->new( name => $bookName );
                  $sb->readFromFile();
                  $general->addBuiltInBook($sb);
              }
              else {
                  $self->logger()
                    ->logcroak("failed to find primary skill book for $name");
              }

              for (@otherBookNames) {
                my $tbName = $_;
                if ( $tbName =~ /$bookName/i ) {
                    next;
                }
                my $tb =
                  Game::EvonyTKR::SkillBook::Special->new( name => $tbName );
                $tb->readFromFile();
                $general->addAnotherBook($tb);
              }

              for (@SpecialityNames) {
                my $sn  = $_;
                my $tsi = Game::EvonyTKR::Speciality->new( name => $sn, );
                $tsi->readFromFile();
                $general->addSpeciality($tsi);
              }

              if ($ascending) {
                  $general->ascendingAttributes()->readFromFile($name);
              }
              
              $general->validation();

              $self->add_general($general);
              $self->logger()->debug(sprintf('created %s', $generals->{$name}));

              $self->logger()->debug(sprintf('added %s', 
                Data::Printer::np $generals->{$name} ));
            }

            
            $self->logger()
              ->debug( "details populated for " . Data::Printer::np $name );

        }

    }

}
1;
__END__
# ABSTRACT: store Game::EvonyTKR::General objects in memory for use by Game::EvonyTKR::Web::Controllers