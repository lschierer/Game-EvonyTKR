use v5.40.0;
use experimental qw(class);
use FindBin;
use lib "$FindBin::Bin/../../../../../lib";

class Game::EvonyTKR::Web::Model::General :isa(Game::EvonyTKR::Web::Logger) {
# PODNAME: Game::EvonyTKR::Web::Model::General
# ABSTRACT: store Game::EvonyTKR::General objects in memory for use by Game::EvonyTKR::Web::Controllers
  use Carp;
  use Data::Printer;
  use Devel::Peek;
  use File::ShareDir ':ALL';
  use File::Spec;
  use Util::Any ':all';
  use Game::EvonyTKR::General;
  use Game::EvonyTKR::Data;
  use namespace::autoclean;
# VERSION
  use FindBin;
  use lib "$FindBin::Bin/../../../../../lib";

  field $generals :reader;

  method add_general($ng) {
    my $gc = blessed($ng);
    my @gcl = split(/::/, $gc);
    if($gcl[2] !~ /general/i ) {
      $self->logger()->logwarn("provided object is of type '$gc' not 'Game::EvonyTKR::General '");
      return 0;
    }

    if ( not exists( $generals->{ $ng->generalType() } ) ) {
      $generals->{ $ng->generalType() } = [ $ng->generalType() ];
    }
    
    if ( not exists( $generals->{ $ng->generalType() }->{ $ng->name() } ) ) {
      $generals->{ $ng->generalType() }->{ $ng->name() } = $ng;

      if(not exists $generals->{ $ng->name() } ) {
        $generals->{ $ng->name() } = [ $ng->generalType() ];
      }
      else {
        push @{ $generals->{ $ng->name() } }, $ng->generalType();
      }
      return 1;
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
      if(defined $type and any {$_ =~ $type } @{$generals->{$name}}) {
        if(exists $generals->{$type}->{$name}){
          return $generals->{$type}->{$name};
        }
      }
      else {

      }
        
      my @gcl = split(/::/, $gc);
      if($gcl[2] !~ /general/i ) { 
        $self->logger()->logwarn("$name refers to a type of general, not a single general");
        $self->logger()->trace("$name has class $gc");
        $self->logger()->trace(sprintf("g looks like %s", Data::Printer::np $g));
      }
      else {
        $self->logger()->trace("returning cached general for $name");
        return $g;
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
        $ng->readFromFile();
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
            my $data = LoadFile($FileWithPath);
            my $leadership = $data->{'general'}->{'leadership'};
            my $leadership_increment =
              $data->{'general'}->{'leadership_increment'};
            my $attack             = $data->{'general'}->{'attack'};
            my $attack_increment   = $data->{'general'}->{'attack_increment'};
            my $defense            = $data->{'general'}->{'defense'};
            my $defense_increment  = $data->{'general'}->{'defense_increment'};
            my $politics           = $data->{'general'}->{'politics'};
            my $politics_increment = $data->{'general'}->{'politics_increment'};

            $self->logger()->trace(
                sprintf(
'for %s: leadership: %d, li: %d, attack: %d, ai: %d, defense: %d, di: %d, politics: %d, pi: %d',
                    $name,   $leadership,          $leadership_increment,
                    $attack, $attack_increment,    $defense,
                    $defense_increment, $politics, $politics_increment,
                )
            );

            my @SpecialityNames = @{ $data->{'general'}->{'specialities'} };
            my @otherBookNames;
            if ( exists $data->{'general'}->{'extra'} ) {
                push @otherBookNames, @{ $data->{'general'}->{'extra'} };
            }

            my $bookName = $data->{'general'}->{'book'};
            if ( defined $bookName ) {
                $self->logger()
                  ->trace("primary skill book for $name is $bookName");
                my $sb =
                  Game::EvonyTKR::SkillBook::Special->new( name => $bookName );
                $sb->readFromFile();
                $self->addBuildInBook($sb);
            }
            else {
                $self->logger()
                  ->logcroak("failed to find primary skill book for $name");
            }

            $self->logger()
              ->trace(sprintf('ascending for %s is %s', $name, $data->{'general'}->{'ascending'},));
            my $ascending = $data->{'general'}->{'ascending'};

            my @generalClassKey;
            my @scoreType = @{ $data->{'general'}->{'score_as'} };
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
              $data->{'general'}->{'name'}, Data::Printer::np @scoreType));
            }

            for (@generalClassKey) {
              $self->add_general($generalClass{$_}->new(
                name => $data->{'general'}->{'name'},
              ));
              
              $self->logger()->debug(sprintf('created %s', $generals->{$name}));

              $$self->logger()->debug(sprintf('added %s', 
                Data::Printer::np $generals->{$name} ));
            }

            for (@otherBookNames) {
                my $tbName = $_;
                if ( $tbName =~ /$bookName/i ) {
                    next;
                }
                my $tb =
                  Game::EvonyTKR::SkillBook::Special->new( name => $tbName );
                $tb->readFromFile();
                $generals->{$name}->addAnotherBook($tb);
            }

            for (@SpecialityNames) {
                my $sn  = $_;
                my $tsi = Game::EvonyTKR::Speciality->new( name => $sn, );
                $tsi->readFromFile();
                $generals->{$name}->addSpeciality($tsi);
            }

            if ($ascending) {
                $generals->{$name}->ascendingAttributes()->readFromFile($name);
            }

            $generals->{$name}->_validation();
            $self->logger()
              ->debug( "details populated for " . Data::Printer::np $name );

        }

    }

}
1;
