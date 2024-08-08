use v5.40.0;
use utf8::all;
use experimental qw(class);

class Game::EvonyTKR::REST::Generals :isa(Game::EvonyTKR::Logger) {
# PODNAME: Game::EvonyTKR::REST::Generals
# ABSTRACT: Route Handler for the /generals route. 

  use Carp;
  use Data::Printer;
  use File::Spec;
  use File::ShareDir qw{ dist_dir dist_file };
  use File::HomeDir;
  use File::Path qw(make_path);
  use File::Touch;
  use YAML::XS qw{LoadFile Load};
  use Util::Any -all;
  use Devel::Peek;
  use Log::Log4perl;
  use Game::EvonyTKR::General::Ground;
  use Game::EvonyTKR::General::Mounted;
  use Game::EvonyTKR::General::Ranged;
  use Game::EvonyTKR::General::Siege;
  use Game::EvonyTKR::SkillBook::Special;
  use Game::EvonyTKR::Speciality;
  use Game::EvonyTKR::Ascending;
  use namespace::autoclean;

  field %generals;

  method read_generals() {
    $self->logger()->info('starting read_generals');
    my $general_share = File::Spec->catfile(File::ShareDir::dist_dir('Game-EvonyTKR'), 'generals');

    my @found = grep { -T -s -r } glob("$general_share/*.yaml");
    my $message = "general_share: " . scalar @found;
    $self->logger()->info("general_share '$general_share' contained " . scalar @found . " generals");
    
    foreach my $tg (@found) {
      open(my ($fh), '<', $tg) or croak "$!";
      my $data = LoadFile($tg);
      my $name = $data->{'general'}->{'name'};
      $self->logger()->info($name);
      my @bookNames = @{ $data->{'general'}->{'books'} };
      my @SpecialityNames = @{ $data->{'general'}->{'specialities'}};

      if(exists $data->{'general'}->{'extra'} ) {
        push @bookNames, @{ $data->{'general'}->{'extra'} };
      }

      my $bookName = $bookNames[0];
      $self->logger()->trace("primary skill book for $name is $bookName");
      my $sb = Game::EvonyTKR::SkillBook::Special->new(
        name  => $bookName
      );
      $sb->readFromFile();

      my $ascendingToggle;
      if( $data->{'general'}->{'ascending'} =~ /true/i){
        $ascendingToggle = 1;
      } else {
        $ascendingToggle = 0;
      }

      my %generalClass = (
        'Ground'  => 'Game::EvonyTKR::General::Ground',
        'Mounted' => 'Game::EvonyTKR::General::Mounted',
        'Ranged'  => 'Game::EvonyTKR::General::Ranged',
        'Siege'   => 'Game::EvonyTKR::General::Siege',
      );

      my @generalClassKey;
      my @scoreType = @{ $data->{'general'}->{'score_as'} };
      if (any {$_ =~ /Ground/ } @scoreType) {
        push @generalClassKey => 'Ground';
      } 
      if (any {$_ =~ /Mounted/ } @scoreType) {
        push @generalClassKey => 'Mounted';
      } 
      if (any {$_ =~ /Ranged/ or $_ =~ /Archers/ } @scoreType) {
        push @generalClassKey => 'Ranged';
      } 
      if (any {$_ =~ /Siege/ } @scoreType) {
        push @generalClassKey => 'Siege';
      } 
      if (any {$_ =~ /Mayor/ } @scoreType) {
        next;
      }
      if (scalar @generalClassKey != scalar @scoreType) {
        croak $data->{'general'}->{'name'} . " is of unknown general type " . p @scoreType;
      }

      for (@generalClassKey){
        $generals{$name} = $generalClass{$_}->new(
          name                  => $data->{'general'}->{'name'},
          leadership            => $data->{'general'}->{'leadership'},
          leadership_increment  => $data->{'general'}->{'leadership_increment'},
          attack                => $data->{'general'}->{'attack'},
          attack_increment      => $data->{'general'}->{'attack_increment'},
          defense               => $data->{'general'}->{'defense'},
          defense_increment     => $data->{'general'}->{'defense_increment'},
          politics              => $data->{'general'}->{'politics'},
          politics_increment    => $data->{'general'}->{'politics_increment'},
          builtInBook           => $sb,
          ascending             => $ascendingToggle,
        );

        for (@bookNames) {
          my $tbName = $_;
          if($tbName eq /$bookName/ ) {
            next;
          }
          my $tb = Game::EvonyTKR::SkillBook::Special->new(
            name  => $tbName
          );
          $tb->readFromFile();
          $generals{$name}->addAnotherBook($tb);
        }

        for (@SpecialityNames) {
          my $sn = $_;
          my $tsi = Game::EvonyTKR::Speciality->new(
            name => $sn,
          );
          $tsi->readFromFile();
        }
        
        if($ascendingToggle) {
          $generals{$name}->ascendingAttributes()->readFromFile($name);
        }

        $self->logger()->debug("added ". np $generals{$name});
      }


    }
  }

};

1;

__END__

