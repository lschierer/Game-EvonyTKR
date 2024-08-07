package Game::EvonyTKR;
use v5.40.0;
use utf8::all;

use Carp;
use experimental qw(class);
use Data::Printer;
use base qw(App::Cmd::Simple);
use File::ShareDir ':ALL';
use File::Spec;
use File::HomeDir;
use File::Path qw(make_path);
use File::Touch;
use YAML::XS qw{LoadFile Load};
use Util::Any -all;
use Devel::Peek;
use Game::EvonyTKR::Logger;
use Game::EvonyTKR::General::Pair::Creator;
use Game::EvonyTKR::General::Pair;
use Game::EvonyTKR::General::Ground;
use Game::EvonyTKR::General::Mounted;
use Game::EvonyTKR::General::Ranged;
use Game::EvonyTKR::General::Siege;
use Game::EvonyTKR::Ascending;
use Game::EvonyTKR::SkillBook::Special;
use Game::EvonyTKR::Specality;
use Game::EvonyTKR::Buff;
use Game::EvonyTKR::Buff::Value;
use Game::EvonyTKR::Buff::Data;
use namespace::autoclean;

sub opt_spec {
  return (
    [ "option1|a",  "do option 1" ],
  );
}



sub validate_args {
  my ($self, $opt, $args) = @_;

  # no args allowed but options
  $self->usage_error("No args allowed") if @$args;
}

sub execute {
  my ($self, $opt, $args) = @_;
  binmode(STDOUT, ":encoding(UTF-8)"); # apparently not the same thing as "use utf8;"
  binmode(STDIN, ":encoding(UTF-8)"); # apparently not the same thing as "use utf8;"
  if ($opt->{option1}) {
      # do option 1 stuff
  } else {

    my $logController = Game::EvonyTKR::Logger->new();
    my $logger = $logController->logger();
    my %generals = read_generals($logger);
    my $classData = Game::EvonyTKR::Buff::Data->new();
    my $conflicData = Game::EvonyTKR::General::Conflicts->new();
    $conflicData->initializeConflictDB();
    $classData->set_BuffClasses();
    my @BuffClasses = $classData->BuffClasses();
    if(scalar @BuffClasses == 0) {
      croak 'error loading BuffClasses';
    }
    $logger->info("start");
    $logger->info(sub {np %generals });
    my $pairCreator = Game::EvonyTKR::General::Pair::Creator->new();
    $pairCreator->set_generals(%generals);
    my %pairs = $pairCreator->getPairs();
    
    for my $bc (@BuffClasses) {
      if(exists $pairs{$bc}) {
        my @pairGroup = @{$pairs{$bc}};
        $logger->info("$bc: " . scalar @pairGroup);
        for my $gp (@pairGroup) {
          $logger->trace("primary: " . $gp->primary()->name());
          $logger->trace("secondary: " . $gp->secondary()->name())
        }
      } else {
        $logger->info("$bc: no pairs");
      }
    }
    $logger->info("done");
  }
}

sub read_generals($logger) {
  
  $logger->info("read_generals");
  my $general_share = File::Spec->catfile(dist_dir('Game-EvonyTKR'), 'generals');
  
  my $special_share = File::Spec->catfile(dist_dir('Game-EvonyTKR'), 'specialities');
  my $ascending_share = File::Spec->catfile(dist_dir('Game-EvonyTKR'), 'ascending');
  my @found = grep { -T -s -r } glob("$general_share/*.yaml");
  $logger->info("$general_share");
  $logger->info(scalar @found);
    
  my %generals;
  foreach my $tg (@found) {
    if(defined($tg)) {
      open(my ($fh), '<', $tg) or croak "$!";
      my $data = LoadFile($tg);
      my $name = $data->{'general'}->{'name'};
      $logger->info($name);
      my @books = @{ $data->{'general'}->{'books'} };
      my @SpecialityNames = @{ $data->{'general'}->{'specialities'}};
      
      if(exists $data->{'general'}->{'extra'} ) {
        push @books, @{ $data->{'general'}->{'extra'} };
      }

      my $bookName = $books[0];
      my $sb = Game::EvonyTKR::SkillBook::Special->new(
        name  => $bookName
      );
      $sb->readFromFile();

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
        );
        for (@books) {
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
          my $tsi = Game::EvonyTKR::Specality->new(
            name => $sn,
          );
          $tsi->readFromFile();
          $generals{$name}->
        }
        $logger->debug("added ". np $generals{$name});
      }
    
    }
  }
  return %generals;
}

1;
__END__

# PODNAME: Game::EvonyTKR

# ABSTRACT: Perl Modules providing utilities for players of Evony The King's Return

=head1 SYNOPSIS

  use Game::EvonyTKR;

  Game::EvonyTKR->execute();
=cut

=head1 DESCRIPTION

This distribution aims to help players create and process the wealth of data that _Evony The King's Return_ dumps on users with next to no organization, documentation, or built-in tools to handle.

This module will (eventually) help players of the game needing to make reasonably complex analysis to make optimal choices about each of these:

=for :list

* Which generals to invest time and resources in

* How to best pair generals for different senarios

* The effects of Armor, Spiritual Beats, and Dragons on buffs and debuffs

* Calculating your total buffs and debuffs in different senarios given the ever increasing number of sources for these buffs and debuffs



=method getBookData($sb, $bookName)
$sb is a Game::EvonyTKR::SkillBook
$bookname is the YAML file containing its data. 
This populates the skill book with the data. 
=cut