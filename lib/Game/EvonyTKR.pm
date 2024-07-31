package Game::EvonyTKR;
use v5.40.0;
use utf8::all;

use Carp;
use experimental qw(class);

use base qw(App::Cmd::Simple);
use File::ShareDir ':ALL';
use File::Spec;
use YAML::XS qw{LoadFile Load};
use Devel::Peek;
use Game::EvonyTKR::General::Pair::Creator;
use Game::EvonyTKR::General::Ground;
use Game::EvonyTKR::General::Mounted;
use Game::EvonyTKR::General::Ranged;
use Game::EvonyTKR::General::Siege;
use Game::EvonyTKR::SkillBook::Special;
use Game::EvonyTKR::Buff;
use Game::EvonyTKR::Buff::Value;
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
    my %generals = read_generals();
    say "start";
    say scalar %generals;
    my $pairCreator = Game::EvonyTKR::General::Pair::Creator->new();
    $pairCreator->set_generals(%generals);
    $pairCreator->getConflictData();
    say "done";
  }
}

sub read_generals {
  my ($self, $opt, $args) = @_;
  my $debug = 1;
  if($debug) { say "read_generals";}
  my $general_share = File::Spec->catfile(dist_dir('Game-EvonyTKR'), 'generals');
  my $book_share = File::Spec->catfile(dist_dir('Game-EvonyTKR'), 'skillBooks');
  my @found = grep { -T -s -r } glob("$general_share/*.yaml");
  if($debug) {
    say "$general_share";
    say scalar @found;
  }
  my %generals;
  foreach my $tg (@found) {
    if(defined($tg)) {
      open(my ($fh), '<', $tg) or croak "$!";
      my $data = LoadFile($tg);
      my $name = $data->{'general'}->{'name'};
      if($debug) {say $name;}
      my $bookName = $data->{'general'}->{'books'}[0].".yaml";
      my $sb = Game::EvonyTKR::SkillBook::Special->new(
        name  => $bookName
      );
      my $data_filename = File::Spec->catfile($book_share, $bookName);
      if( -T -s -r $data_filename ) {
        my $bookData = LoadFile($data_filename);
        my @buffs = @{ $bookData->{'buff'} };
        foreach my $rb (@buffs) {
          my $v = Game::EvonyTKR::Buff::Value->new(
            number      => $rb->{'number'},
            unit        => $rb->{'unit'},
          );
          my $b = Game::EvonyTKR::Buff->new(
            attribute  => $rb->{'attribute'},
            value      => $v,
            buffClass   => $rb->{'class'},
          );
          foreach my $rc ($rb->{'condition'}) {
            $b->set_condition($rc);
          }
          $sb->add_buff($b);
        }
      } else {
        croak "$data_filename is not found or cannot be read."
      }

      my %generalClass = (
        'Ground'  => 'Game::EvonyTKR::General::Ground',
        'Mounted' => 'Game::EvonyTKR::General::Mounted',
        'Ranged'  => 'Game::EvonyTKR::General::Ranged',
        'Siege'   => 'Game::EvonyTKR::General::Siege',
      );
      
      my $generalClassKey;

      my $scoreType = $data->{'general'}->{'score_as'};
      
      if ($scoreType =~ /Ground/) {
        $generalClassKey = 'Ground';
      } elsif ($scoreType =~ /Mounted/) {
        $generalClassKey = 'Mounted';
      } elsif ($scoreType =~ /Ranged/ || $scoreType =~ /Archers/) {
        $generalClassKey = 'Ranged';
      } elsif ($scoreType =~ /Siege/) {
        $generalClassKey = 'Siege';
      } elsif ($scoreType =~ /Mayor/) {
        next;
      } 
      else {
        croak $data->{'general'}->{'name'} . " is of unknown general type $_";
      }
    
      $generals{$name} = $generalClass{$generalClassKey}->new(
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



