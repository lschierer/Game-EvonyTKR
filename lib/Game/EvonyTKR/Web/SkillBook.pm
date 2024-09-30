use v5.40.0;
use utf8::all;
use experimental qw{class defer};
use FindBin;
use lib "$FindBin::Bin/../../../../lib";

package Game::EvonyTKR::Web::SkillBook {
# ABSTRACT: Route Handler for the /skillbook route.
  use Carp;
  use Data::Printer;
  use Devel::Peek;
  use File::HomeDir;
  use File::Path     qw{ make_path };
  use File::ShareDir qw{ dist_dir dist_file };
  use File::Spec;
  use File::Touch;
  use Game::EvonyTKR::Ascending;
  use Game::EvonyTKR::General::Ground;
  use Game::EvonyTKR::General::Mounted;
  use Game::EvonyTKR::General::Ranged;
  use Game::EvonyTKR::General::Siege;
  use Game::EvonyTKR::SkillBook::Special;
  use Game::EvonyTKR::Speciality;
  use Game::EvonyTKR::Web::Store;
  use Log::Log4perl;
  use Util::Any ':all';
  use YAML::XS qw{ LoadFile Load };
  use namespace::clean;
  use Dancer2 appname => 'Game::EvonyTKR';
  use Dancer2::Plugin::REST;
  use FindBin;
  use lib "$FindBin::Bin/../../../../lib";

  my $store;
  my $generals;
  my $logger = Log::Log4perl::get_logger('Web::General');

  prefix '/skillbook' => sub {
    prefix '/list' => sub {
      get ''         => \&_list;
      get '/details' => \&_details;
    };
    get '/:id' => \&_singleSkillBook;
  };

  sub _init {
    $store = Game::EvonyTKR::Web::Store::get_store();
    if (not exists $$store{'skillbooks'}) {
      $store->{'skillbooks'} = {
        'standard' => {},
        'special'  => {},
      };
      _read_skillBooks();
      $logger->trace('store now holds' . np $store);
    }
  }

  sub _list {
    _init();
    my $bookCount += scalar keys %{ $store->{'skillbooks'}->{'special'} };
    $bookCount += scalar keys %{ $store->{'skillbooks'}->{'standard'} };
    if ($bookCount <= 0) {
      return status_400('no SkillBooks Available');
    }
    my @returnList = ();
    push @returnList, keys %{ $store->{'skillbooks'}->{'special'} };
    for my $bookName (keys %{ $store->{'skillbooks'}->{'standard'} }) {
      for my $bookLevel (
        keys %{ $store->{'skillbooks'}->{'standard'}->{$bookName} }) {
        if ($bookLevel =~ /level-(\d)+/i) {
          push @returnList, "$bookName Level $1";
        }
      }
    }
    return status_ok(\@returnList);
  }

  sub _details {
    _init();
    my $bookCount += scalar keys %{ $store->{'skillbooks'}->{'special'} };
    $bookCount += scalar keys %{ $store->{'skillbooks'}->{'standard'} };
    if ($bookCount <= 0) {
      return status_400('no SkillBooks Available');
    }

    my $verbose = query_parameters->get('verbose');
    if (defined $verbose) {
      if ($verbose ne 'false') {
        $verbose = 1;
      }
      else {
        $verbose = 0;
      }
    }
    else {
      $verbose = 0;
    }

    my @returnList = ();

    for my $book (keys %{ $store->{'skillbooks'}->{'special'} }) {
      my $r = $store->{'skillbooks'}->{'special'}->{$book}->toHashRef($verbose);
      $r->{'id'} = $r->{'name'};
      push @returnList, $r;
    }

    for my $bookName (keys %{ $store->{'skillbooks'}->{'standard'} }) {
      for my $bookLevel (
        keys %{ $store->{'skillbooks'}->{'standard'}->{$bookName} }) {
        my $r = $store->{'skillbooks'}->{'standard'}->{$bookName}->{$bookLevel}
          ->toHashRef($verbose);
        $r->{'id'} =
          sprintf('%s %s', $bookName, $bookLevel =~ s/level-/Level /r);
        push @returnList, $r;
      }
    }
    return status_ok(\@returnList);
  }

  sub _singleSkillBook {
    _init();

    my $verbose = query_parameters->get('verbose');
    if (defined $verbose) {
      if ($verbose ne 'false') {
        $verbose = 1;
      }
      else {
        $verbose = 0;
      }
    }
    else {
      $verbose = 0;
    }

    my $id = route_parameters->get('id');
    if (exists $store->{'skillbooks'}->{'special'}->{$id}) {
      return status_ok(
        $store->{'skillbooks'}->{'special'}->{$id}->toHashRef($verbose));
    }
    else {
      return status_400("SkillBook with name '$id' is not available.");
    }
  }

  sub _read_skillBooks() {
    $logger->info('starting _read_skillBooks');

    my $share = File::Spec->catfile(File::ShareDir::dist_dir('Game-EvonyTKR'),
      'skillBooks');
    my @found = grep { -T -s -r } glob("$share/*.yaml");
    $logger->info(sprintf('found %d skillbooks in %s.', scalar @found, $share));

    for my $tsbf (@found) {
#I am essentially just testing that I *can* open the file in a more loggable way.
      open(my ($fh), '<', $tsbf) or $logger->logcroak("$!");
      close $fh;

      #then I get the data for real with the Yaml library
      my $data = LoadFile($tsbf);

      if (exists $data->{'books'}) {
        my @books = @{ $data->{'books'} };
        for my $tbe (@books) {
          $logger->info("$tsbf contains standard SkillBooks");
          if (exists $tbe->{'name'}) {
            my $name = $tbe->{'name'};
            if (exists $tbe->{'level'}) {
              my $level = $tbe->{'level'};
              if (exists $tbe->{'buff'}) {
                my @tbeBuffs = @{ $tbe->{'buff'} };
                if (
                  not(  defined($name)
                    and defined($level)
                    and (scalar @tbeBuffs > 0))
                ) {
                  $logger->logcroak(
                    "something went wrong setting values for book in $tsbf");
                }
                my $tsb = Game::EvonyTKR::SkillBook::Standard->new(
                  name  => $name,
                  level => $level,
                );

                $logger->debug(sprintf(
                  '%s level %d has %d buffs',
                  $name, $level, scalar @tbeBuffs,
                ));

                for my $tbeb (@tbeBuffs) {
                  my $v;
                  my $b;
                  my @tbebKeys = keys %{$tbeb};

                  if (any { $_ eq 'value' } @tbebKeys) {
                    $logger->debug(sprintf(
                      'Skillbook %s level %d has a buff with a value',
                      $name, $level,
                    ));
                    $v = Game::EvonyTKR::Buff::Value->new(
                      number => $tbeb->{'value'}->{'number'},
                      unit   => $tbeb->{'value'}->{'unit'},
                    );
                    if (any { $_ eq 'class' } @tbebKeys) {
                      $b = Game::EvonyTKR::Buff->new(
                        attribute => $tbeb->{'attribute'},
                        value     => $v,
                        buffClass => $tbeb->{'class'},
                      );
                    }
                    else {
                      $b = Game::EvonyTKR::Buff->new(
                        attribute => $tbeb->{'attribute'},
                        value     => $v,
                      );
                    }
                  }
                  else {
                    $logger->warn(sprintf(
                      'Skillbook %s level %d has a buff with no value.',
                      $name, $level,
                    ));
                  }
                  if (defined($b)) {
                    if (any { $_ eq 'condition' } @tbebKeys) {
                      my @conditions = @{ $tbeb->{'condition'} };
                      for my $tbebc (@conditions) {
                        $b->set_condition($tbebc);
                      }
                    }
                    $tsb->add_buff($b);
                  }
                }
                $logger->info(sprintf(
                  'adding standard skillbook %s level %d.',
                  $name, $level,
                ));
                $store->{'skillbooks'}->{'standard'}->{$name}->{"level-$level"}
                  = $tsb;
              }
              else {
                $logger->warn("No buff defined for a book in $tsbf");
              }
            }
            else {
              $logger->logcroak("failed to find level for book in $tsbf");
            }
          }
          else {
            $logger->logcroak("the $tsbf file has an unexpected structure");
          }
        }
      }
      elsif (exists $data->{'name'}) {
        my $name = $data->{'name'};
        if (defined $name) {
          $logger->info($name);
          my $tsb = Game::EvonyTKR::SkillBook::Special->new(name => $name,);
          $tsb->readFromFile();
          $store->{'skillbooks'}->{'special'}->{$name} = $tsb;
        }
        else {
          $logger->logcroak("failed to set name from name field for $tsbf");
        }
      }
      else {
        $logger->logcroak("the name field does not exist in the $tsbf file");
      }
    }
  }
}
1;

__END__
