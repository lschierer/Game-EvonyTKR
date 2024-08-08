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
  use namespace::autoclean;


  method read_generals() {
    $self->logger()->info('starting read_generals');
    my $general_share = File::Spec->catfile(File::ShareDir::dist_dir('Game-EvonyTKR'), 'generals');

    my @found = grep { -T -s -r } glob("$general_share/*.yaml");
    my $message = "general_share: " . scalar @found;
    $self->logger()->info("general_share '$general_share' contained " . scalar @found . " generals");
    
  }

};

1;

__END__

