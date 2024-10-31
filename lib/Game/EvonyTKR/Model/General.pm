use v5.40.0;
use feature 'try';
use experimental qw(class);
use utf8::all;
require Data::Printer;
use File::FindLib 'lib';
use File::ShareDir ':ALL';
use Types::Common qw(t);
use Types::Common::Numeric qw(PositiveOrZeroInt);
use Types::Standard        qw(is_Int Int Str is_Str);
use Type::Utils            qw(is enum);
use YAML::PP;
use X500::RDN;
use X500::DN;
require Game::EvonyTKR::Data;
use namespace::clean;

class Game::EvonyTKR::Model::General :isa(Game::EvonyTKR::Data) {
# PODNAME: Game::EvonyTKR::Model::General
  use List::AllUtils qw( any none );
  use UUID qw(uuid5);
  use namespace::autoclean;
  use Carp;
  use File::FindLib 'lib';
  our $VERSION = 'v0.30.0';
  my $debug = 1;

  field $name :reader :param;

  field $type :reader :param;

  field $data = Game::EvonyTKR::Data->new();

  ADJUST {
    my @errors;
    if(none { $_ =~ /$type/i } $data->GeneralKeys() ) {
      my @gk = $data->GeneralKeys();
      push @errors, sprintf('type must be one of %s, not %s', Data::Printer::np @gk, $type);
    }
    if(@errors) {
      $data->logger()->logcroak(join ', ' => @errors);
    }
  }


}
1;

__END__
#ABSTRACT: how to store a General in memory

=pod

=head1 DESCRIPTION

I am not doing true MVC because I am using yaml files as the persistence layer instead of a database.  This class stores a General in memory so that the Controller need not read
in and parse the YAML every time.

=cut

=method new($name)

Create an instance of a Model::General with name $name.

=method name()

returns the general's name.

=cut

=method type()

returns the general's type, which must be one of the values from Game::EvonyTKR::Data->GeneralKeys()

=cut
