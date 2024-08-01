use v5.40.0;
use experimental 'class';
use utf8::all;

class Game::EvonyTKR::Buff::Data {

use Types::Standard qw(is_Int Int Str is_Str);
use Types::Common::Numeric qw(PositiveOrZeroInt);
use Type::Utils "is"; 
use File::ShareDir ':ALL';
use YAML::XS;
use namespace::autoclean;

# PODNAME: Game::EvonyTKR::Buff::Data
# ABSTRACT: Data files for Game::EvonyTKR::Buff

=head1 DESCRIPTION

Due to the encapsulation and initialization order requirements, even if the perlclass feature had already implemented the :common attribute, things marked as common would not be initialized in time for other parameters to validate against them.  Thus I need a ::Data class that users can initialize first. 

=cut

=attr BuffAttributes
Array Attribute. 

A buff will have exactly one attribute from this list of possible Attributes.  This list is effeectively attempting to replace having an enum. 
=cut

  field @BuffAttributes :reader;

=method set_BuffAttributes()

get the possible attributes from the yaml data file in the shared directory and load them into memory. 
=cut
  method set_BuffAttributes {
    my $data_location = dist_file('Game-EvonyTKR', 'buff/attributes.yaml');
    open(my $DATA, '<', $data_location) or die $!;
    my $yaml = do { local $/; <$DATA> };
    my $data = Load $yaml;
    close $DATA;
    @BuffAttributes = @{ $data->{'attributes'} };
  }

=attr BuffConditions
Array attribute. 

A buff will have one or more conditions from this list of Conditioons.  This list is effectively attempting to replace having an emum.
=cut
  field @BuffConditions :reader;

=method set_BuffConditions()

read the possible conditions from the yaml file in the shared directory into memory
=cut
  method set_BuffConditions {
    my $data_location = dist_file('Game-EvonyTKR', 'buff/conditions.yaml');
    open(my $DATA, '<', $data_location) or die $!;
    my $yaml = do { local $/; <$DATA> };
    my $data = Load $yaml;
    close $DATA;
    @BuffConditions = @{ $data->{'conditions'} };
  }

=attr BuffClasses
Array attribute. 

A buff will affect either exactly one or All classes of troops. This is attempting to replace having an enum.
=cut
  field @BuffClasses :reader;

=method set_BuffClasses()

read the possible troop classes from the yaml file in the shared directory into memory.
=cut
  method set_BuffClasses {
    my $data_location = dist_file('Game-EvonyTKR', 'buff/classes.yaml');
    open(my $DATA, '<', $data_location) or die $!;
    my $yaml = do { local $/; <$DATA> };
    my $data = Load $yaml;
    close $DATA;

    @BuffClasses = @{ $data->{'classes'} };
  }

}

1;

__END__

=method new()

instantiate the shared data helper functions.
=cut