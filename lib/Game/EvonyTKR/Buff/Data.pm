use 5.40.0;
use experimental 'class';

class Game::EvonyTKR::Buff::Data {

use Types::Standard qw(is_Int Int Str is_Str);
use Types::Common::Numeric qw(PositiveOrZeroInt);
use Type::Utils "is"; 
use YAML::XS;
use namespace::autoclean;

# PODNAME: Game::EvonyTKR::Buff::Data
# ABSTRACT: Data files for Game::EvonyTKR::Buff

=head1 SYNOPSIS

=for comment Brief examples of using the module.

=head1 DESCRIPTION

=for due to the encapsulation and initialization order requirements, even if the perlclass feature had already implemented the :common attribute, things marked as common would not be initialized in time for other parameters to validate against them.  Thus I need a ::Data class that users can initialize first. 

=cut

  field @BuffAttributes;

  method BuffAttributes {
    return @BuffAttributes;
  }

  method set_BuffAttributes {
    my $data_location = dist_file('Game-EvonyTKR', 'buff/attributes.yaml');
    open(my $DATA, '<', $data_location) or die $!;
    my $yaml = do { local $/; <$DATA> };
    my $data = Load $yaml;
    close $DATA;
    @BuffAttributes = @{ $data->{'attributes'} };
  }

  field @BuffConditions;

  method BuffConditions {
    return @BuffConditions;
  }

  method set_BuffConditions {
    my $data_location = dist_file('Game-EvonyTKR', 'buff/conditions.yaml');
    open(my $DATA, '<', $data_location) or die $!;
    my $yaml = do { local $/; <$DATA> };
    my $data = Load $yaml;
    close $DATA;
    @BuffConditions = @{ $data->{'conditions'} };
  }

  field @BuffClasses;

  method BuffClasses {
    return @BuffClasses;
  }

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