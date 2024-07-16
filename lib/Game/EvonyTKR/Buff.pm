package Game::EvonyTKR::Buff;
use 5.38.0;
use Moose;
use File::ShareDir ':ALL';
use YAML::XS;
use Moose::Util::TypeConstraints;
use Data::Dumper qw(Dumper);
use Game::EvonyTKR::Buff::Value;
use namespace::autoclean;

# PODNAME: Game::EvonyTKR::Buff

# ABSTRACT: Buff and Debuff primatives 

=head1 SYNOPSIS

=for comment Brief examples of using the module.

=head1 DESCRIPTION

=for A Buff is an attribute that modifies various attributes in Evony TKR

Buffs can be positive or negative.  When negative, they are commonly referred to
as "Debuffs," however the game internals make very little distinction between a positive and negative buff. 

Buffs are most commonly I<calculated> as if all buffs came from generals.  This is not true, buffs come from a variety of sources, and this module forms the primative for use in any of them. 

=cut

# extends, roles, attributes, etc.

my @BuffAttributes = _initialize_attributes(); 
my @BuffConditions = _initialize_conditions();
my @BuffClasses = _initialize_classes();

subtype 'buffAttribute'
    => as Str
    => where {
        (
            grep {/^$_/} @BuffAttributes  
        )
    };

subtype 'buffCondition'
    => as Str
    => where {
        (
            grep {/^$_/} @BuffConditions  
        )
    };

subtype 'buffClass'
    => as Str
    => where {
        (
            grep {/^$_/} @BuffClasses
        )
    };

has 'attribute' => (
    is => 'ro',
    isa => 'buffAttribute'
);

has 'class' => (
    is => 'ro',
    isa => 'BuffCondition'
);

has 'condition' => (
    is => 'ro',
    isa => 'buffCondition'
);

has 'value' => (
    is  => 'ro',
    isa => 'Game::EvonyTKR::Buff::Value'
);
 
# methods

sub _initialize_attributes {
    my $self = shift;
    my $data_location = dist_file('Game-EvonyTKR', 'buff/attributes.yaml');
    open (my $DATA, '<', $data_location) or die $!;
    my $yaml = do { local $/; <$DATA> };
    my $data = Load $yaml; 
    close $DATA;

    my @BuffAttributes = $data->{'attributes'};   

    return @BuffAttributes;
    
}

sub _initialize_conditions {
    my $self = shift;
    my $data_location = dist_file('Game-EvonyTKR', 'buff/conditions.yaml');
    open (my $DATA, '<', $data_location) or die $!;
    my $yaml = do { local $/; <$DATA> };
    my $data = Load $yaml; 
    close $DATA;

    my @BuffConditions = $data->{'conditions'};   

    return @BuffConditions;
    
}

sub _initialize_classes {
    my $self = shift;
    my $data_location = dist_file('Game-EvonyTKR', 'buff/classes.yaml');
    open (my $DATA, '<', $data_location) or die $!;
    my $yaml = do { local $/; <$DATA> };
    my $data = Load $yaml; 
    close $DATA;

    my @BuffClasses = $data->{'classes'};   

    return @BuffClasses;
    
}

__PACKAGE__->meta->make_immutable;
 
1;

