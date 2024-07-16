use 5.38.0;
package Game::EvonyTKR::Buff;
use Moose;
use File::ShareDir ':ALL';
use YAML::XS;
use Moose::Util::TypeConstraints;
use Data::Dumper qw(Dumper);

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

my @BuffAttributes; 
subtype 'buffAttribute'
    => as Str
    => where {
        $Self->_initialize_attributes;
        (
            grep(/^$_/, @BuffAttributes) 
        )
    };

has 'attribute' => (
    is => 'ro',
    isa => 'buffAttribute'
);

has 'class' => (
    is => 'ro'
);

has 'condition' => (
    is => 'ro'
);


 
# methods

sub _initialize_attributes {
    my $self = shift;
    my $data_location = dist_file('Game-EvonyTKR', 'buff/attributes.yaml');
    open (my $DATA, '<', $data_location) or die $!;
    my $yaml = do { local $/; <$DATA> };
    my $data = Load $yaml; 

    @BuffAttributes = $data->{'attributes'};   
    
}

__PACKAGE__->meta->make_immutable;
 
1;

__DATA__
@@ attributes
