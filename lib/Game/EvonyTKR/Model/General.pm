package Game::EvonyTKR::Model::General {
    use strict;
    use warnings;
    use JSON::PP;
    use Game::EvonyTKR::Model::BasicAttributes;

    use overload
        '""'       => \&as_string,
        'eq'       => \&equality,
        'bool'     => sub { $_[0]->_isTrue() },
        "fallback" => 1;

    our $VERSION = 'v0.40.0';

    sub new ($class, %args) {
        my $self = {
            id                  => $args{id} // undef,
            name                => $args{name} // undef,
            type                => $args{type} // undef,
            ascending           => $args{ascending} // 0,
            ascendingAttribute  => $args{ascendingAttribute} // undef,
            stars               => $args{stars} // 'none',
            basicAttributes     => Game::EvonyTKR::Model::BasicAttributes->new(),
            builtInBookName     => $args{builtInBookName} // undef,
            builtInBook         => undef,
            specialtyNames      => $args{specialtyNames} // [],
            specialties         => [],
        };
        bless $self, $class;
        return $self;
    }

    sub to_hash {
        my $self = shift;
        return {
            id              => $self->{id},
            name            => $self->{name},
            type            => $self->{type},
            basicAttributes => $self->{basicAttributes},
            ascending       => $self->{ascending},
            builtInBookName => $self->{builtInBookName},
            specialtyNames  => $self->{specialtyNames},
        };
    }

    sub TO_JSON {
        my $self = shift;
        return JSON::PP->new->utf8(1)->pretty->canonical(1)
            ->allow_blessed(1)
            ->convert_blessed(1)
            ->encode($self->to_hash());
    }

    sub as_string {
        my $self = shift;
        my $json = JSON::PP->new->utf8(0)->pretty->canonical(1)
            ->allow_blessed(1)
            ->convert_blessed(1)
            ->encode($self->to_hash());
        return $json;
    }

    sub equality($self, $other, $swap = 0 ){
      my $one = $swap ? $other  : $self;
      my $two = $swap ? $self   : $other;
      my $on = '';
      my $tn = '';
      if(ref($one) && $one->isa('Game::EvonyTKR::Model::General')) {
        $on = $one->{name};
      } else {
        $on = "$one";
      }
      if(ref($two) && $two->isa('Game::EvonyTKR::Model::General')){
        $tn = $two->{name};
      } else {
        $tn = "$two";
      }
      return $on eq $tn;
    }

}

1;

__END__
