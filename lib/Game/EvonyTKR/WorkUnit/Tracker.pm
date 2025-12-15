package Game::EvonyTKR::WorkUnit::Tracker;
use Mojo::Base -base, -signatures;

has 'ddb';
has 'table_name' => 'evony-work-units';

sub mark_incomplete ($self, $unit_id) {
    $self->ddb->put_item(
        TableName => $self->table_name,
        Item => {
            work_unit_id => { S => $unit_id },
            completed => { BOOL => \0 }
        }
    );
}

sub mark_complete ($self, $unit_id) {
    $self->ddb->put_item(
        TableName => $self->table_name,
        Item => {
            work_unit_id => { S => $unit_id },
            completed => { BOOL => \1 }
        }
    );
}

sub is_complete ($self, $unit_id) {
    my $result = $self->ddb->get_item(
        TableName => $self->table_name,
        Key => { work_unit_id => { S => $unit_id } }
    );
    
    return 0 unless $result->{Item};
    return $result->{Item}{completed}{BOOL} ? 1 : 0;
}

1;
