package Game::EvonyTKR::WorkUnit::Tracker;
use Mojo::Base -base, -signatures;

has 'persistence';

sub mark_incomplete ($self, $unit_id) {
    $self->persistence->store_data('work_units', $unit_id, { completed => 0 });
}

sub mark_complete ($self, $unit_id) {
    $self->persistence->store_data('work_units', $unit_id, { completed => 1 });
}

sub is_complete ($self, $unit_id) {
    my $data = $self->persistence->get_data('work_units', $unit_id);
    return 0 unless $data;
    return $data->{completed} ? 1 : 0;
}

1;
