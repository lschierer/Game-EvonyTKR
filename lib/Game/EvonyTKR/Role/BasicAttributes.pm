use v5.42.0;
use experimental qw(class);
use utf8::all;

use File::FindLib 'lib';
use namespace::autoclean;

package Game::EvonyTKR::Role::BasicAttributes {
  use Mojo::Base 'Game::EvonyTKR::Role::Common';
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::BuffConstants',    -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::GeneralConstants', -role;
  use Carp;
  use List::AllUtils qw( any none first );
  use Data::Printer;
  require JSON::PP;
# VERSION

  use File::FindLib 'lib';

  sub setAttribute($self, $attributeName, $newAttribute) {
    if (none { $_ =~ $attributeName } $self->basic_types->@*) {
      $self->logger->error(sprintf(
        'attributeName must be one of %s, not %s',
        Data::Printer::np($self->AttributeValues),
        $attributeName,
      ));
      return;
    }

    unless (ref($newAttribute)
      && $newAttribute->isa('Game::EvonyTKR::Model::BasicAttribute')) {
      $self->logger->error(sprintf(
        'newAttribute must be a %s not a %s',
        'Game::EvonyTKR::Model::BasicAttribute',
        blessed $newAttribute
      ));
      return;
    }
    if ($self->can($attributeName)) {
      $self->$attributeName($newAttribute);
    }
    else {
      $self->logger->error(sprintf(
        'cannot find method "%s" in "%s"',
        $attributeName, blessed($self)
      ));
    }

  }
}
1;
__END__
