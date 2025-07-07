use v5.42.0;
use experimental qw(class);
use utf8::all;
require Path::Tiny;
use Log::Log4perl;
use namespace::autoclean;

class Game::EvonyTKR::Model::Logger {
# PODNAME: Game::EvonyTKR::Model::Logger
  use Carp;
  our $VERSION = 'v0.30.0';

  field $location : reader;

  field $logger : reader;

  field $category : reader : param = __CLASS__;

  ADJUST {
    $self->get_logger();
  }

  method get_logger {
    return $logger if defined $logger;
    Log::Log4perl::Config->utf8(1);
    $logger = Log::Log4perl->get_logger($category)
      ;    # $category is a field set to __CLASS__
    return $logger;
  }

  method toHashRef {
    return {};
  }

  # Method for JSON serialization
  method TO_JSON {
    return $self->to_hash();
  }

  # Stringification method using JSON
  method as_string {
    my $json =
      JSON::PP->new->utf8->pretty->allow_blessed(1)
      ->convert_blessed(1)
      ->encode($self->to_hash());
    return $json;
  }

}
1;

__END__

# ABSTRACT: Set up and manage logging for the distribution

=pod

=head1 DESCRIPTION

This is intended as an abstract class of sorts from which to inherit uniform methods
for using logging.

=cut

=head1 METHODS

=method new()

instantiate the class. This will also configure the logging.

=cut

=method logger()

returns the logger for use in logging messages.

=cut
=cut
