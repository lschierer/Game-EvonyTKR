use v5.40.0;
use experimental qw(class);
use FindBin;
use lib "$FindBin::Bin/../../../../../lib";

class Game::EvonyTKR::Web::Model::General : isa(Game::EvonyTKR::Logger) {
# PODNAME: Game::EvonyTKR::Web::Model::General
# ABSTRACT: Route Handler for the /general and routes.
    use builtin qw(indexed);
    use Carp;
    use Types::Common qw( t is_Num is_Str is_Int);
    use Type::Utils   qw(is enum);
    use Util::Any -all;
    use Data::Printer;
    use namespace::autoclean;

}