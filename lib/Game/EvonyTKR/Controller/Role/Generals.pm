use v5.42.0;
use utf8::all;
use File::FindLib 'lib';
require Log::Log4perl::Level;

package Game::EvonyTKR::Controller::Role::Generals {
  use Mojo::Base -role,                         -signatures;
  use Mojo::Base 'Game::EvonyTKR::Role::Cache', -role;
  use List::AllUtils qw(uniq);
  use Sereal::Encoder;
  use Sereal::Decoder;
  use Carp;

  our $namespace = 'generals__';

  our $decoder = Sereal::Decoder->new();

  our $encoder = Sereal::Encoder->new({
    canonical           => 1,
    no_shared_hashkeys  => 1,
    freeze_callbacks    => 1,
    freeze_unknown      => 1,
  });


  sub create_general_cache ($self) {
    $self->logger->debug(
      sprintf(
        '%s log level is %s',
        __PACKAGE__, Log::Log4perl::Level::to_level($self->logger->level())
      )
    );
    state $cache;
    $cache = $self->create_cache({
      namespace => $namespace,
    })
      unless (defined $cache);
    return $cache;
  }

  sub add_general ($self, $name, $general, $store) {
    my $key = $name =~ s/ /_/gr;
    my $eg = $encoder->encode($general);
    my $ar  = $self->add_item($key, $eg, $store);
    $self->logger->debug(
      "attempting to add key '$key' for name '$name' was '$ar'");
    return $ar;
  }

  sub get_general ($self, $name, $store) {
    state $generals = {};
    if(exists $generals->{$self->normalize($name)}){
      $self->logger->debug("Returning general $name from local cache");
      return $generals->{$self->normalize($name)};
    }
    my $key = $name =~ s/ /_/gr;
    $key = $self->normalize($key);
    my $eg = $self->get_value($key, $store);
    if(defined($eg) && length($eg)){
      $decoder->decode($eg, my $general );
      unless(blessed($general) &&
      $general->isa('Game::EvonyTKR::Model::General') &&
      $general->name eq $name) {
        $self->logger->error(sprintf('retrieved unexpected general: expected: "%s" ne recieved: "%s" - a %s',
        $name, $general->name, blessed($general) ));
        return;
      }
      $generals->{$self->normalize($general->name)} = $general;
      return $general;
    } else {
      $self->logger->warn(sprintf('unable to retrieve encoded general for %s with key %s',
      $name, $key));
    }
  }

  sub get_generals ($self, $store) {
    my $all      = $self->get_all_items($store);
    my @keys     = keys $all->%*;
    my $keyCount = scalar @keys;
    if ($keyCount == 0) {
      $self->logger->warn(sprintf(
        'get_generals helper found %s generals', scalar keys $all->%*));
      my $all_keys = $self->get_value('_all_keys', $store) // '';
      my @keys     = sort { "$a" cmp "$b" }
        List::UtilsBy::uniq_by {"$_"} grep {length} split ',', $all_keys;
      if (length($all_keys) == 0) {
        $self->logger->warn('No keys present!');
      }
      else {
        $self->logger->warn(sprintf(
          'get_generals available keys: %s',
          join ', ', map {"'$_'"} @keys
        ));
      }
    }

    my $result = {};
    foreach my $key (keys $all->%*) {
      unless(defined($key) && length($key)){
        $self->logger->error("bogus key in get_all_items result!!");
        next;
      }
      my $ev = $all->{$key};
      unless(defined($ev) && length($ev)) {
        $self->logger->error("key '$key' points at undef!!");
        next;
      }
      $decoder->decode($ev, my $item );
      my $name  = $key =~ s/_/ /rg;
      $result->{$self->normalize($name)} = $item;
    }
    return $result;
  }
}
1;
__END__
