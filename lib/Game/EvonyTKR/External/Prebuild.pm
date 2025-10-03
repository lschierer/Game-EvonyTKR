use v5.40;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require File::Share;
require JSON::PP;
require Log::Log4perl;
require MIME::Base64;
require Path::Tiny;
require Game::EvonyTKR;
require Game::EvonyTKR::Logger::Config;
require Game::EvonyTKR::Shared::Constants;
require Game::EvonyTKR::Model::General;
require Game::EvonyTKR::External::General::PairBuilder;

package Game::EvonyTKR::External::Prebuild {
  use Mojo::Base 'Mojolicious::Plugin', -signatures;
  use experimental qw(class);
  use Carp;

  my $logger;

  sub register ($self, $app, $conf = {}) {
    $logger = Log::Log4perl->get_logger(__PACKAGE__);

    $app->plugins->on(generals_loaded => sub ($plugin, $data) {
      my $generals = $data->{generals};
      unless($generals && ref($generals) eq 'HASH'){
        $logger->error(sprintf('in %s generals_loaded callback, generals is a "%s" rather than a HASH',
        __PACKAGE__, defined($generals) ? ref($generals) : 'Undefined' ));
        return;
      }
      # args to minion workers are JSON encoded, and must be HASH type objects.
      # send a list of names to the workers and reconstruct the generals there.
      my @general_names = map { $_->name } sort {$a->name cmp $b->name } values $generals->%*;
      my $jid = $app->minion->enqueue(PairBuilder_kickoff => [{
        general_names => \@general_names,
      }]);
    });

    $app->minion->add_task(PairBuilder_kickoff => sub ($job, $args){
      return $job->finish('PairBuilder_kickoff launched')
        unless my $pbGuard = $app->minion->guard('PairBuilder_kickoff', 3600);
      my $generals = $args->{general_names};
      unless($generals && ref($generals) eq 'ARRAY'){
        $logger->error(sprintf('PairBuilder_kickoff requires a general_names of type ARRAY not "%s"', defined($generals) ? ref($generals) : 'Undefined'));
        return $job->finish('Invalid Generals');
      }
      my $pb = Game::EvonyTKR::External::General::PairBuilder->new(
        app       => $app,
        job       => $job,
        general_names  => $generals,
      );
      return $pb->execute();
    });
  }
}
1;
__END__
