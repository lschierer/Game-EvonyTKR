use v5.40;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Data::Printer;
require File::Share;
require JSON::PP;
require MIME::Base64;
require Path::Tiny;
require YAML::PP;
require Game::EvonyTKR;
require Game::EvonyTKR::External::Buff::Computer;
require Game::EvonyTKR::Model::AscendingAttributes;
require Game::EvonyTKR::Model::BasicAttribute;
require Game::EvonyTKR::Model::BasicAttributes;
require Game::EvonyTKR::Model::Book;
require Game::EvonyTKR::Model::Buff::Summarizer;
require Game::EvonyTKR::Model::Buff;
require Game::EvonyTKR::Model::Covenant;
require Game::EvonyTKR::Model::Data;
require Game::EvonyTKR::Model::General;
require Game::EvonyTKR::Model::Specialty;
require Game::EvonyTKR::Role::Book::Builtin;
require Game::EvonyTKR::Role::Book::SkillBook;
require Game::EvonyTKR::Shared::Constants;

package Game::EvonyTKR::External::Buff::Worker {
  use Mojo::Base 'Game::EvonyTKR::External::JobBase', -signatures;
  use experimental qw(class);
  use Carp;

  my $logger;

  sub register ($self, $app, $conf) {
    $logger = Log::Log4perl->get_logger(__PACKAGE__);

    $app->minion->add_task(
      pair_worker => sub ($job, $args) {
        eval {
          $logger->debug(sprintf(
            'pair worker job starting for runId %s pair %s/%s',
            $args->{runId}, $args->{general1}, $args->{general2},
          ));
          my $debug  = $app->mode eq 'development';
          my $worker = Game::EvonyTKR::External::Buff::Computer->new(
            debug_enabled => $debug);
          $worker->load_generals($job->info->{task});
          my $result = $worker->calculate_buffs($args);
          $logger->debug(sprintf('result is %s', $result));
          $job->note(result => $result);
          my $runId = $args->{runId};
          $job->finish({
            status => 'complete',
            result => $result,
          });
        };
        if ($@) {
          $logger->error("Job " . $job->id . " failed: $@");
          $job->fail($@);
        }

      }
    );
  }

}
1;

__END__
