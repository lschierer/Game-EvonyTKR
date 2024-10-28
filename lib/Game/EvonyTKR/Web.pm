use v5.40.0;
use experimental qw(class);
use utf8::all;
use File::ShareDir ':ALL';
use File::Spec;
use File::Temp;
use Sereal;
use YAML::PP;
require Mojolicious::Routes;
use namespace::autoclean;

package Game::EvonyTKR::Web {
# ABSTRACT: package providing REST wrappers for the content created by this distribution

  use feature 'try';
  use File::FindLib 'lib';
  use Carp;
  use Mojo::Base 'Mojolicious', -signatures;
  use Mojolicious::Routes::Route;
  use Mojo::File::Share qw(dist_dir dist_file);
  use Mojo::JSON        qw(decode_json encode_json);
  use Log::Log4perl::Level;

  use Game::EvonyTKR::Web::Logger;
  use Game::EvonyTKR::Logger::Config;
  use OpenAPI::Modern;
  use JSON::Schema::Modern;

  my $logLevel = 'INFO';
  my $OpenAPISchemaCache;

  # This method will run once at server start
  sub startup ($self) {

    my $home = Mojo::Home->new;
    $home->detect;

    say "$home";

    my $dist_dir = dist_dir('Game-EvonyTKR');

    my $confFile = File::Spec->catfile($dist_dir, 'game-evony_t_k_r-web.yml');

    my $logConf = new Game::EvonyTKR::Logger::Config;
    my $logPath = $logConf->path();
    $logLevel = $self->mode() eq 'production' ? 'INFO' : 'TRACE';

    # Load configuration from config file
    my $config =
      $self->plugins->register_plugin('Mojolicious::Plugin::NotYAMLConfig',
      Mojolicious->new, { file => $confFile });

    #log request parameters at least in development mode
    $self->plugin('ParamLogger', level => 'info');

    my $wlog = Game::EvonyTKR::Web::Logger->new(category => 'Web',);
    $wlog->logInit($self->mode());
    $self->log($wlog->webLogger());

    # Configure the application
    if (my $secrets = $config->{secrets}) {
      $self->secrets($secrets);
    }

    $self->renderer->paths([File::Spec->catfile($dist_dir, 'templates')]);

    # Router
    my $r = $self->routes;
    $r->namespaces(['Game::EvonyTKR::Web::Controller']);

    #let me use the DefaultHelpers
    $self->plugin('DefaultHelpers');

    my $OpenAPISchemaFilename =
      File::Spec->catfile($dist_dir, "openapi.schema.yaml");
    my $OpenAPISchema = get_openapi($OpenAPISchemaFilename);
    $self->config({
      openapi => {
        document_filename => $OpenAPISchemaFilename,
        after_response    => \&log_responses,
      }
    });

    $self->plugin('OpenAPI::Modern', $self->config->{openapi});

    $self->plugin('Game::EvonyTKR::Web::Routes::Generals', $r);
    # Normal route to controller
    $r->get('/')->to('Example#welcome');

    $self->log()->info('start up complete');
  }

  sub log_responses($controller) {
    my $result = $controller->validate_response();
    if ($result) {
      $controller->app->log()->debug("response is valid");
    }
    else {
      $controller->app->log()->error("response is invalid", $result);
    }
  }

  sub get_openapi ($openapi_filename) {
    my $ypp     = YAML::PP->new(boolean => 'JSON::PP');
    my $newTemp = 0;
    if (not defined($OpenAPISchemaCache)) {
      $OpenAPISchemaCache = File::Temp->new();
      $newTemp            = true;
    }
    my $serialized_file = Path::Tiny::path($OpenAPISchemaCache);
    my $openapi_file    = Path::Tiny::path($openapi_filename);
    my $openapi;
    if ($newTemp or $serialized_file->stat->mtime < $openapi_file->stat->mtime)
    {
      $openapi = OpenAPI::Modern->new(
        openapi_uri    => '/',
        openapi_schema => $ypp->load_file($openapi_file)
        ,    # your openapi document
      );
      my $frozen =
        Sereal::Encoder->new({ freeze_callbacks => 1 })->encode($openapi);
      $serialized_file->spew_raw($frozen);
    }
    else {
      my $frozen = $serialized_file->slurp_raw;
      $openapi = Sereal::Decoder->new->decode($frozen);
    }

    return $openapi;
  }

}
1;

__END__
