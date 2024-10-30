use v5.40.0;
use experimental qw(class);
use utf8::all;
use MojoX::Log::Log4perl;
use Mojo::File qw(curfile);
use Mojo::File::Share qw(dist_dir dist_file);
use File::FindLib 'lib';
use OpenAPI::Modern;
require Path::Tiny;
use Sereal::Encoder;
use Sereal::Decoder;
use YAML::PP;

package Game::EvonyTKR::Plugin::Route::Base {
  use Mojo::Base 'Mojolicious::Plugin', -role, -signatures;
  use Carp;
  our $VERSION = 'v0.30.0';

  my $OpenAPISchemaCache;

  sub register ($self, $app, $conf) {
    $app->routes->get('/')->to('Example#welcome');

    $app->plugin('Route::Generals', $app->routes);

    my $home = Mojo::Home->new();

    my $OpenAPISchemaFilename = $home->child('share')->child('openapi.schema.yaml');
    $app->log()->trace(sprintf('Schema File is at path %s', $OpenAPISchemaFilename));
    my $OpenAPISchema = get_openapi($OpenAPISchemaFilename);

    $app->config({
      openapi => {
        document_filename => $OpenAPISchemaFilename,
        after_response    => \&log_responses,
      }
    });

    $app->plugin('OpenAPI::Modern', $app->config->{openapi});

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

#ABSTRACT: the base router for the root of the distribution

=pod

=head1 DESCRIPTION

I have pulled the route handling out into plugins to keep the main file
as clean as possible.  This particular one handles the base routes, and as
such also initializes the OpenAPI plugin.

=cut
