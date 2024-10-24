#!/usr/bin/env perl
use v5.40.0;
use experimental qw(class);
use utf8::all;
use YAML::XS qw{ LoadFile Load };
use OpenAPI::Modern;
use File::Share qw(dist_dir dist_file);

my $OpenAPISchemaCache;
my $dist_dir = dist_dir('Game-EvonyTKR');

my $OpenAPISchemaFilename = File::Spec->catfile($dist_dir, "openapi.schema.yaml");
my $OpenAPISchema = get_openapi($OpenAPISchemaFilename);

sub get_openapi ($openapi_filename) {
    my $newTemp = 0;
    if(not defined($OpenAPISchemaCache)) {
      $OpenAPISchemaCache = File::Temp->new();
      $newTemp = true;
    }
    my $serialized_file = Path::Tiny::path($OpenAPISchemaCache);
    my $openapi_file = Path::Tiny::path($openapi_filename);
    my $openapi;
    if ($newTemp or $serialized_file->stat->mtime < $openapi_file->stat->mtime) {
      $openapi = OpenAPI::Modern->new(
        openapi_uri => '/',
        openapi_schema => Load($openapi_file->slurp_raw), # your openapi document
      );
      my $frozen = Sereal::Encoder->new({ freeze_callbacks => 1 })->encode($openapi);
      $serialized_file->spew_raw($frozen);
    }
    else {
      my $frozen = $serialized_file->slurp_raw;
      $openapi = Sereal::Decoder->new->decode($frozen);
    }

    return $openapi;
  }
