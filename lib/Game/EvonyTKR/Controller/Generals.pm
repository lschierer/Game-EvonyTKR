use v5.40.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Mojo::Home;
require Data::Printer;
require Game::EvonyTKR::BasicAttribute;
require Game::EvonyTKR::Data;
require Game::EvonyTKR::Model::General;
require YAML::PP;

package Game::EvonyTKR::Controller::Generals {
  use Mojo::Base 'Mojolicious::Controller', -role, -strict, -signatures;
  use UUID qw(uuid5);
  our $VERSION = 'v0.30.0';

  my $generals = {};
  my $data = Game::EvonyTKR::Data->new();

  sub list ($self) {
    my $jsonResponse = [];
    preSeedGenerals($self);
    for my $k (keys %{$generals}) {
      $self->app()->log()
        ->trace(sprintf(
        'generals of type %s getting added to jsonResponse', $k));
      my @gk = keys %{ $generals->{$k} };
      my $kr = {
        type    => $k,
        members => [],
      };
      for my $i (0 .. scalar @gk - 1) {
        my $n = $gk[$i];
        $self->app()->log()->trace(sprintf(
          'general %d of %d with name %s added to jsonResponse',
          $i + 1, scalar @gk, $n
        ));
        my $e = {
          name  => $n,
          id    => $generals->{$k}->{$n}->id()
        };
        push @{ $kr->{members} }, $e;
      }
      push @{$jsonResponse}, $kr;
    }
    $self->respond_to(
      txt  => { text => Data::Printer::np($jsonResponse, indent => 2) },
      json => { json => $jsonResponse },
      html => sub {
        $self->render(text => '<pre>'
            . Data::Printer::np($jsonResponse, indent => 4, colored => 0)
            . '</pre>');
      },
      any => { data => '', status => 204 },
    );
    return;
  }

  sub getUUID($self) {
    $self->app()->log()->trace(sprintf('Entering Controller::Generals->getUUID'));

    my $nameParam = $self->param('name');
    my $typeParam = $self->param('type');
    $self->app()->log()->trace(sprintf('generating UUID for %s with type %s',
      $nameParam, $typeParam));

    my $returnUUID = uuid5($data->UUID5_Generals()->{$typeParam}, $nameParam);
    $self->respond_to(
      txt  => { text => $returnUUID },
      json => { json => { UUID => $returnUUID } },
      html => sub {
        $self->render(text => '<pre>'
            . $returnUUID
            . '</pre>');
      },
      any => { data => '', status => 204 },
    );
    return;
  }

  sub preSeedGenerals($c) {
    my $ypp  = YAML::PP->new(boolean => 'JSON::PP');
    my $home = Mojo::Home->new();
    $home->detect();
    my $generalDir   = $home->child('share')->child('generals');
    my $generalFiles = $generalDir->list();
    $c->app()->log()
      ->trace(
      sprintf('collection returned %d generals', $generalFiles->size()));
    for my $element ($generalFiles->each()) {
      my $yamlData = $ypp->load_file($element->to_string());

      for my $gt (@{ $yamlData->{'general'}->{'type'} }) {
        my $rg = Game::EvonyTKR::Model::General->new(
          name => $yamlData->{'general'}->{'name'},
          type => $gt,
        );
        if (not exists $generals->{$gt}) {
          $generals->{$gt} = {};
        }

        for my $ak ($data->AttributeNames()) {
          $c->app()->log()->trace(sprintf('getting attributes from yaml for attribute %s',
            $ak));
          my $base = $yamlData->{'general'}->{'basic_attributes'}->{$ak}->{'base'};
          my $increment = $yamlData->{'general'}->{'basic_attributes'}->{$ak}->{'increment'};
          my $aObject = Game::EvonyTKR::BasicAttribute->new(
            base            => $base,
            increment       => $increment,
            attribute_name  => $ak,
          );
          $rg->basicAttributes()->setAttribute($ak,$aObject);
        }

        $generals->{$gt}->{ $rg->name() } = $rg;
        $c->app()->log()->trace(
          sprintf(
            'added %s with type %s from %s',
            $rg->name(), $rg->type(), $element->to_string()
          )
        );
      }
    }

  }

}
1;

__END__
#ABSTRACT: Controller in a Model-View-Controller take on handling Generals for the Evony TKR Game.

=head1 DESCRIPTION

Mojolicious uses a Model-View-Controller framework, this is a Controller in that framework.
This particular Controller is for handling Generals, which are central to success in the Game Evony TKR,
and the primary reason for this distribution.  This contains methods for listing the
Generals we have information about, finding and displaying the correct Model for a
particular General, and converting a General's name to UUIDv5 id string.

=cut
