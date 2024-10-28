use v5.40.0;
use experimental qw(class);
use File::FindLib 'lib';

package Game::EvonyTKR::Web::Controller::Generals {
  use Carp;
  use Data::Printer;
  use Devel::Peek;
  use File::ShareDir ':ALL';
  use File::Spec;
  use Util::Any ':all';
  use Game::EvonyTKR::Data;
  use Game::EvonyTKR::General;
  use Game::EvonyTKR::Web::Model::Generals;
  use HTML::FromANSI;
  use namespace::autoclean;
  use Mojo::Base 'Mojolicious::Controller', -strict, -signatures;
# VERSION
  use File::FindLib 'lib';
  use Mojo::JSON qw(decode_json encode_json);

  my $generalModel = Game::EvonyTKR::Web::Model::Generals->new();

  my $EvonyData = Game::EvonyTKR::Data->new();

  sub index {
    my ($self) = @_;
    my $text = "Generals Pages";
    $self->respond_to(
      txt  => { text => $text },
      html => sub {
        $self->render(text => $text);
      },
      any => { data => '', status => 204 },
    );
    return;
  }

  sub list {
    my ($self) = @_;
    my $jsonResponse = {};
    if (defined $generalModel) {
      for my $type ($EvonyData->GeneralKeys()) {
        if (defined $generalModel->generals()->{$type}) {
          $jsonResponse->{$type} = $generalModel->generals()->{$type};
        }
      }
    }
    $self->respond_to(
      txt  => { text => Data::Printer::np($jsonResponse, indent => 2) },
      json => { json => $jsonResponse },
      html => sub {
        $self->render(
          text => ansi2html(
            Data::Printer::np($jsonResponse, indent => 4, colored => 0)
          )
        );
      },
      any => { data => '', status => 204 },
    );
    return;
  }

  sub _specialityParamHelper($c, $name) {
    my @specialityLevels = qw( None None None None None );

    my $specialityLevelEnum = $EvonyData->specialityLevels();
    my $t                   = $specialityLevelEnum->compiled_check();

    for my $sl (1 .. 4) {
      my $sp = $c->param("specialityLevel$sl");
      if (defined $sp) {
        if ($t->($sp)) {
          $c->log()
            ->debug(sprintf(
            'setting %s at specialityLevel%d for %s', $sp, $sl, $name));
          @specialityLevels[$sl - 1] = $sp;
        }
        else {
          $c->log()->warn(sprintf(
            'invalid specialityLevel %s at %d for %s', $sp, $sl, $name
          ));
          $c->log()->warn(sprintf('valid values are %s',
            Data::Printer::np $specialityLevelEnum->values()));
        }
      }
    }
    $c->log()->debug(sprintf('detected specialityLevels %s',
      Data::Printer::np @specialityLevels));
    return @specialityLevels;
  }

  sub _ascendingParamHelper($c, $name) {
    my $ascendingLevel = $c->param('ascendingLevel');
    if (defined $ascendingLevel) {
      $c->log()->debug("Query ascendingLevel is '$ascendingLevel'");
      if ($ascendingLevel =~ /[1-5](Red|Purple)/) {
        $c->log()->trace("returning $ascendingLevel");
        return $ascendingLevel;
      }
      else {
        $c->log()
          ->warn("Detected bad input '$ascendingLevel' reading ascendingLevel");
      }
    }
    else {
      $c->log()->debug("Query ascendingLevel is not defined.");
    }
  }

  sub GetGeneral {
    my ($self) = @_;
    $self->log()->trace("get in Controller::General");

    my $nameParam = $self->param('name');
    $self->log()->trace("looking for $nameParam in GetGeneral");

    if (not defined $generalModel) {
      #this should be unnecessary and never actually be reached.
      $generalModel = Game::EvonyTKR::Web::Model::Generals->new();
    }
    my $general = $generalModel->get_by_name($nameParam);

    if (not defined $general or not $general) {
      $self->log()->error('no id from Model');
      $self->reply->not_found();
    }
    else {
      my $gc  = blessed $general;
      my @gcl = split(/::/, $gc);
      if ($gcl[2] !~ /general/i) {
        $self->log()
          ->error(
          sprintf('general is a %s instead of a general', blessed $general));
        $self->reply->not_found();
      }
      else {
        $self->log()->trace('general found');
        my $verbose = $self->param('verbose');
        if (defined $verbose and $verbose ne 'false') {
          $verbose = 1;
        }
        else {
          $verbose = 0;
        }

        my $level = $self->param('level');
        if (defined $level) {
          $self->log()->debug("Query level is '$level'");
          $general->setLevel(0+ $level);
        }
        else {
          $self->log()->debug("Query level is not defined.");
        }

        my @specialityLevels = $self->_specialityParamHelper($nameParam,);
        foreach my $i (0 .. $#specialityLevels) {
          my $sp = $specialityLevels[$i];
          my $sl = $i + 1;
          $general->changeActiveSpecialityLevel($sl, $sp);
        }

        my $ascendingLevel = $self->_ascendingParamHelper($nameParam);
        $general->ascendingAttributes()->setActiveLevel($ascendingLevel);

        my $generalHashRef = {};
        $$generalHashRef{'data'} = $general->toHashRef($verbose);
        $self->respond_to(
          json => { json => $generalHashRef },
          html => {
            text => ansi2html(Data::Printer::np(
              $generalHashRef,
              indent  => 4,
              colored => 0
            )),
          },
          any => { data => '', status => 204 },
        );
        return;
      }
    }
  }

}
1;
__END__

# ABSTRACT: Restify controller depicting the REST actions for the /accounts collection.
