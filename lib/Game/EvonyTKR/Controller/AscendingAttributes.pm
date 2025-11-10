use v5.42.0;
use experimental qw(class);
use utf8::all;
use File::FindLib 'lib';
require Game::EvonyTKR::Model::AscendingAttributes;
require Data::Printer;
use namespace::clean;

package Game::EvonyTKR::Controller::AscendingAttributes {
  use Mojo::Base 'Game::EvonyTKR::Controller::ControllerBase', -strict, -signatures;
  use Mojo::Base 'Game::EvonyTKR::Role::Logger', -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Common';
  use Mojo::Base 'Game::EvonyTKR::Controller::Role::AscendingAttributes', -role;
  use Mojo::Base 'Game::EvonyTKR::Role::Constants::AscendingAttributes', -role;
  use List::AllUtils qw(uniq first);
  use Carp;

  # Specify which collection this controller handles
  sub collection_name {'ascending attributes'}

  my $base = '/Ascending Attributes';

  sub getBase($self) {
    return $base;
  }

  sub controller_name($self) {
    return 'AscendingAttributes';    # Explicitly return the controller name
  }

  sub get_all_ascending_attributes ($c, $app) {
    state %aa_by_general;  # normalized general name -> AA object
    state $sig;

    return _hydrate_from_list(
      $c, $app,
      sub ($app2) { $c->list_ascending_attributes($app2) },   # expected AAs
      sub ($aa_name) { $c->get_ascending_attribute($aa_name) },
      \%aa_by_general,
      \$sig,
    );
  }

  sub register($c, $app, $config = {}) {
    $c->logger->info("Registering routes for " . __PACKAGE__);
    $c->SUPER::register($app, $config);

    my $distDir    = Mojo::File::Share::dist_dir('Game::EvonyTKR');
    my $collection = $c->collection_name;
    my $SourceDir  = $distDir->child("collections/$collection");

    $c->logger->info("Successfully loaded Ascending Attributes "
        . "manager with collection from $SourceDir");

    $app->helper(
      get_ascending_attributes => sub {
        return $c->get_ascending_attributes();
      }
    );

    $app->helper(
      get_ascendingattributes_for_general => sub ($self, $g) {
        return $c->get_ascendingattributes_for_general($g);
      }
    );

    $app->helper(
      get_column_info => sub($self, $item) {
        return $c->get_column_info($item);
      }
    );

    $app->helper(
      sort_ascending_levels => sub($self, $item) {
        return $c->sort_ascending_levels($item);
      }
    );

    $app->helper(
      ascending_level_names => sub($self, $level = '', $printable = 0) {
        $c->logger->debug(sprintf(
          'ascending_level_names helper started, level is %s, printable is %s',
          defined $level     ? $level     : '',
          defined $printable ? $printable : 0,
        ));

        # Case 1: Return all level values (for dropdown values)
        if (length($level) == 0) {
          if ($printable) {
            # Return all printable names
            my @purpleNames =
              $c->SUPER::getConstants()->AscendingAttributeLevelNames(0);
            my @redNames =
              $c->SUPER::getConstants()->AscendingAttributeLevelNames(1);

            # Combine and get unique values
            my %combined;
            foreach my $ln (@purpleNames, @redNames) {
              $combined{$ln}++;
            }
            my @unique = sort keys(%combined);
            $c->logger->debug("derived unique keys " . join(', ', @unique));
            return \@unique;
          }
          else {
            # Return all internal values
            my @purpleValues =
              $c->SUPER::getConstants()->AscendingAttributeLevelValues(0);
            my @redValues =
              $c->SUPER::getConstants()->AscendingAttributeLevelValues(1);

            # Combine and get unique values
            my %combined;
            foreach my $ln (@purpleValues, @redValues) {
              $combined{$ln}++;
            }
            my @unique = sort keys(%combined);
            $c->logger->debug("derived unique keys " . join(', ', @unique));
            return \@unique;
          }
        }

        # Case 2: Return a specific level's name
        if ($printable) {
          return $c->SUPER::getConstants()->AscendingAttributeLevelName($level);
        }
        else {
          if ($level =~ /red/) {
            return
              first { $_ =~ /level/ }
              $c->SUPER::getConstants()->AscendingAttributeLevelValues(1);
          }
          else {
            return
              first { $_ =~ /level/ }
              $c->SUPER::getConstants()->AscendingAttributeLevelValues(0);
          }
        }
        return 'none';
      }
    );

    $app->helper(
      get_ascending_section => sub ($self, $name = '') {
        $c->logger->debug(
          sprintf('in get_ascending_section helper, self is %s ',
            blessed($self))
        );
        $c->logger->debug(sprintf(
          'in get_ascending_section helper, c is %s, c->app is %s',
          blessed($c), defined($c->app) ? blessed($c->app) : 'undefined'
        ));
        return $c->get_ascending_section($self, $name);
      }
    );
  }

  sub get_ascending_section ($c, $caller, $name = '') {
    if (length($name)) {
      my $item = $c->get_ascendingattributes_for_general($name);
      if ( Scalar::Util::reftype($item) eq 'OBJECT'
        && blessed($item) eq 'Game::EvonyTKR::Model::AscendingAttributes') {
        $c->logger->debug("rendering get_ascending_section for $name");
        return $caller->render_to_string(
          item     => $item,
          template => '/ascending attributes/details',
          layout   => undef
        );
      }
      else {
        $c->logger->warn(
          "get_ascending_section cannot find Ascending Attributes for $name");
        $c->logger->debug(sprintf(
          "searching for $name, instead got %s %s",
          Scalar::Util::reftype($item),
          blessed($item)
        ));
      }
    }
    else {
      $c->logger->warn("cannot get_ascending_section without a name");
    }
    return "";
  }

  sub get_ascendingattributes_for_general ($c, $g) {

    my $nn;
    if (Scalar::Util::blessed($g) && $g->isa('Game::EvonyTKR::Model::General'))
    {
      $nn = $g->normalize($g->name);
    }
    else {
      $nn = $c->normalize($g);
    }
    $c->logger->debug("looking for attributes for $nn");

    my $all = $c->get_ascending_attributes();
    my $aa  = $all->{$nn};
    unless (defined $aa) {
      $c->logger->error(sprintf(
        'no ascending attributes found for '.'general named "%s" normalized to "%s"',
        $g->name, $nn
      ));
      $c->logger->debug(sprintf(
        'available ascending attributes are %s',
        join ', ', map { sprintf('"%s"', $_) } sort keys $all->%*
      ));
    }
    return $aa;
  }

  sub import_single_aa_file ($c, $app, $fileName, $delay) {
    my $all = $c->get_ascending_attributes();
    $c->logger->debug("processing $fileName");

    my $data       = $fileName->slurp('UTF-8');
    my $hashObject = YAML::PP->new(
      schema       => [qw/ + Perl /],
      yaml_version => ['1.2', '1.1'],
    )->load_string($data);

    my $aa = Game::EvonyTKR::Model::AscendingAttributes->from_hash($hashObject);
    unless ($aa) {
      $c->logger->error(
        sprintf('failed to build ascending attribute from %s', $fileName));
      next;
    }
    $all->{ $c->SUPER::getConstants->normalize($aa->general) } = $aa;
    $c->logger->debug(sprintf(
      'after delay of %s, imported "%s" as "%s" from "%s"',
      $delay,                                           $aa->general,
      $c->normalize($aa->general), $fileName
    ));
    if ($app) {
      $app->plugins->emit(
        single_ascending_attributes_imported => { attribute => $aa });
    }
    else {
      if ($c) {
        # the problem si that I'm getting to this error log line.
        $c->logger->error(
          '$app was undefined.  $c is: ' . Scalar::Util::blessed($c));
      }
    }

  }

  sub show ($self) {
    return $self->SUPER::show();
  }

  sub get_column_info($self, $item) {
    # Define the two possible sets of level names
    my %level_sets = (
      # First set (e.g., for regular generals)
      purple => {
        levels      => ['purple1', 'purple2', 'purple3', 'purple4', 'purple5'],
        title       => 'Purple Ascending Attributes',
        description => 'Attributes for purple generals'
      },
      # Second set (e.g., for special generals)
      red => {
        levels      => ['red1', 'red2', 'red3', 'red4', 'red5'],
        title       => 'Red Ascending Attributes',
        description => 'Attributes for red generals'
      }
    );

    # Default to regular if we can't determine
    my $set_type = 'purple';

    # Check if item has ascending levels
    if ( $item
      && $item->ascending
      && ref $item->{ascending} eq 'ARRAY'
      && @{ $item->{ascending} }) {
      # Get the first level name
      my $first_level = $item->{ascending}[0]{level};

      # Determine which set it belongs to
      if (grep { $_ eq $first_level } @{ $level_sets{special}{levels} }) {
        $set_type = 'red';
      }
    }

    return $level_sets{$set_type};
  }

  sub sort_ascending_levels($self, $item) {
    # Get column info to determine the level order
    my $column_info = $self->get_column_info($item);
    my %level_order;

    # Create a mapping of level names to their order
    my $i = 0;
    foreach my $level (@{ $column_info->{levels} }) {
      $level_order{$level} = $i++;
    }

    # Return sorted array if ascending exists
    if ($item && $item->{ascending} && ref $item->{ascending} eq 'ARRAY') {
      return [
        sort {
          ($level_order{ $a->{level} } // 999)
            <=> ($level_order{ $b->{level} } // 999)
        } @{ $item->{ascending} }
      ];
    }

    # Return empty array if no ascending data
    return [];
  }

}

1;
__END__
