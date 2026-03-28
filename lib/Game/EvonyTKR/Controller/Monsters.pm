package Game::EvonyTKR::Controller::Monsters;
use v5.42.0;
use utf8::all;
use Mooish::Base -standard;
use Future::AsyncAwait;
extends 'Game::EvonyTKR::Controller::ControllerBase';

with 'Game::EvonyTKR::Role::Constants::MonsterConstants';

=head1 NAME

Game::EvonyTKR::Controller::MonsterSimulator - Controller for Monster Simulator

=head1 DESCRIPTION

Handles the monster combat simulator at /Tools/MonsterSimulator.

=cut

my $base = '/Monsters';

sub controller_name ($self) {
  return "Monsters";
}

sub getBase ($self) {
  return $base;
}

sub build ($self) {
  $self->logger->info("Building Monsters controller");

  # Call parent to register common routes
  $self->SUPER::build();

  # Add navigation
  $self->add_navigation_route($base, 'Monsters',
    { order => 10, parent => '/' });

  $self->add_navigation_route(
    "${base}/Simulator",
    'Monster Hunting Simulator',
    { order => 10, parent => $base }
  );

  # Main simulator page (form)
  $self->router->add(
    "${base}/Simulator",
    {
      to => sub ($self, $ctx) {
        return $self->index($ctx);
      },
      action => 'http.*',
    }
  );

  # Calculate endpoint (POST)
  $self->router->add(
    "$base/calculate",
    {
      to => sub ($self, $ctx) {
        return $self->calculate($ctx);
      },
      action => 'http.POST',
    }
  );

  # API endpoint for monster search (AJAX)
  $self->router->add(
    "$base/api/monsters",
    {
      to => sub ($self, $ctx) {
        return $self->api_monsters($ctx);
      },
      action => 'http.GET',
    }
  );

  # API endpoint for monster levels by name (AJAX)
  # Register explicit routes for each known monster name to avoid
  # percent-encoding issues with spaces in :name path parameters.
  my $monsters_loader = $self->monsters_loader();
  if ($monsters_loader) {
    for my $name ($monsters_loader->list_unique_names()->@*) {
      $self->router->add(
        "$base/api/monster-levels/$name",
        {
          to => sub ($self, $ctx) {
            return $self->_monster_levels_response($ctx, $name);
          },
          action => 'http.GET',
        }
      );
    }
  }

  $self->logger->info("Registered MonsterSimulator routes");
}

sub index ($self, $ctx) {
  $self->logger->debug("Rendering monster simulator form");

  my $monsters_loader = $self->monsters_loader();

  unless ($monsters_loader) {
    return $self->render_error($ctx, 500, "Monster data not loaded");
  }

  # Get unique monster names for the dropdown
  my $monster_names = $monsters_loader->list_unique_names();
  my $boss_monsters = $monsters_loader->list_boss_monsters();

  # Build base stats JSON for calibration modal
  my $monster_simulator_data = $self->monster_simulator_data();
  use JSON::MaybeXS ();
  my $json = JSON::MaybeXS->new(utf8 => 0);
  my $base_stats_json = $json->encode({
    attack  => $monster_simulator_data->troop_base_attack,
    defense => $monster_simulator_data->troop_base_defense,
    hp      => $monster_simulator_data->troop_base_hp,
  });

  my $vars = {
    monster_names     => $monster_names,
    boss_monsters     => $boss_monsters,
    tiers             => $self->TierValues,
    troop_types       => $self->MonsterTroopTypes,
    troop_type_labels => $self->MonsterTroopTypeLabels,
    march_types       => $self->MarchTypes,
    march_type_labels => $self->MarchTypeLabels,
    linkBase          => $base,
    title             => 'Monster Simulator',
    current_year      => (localtime)[5] + 1900,
    sidebar           => 1,
    navigation        => $self->render_navigation($ctx->req->path),
    site_logo         => $self->site_logo(),
    css_files         => ['/css/monsterSimulator.css'],
    base_stats_json   => $base_stats_json,

    # Default values for form
    defaults => {
      tier        => 'T14',
      troop_type  => 'mounted',
      march_type  => 'solo',
      troop_count => 500_000,
    },
  };

  return $self->template('monster_simulator/index.tt', $vars);
}

async sub calculate ($self, $ctx) {
  $self->logger->debug("Processing monster simulation");

  my $monsters_loader        = $self->monsters_loader();
  my $monster_simulator_data = $self->monster_simulator_data();

  unless ($monsters_loader && $monster_simulator_data) {
    return $self->render_error($ctx, 500, "Simulator data not loaded");
  }

  # Parse form parameters
  my $form   = await $ctx->req->form_params;
  my $params = $form->as_hashref;

  my $monster_order = $params->{monster_order};
  my $tier          = $params->{tier}        // 'T15';
  my $troop_type    = $params->{troop_type}  // 'mounted';
  my $march_type    = $params->{march_type}  // 'solo';
  my $troop_count   = $params->{troop_count} // 500_000;

  # Parse buff percentages (convert from percent input to decimal)
  my $buffs = {
    attack => {
      basic   => ($params->{attack_basic}   // 0) / 100,
      march   => ($params->{attack_march}   // 0) / 100,
      monster => ($params->{attack_monster} // 0) / 100,
      misc    => ($params->{attack_misc}    // 0) / 100,
      rally   => ($params->{attack_rally}   // 0) / 100,
      flat    => $params->{attack_flat} // 0,
    },
    defense => {
      basic   => ($params->{defense_basic}   // 0) / 100,
      march   => ($params->{defense_march}   // 0) / 100,
      monster => ($params->{defense_monster} // 0) / 100,
      misc    => ($params->{defense_misc}    // 0) / 100,
      rally   => ($params->{defense_rally}   // 0) / 100,
      flat    => $params->{defense_flat} // 0,
    },
    hp => {
      basic   => ($params->{hp_basic}   // 0) / 100,
      march   => ($params->{hp_march}   // 0) / 100,
      monster => ($params->{hp_monster} // 0) / 100,
      misc    => ($params->{hp_misc}    // 0) / 100,
      rally   => ($params->{hp_rally}   // 0) / 100,
      flat    => $params->{hp_flat} // 0,
    },
  };

  # Handle direct-entry buff totals: if user typed a total but category
  # breakdowns are all zero, use the total as the basic category
  for my $stat (qw(attack defense hp)) {
    my $total_from_form = ($params->{"total_${stat}"} // 0) / 100;
    my $category_sum = $buffs->{$stat}{basic} + $buffs->{$stat}{march}
      + $buffs->{$stat}{monster} + $buffs->{$stat}{misc} + $buffs->{$stat}{rally};
    if ($total_from_form > 0 && $category_sum == 0) {
      $buffs->{$stat}{basic} = $total_from_form;
    }
  }

  # Parse debuffs
  my $debuffs = {
    monster_attack  => ($params->{monster_attack_debuff}  // 0) / 100,
    monster_defense => ($params->{monster_defense_debuff} // 0) / 100,
    troop_attack    => ($params->{troop_attack_debuff}    // 0) / 100,
    troop_defense   => ($params->{troop_defense_debuff}   // 0) / 100,
    troop_hp        => ($params->{troop_hp_debuff}        // 0) / 100,
  };

  # Count unknowns (fields marked as unknown)
  my $unknowns_count = 0;
  for my $field (qw(
    attack_basic attack_march attack_monster attack_misc attack_rally
    defense_basic defense_march defense_monster defense_misc defense_rally
    hp_basic hp_march hp_monster hp_misc hp_rally
  )) {
    $unknowns_count++ if $params->{"${field}_unknown"};
  }

  # Create simulator service
  require Game::EvonyTKR::Service::MonsterSimulator;
  my $simulator = Game::EvonyTKR::Service::MonsterSimulator->new(
    monsters_loader => $monsters_loader,
    reference_data  => $monster_simulator_data,
  );

  # Run simulation
  my $result = $simulator->simulate({
    monster_order          => $monster_order,
    tier                   => $tier,
    troop_type             => $troop_type,
    march_type             => $march_type,
    troop_count            => $troop_count,
    buffs                  => $buffs,
    debuffs                => $debuffs,
    alliance_boss_modifier => $params->{alliance_boss_modifier},
    unknowns_count         => $unknowns_count,
  });

  if ($result->{error}) {
    return $self->render_error($ctx, 400, $result->{error});
  }

  # Get monster names for dropdown (for re-rendering form)
  my $monster_names = $monsters_loader->list_unique_names();

  my $vars = {
    result            => $result,
    params            => $params,
    monster_names     => $monster_names,
    tiers             => $self->TierValues,
    troop_types       => $self->MonsterTroopTypes,
    troop_type_labels => $self->MonsterTroopTypeLabels,
    march_types       => $self->MarchTypes,
    march_type_labels => $self->MarchTypeLabels,
    linkBase          => $base,
    title             => 'Monster Simulator - Results',
    current_year      => (localtime)[5] + 1900,
    sidebar           => 1,
    navigation        => $self->render_navigation($ctx->req->path),
    site_logo         => $self->site_logo(),
    css_files         => ['/css/monsterSimulator.css'],
    unknowns_count    => $unknowns_count,
  };

  return $self->template('monster_simulator/results.tt', $vars);
}

sub api_monsters ($self, $ctx) {
  my $monsters_loader = $self->monsters_loader();

  unless ($monsters_loader) {
    $ctx->res->headers(content_type => 'application/json');
    return '{"error": "Monster data not loaded"}';
  }

  my $query = $ctx->req->query_param('q') // '';

  my $results;
  if ($query) {
    $results = $monsters_loader->search($query);
  }
  else {
    $results = $monsters_loader->list_unique_names();
  }

  use JSON::MaybeXS ();
  my $json = JSON::MaybeXS->new(utf8 => 1);

  $ctx->res->headers(content_type => 'application/json');
  return $json->encode($results);
}

sub _monster_levels_response ($self, $ctx, $name) {
  my $monsters_loader = $self->monsters_loader();

  unless ($monsters_loader) {
    $ctx->res->headers(content_type => 'application/json');
    return '{"error": "Monster data not loaded"}';
  }

  my $levels = $monsters_loader->get_levels_for_name($name);

  # Also get the order numbers for each level
  my @level_data;
  for my $level (@$levels) {
    my $monster = $monsters_loader->get_by_name_and_level($name, $level);
    if ($monster) {
      push @level_data,
        {
        level => $level,
        order => $monster->order,
        };
    }
  }

  use JSON::MaybeXS ();
  my $json = JSON::MaybeXS->new(utf8 => 1);

  $ctx->res->headers(content_type => 'application/json');
  return $json->encode(\@level_data);
}

1;
__END__

=head1 ROUTES

=head2 GET /Tools/MonsterSimulator

Displays the monster simulator form.

=head2 POST /Tools/MonsterSimulator/calculate

Processes the simulation and displays results.

=head2 GET /Tools/MonsterSimulator/api/monsters

Returns JSON list of monsters (optionally filtered by query parameter 'q').

=head2 GET /Tools/MonsterSimulator/api/monster-levels/:name

Returns JSON list of available levels for a monster name.

=head1 AUTHOR

Game::EvonyTKR Development Team

=cut
