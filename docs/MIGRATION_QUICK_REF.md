# Quick Reference: Mojolicious → PAGI/Future::AsyncAwait

## Side-by-Side Comparison

### Application Entry Point

#### Mojolicious (Current)
```perl
#!/usr/bin/env perl
# bin/game-evonytkr
use Mojolicious::Commands;
Mojolicious::Commands->start_app('Game::EvonyTKR');
```

#### PAGI (Target)
```perl
#!/usr/bin/env perl
# bin/server.pl
use PAGI::WebServer;
use Game::EvonyTKR::WebServer;

my $server = Game::EvonyTKR::WebServer->new(
    port => 3000,
    host => '127.0.1.1',
);

$server->run();
```

---

### Route Definition

#### Mojolicious
```perl
sub register($c, $app, $config = {}) {
    my $routes = $app->routes;

    # Simple route
    $routes->get('/Generals/:id')->to('generals#details');

    # Route with callback
    $routes->get('/Generals')->to(cb => sub ($c) {
        my $generals = load_all_generals();
        $c->render(
            template => 'generals/index',
            items => $generals
        );
    });
}
```

#### PAGI
```perl
sub register($app) {
    # Simple route
    $app->add_route(
        GET => qr{^/Generals/([^/]+)$},
        \&details
    );

    # Inline handler
    $app->add_route(
        GET => qr{^/Generals$},
        async sub ($req, $params) {
            my $generals = await load_all_generals_async();
            return render_template('generals/index.tt', {
                items => $generals
            });
        }
    );
}
```

---

### Request Handler

#### Mojolicious
```perl
sub details($c) {
    # Get params
    my $id = $c->param('id');

    # Load data (blocking)
    my $general = load_general($id);

    # Render template
    $c->render(
        template => 'generals/details',
        item => $general,
        title => "General: " . $general->name
    );
}
```

#### PAGI (Async)
```perl
async sub details($req, $params) {
    # Get params from capture groups
    my $id = $params->[0];

    # Load data (non-blocking)
    my $general = await load_general_async($id);

    # Render template
    return render_template('generals/details.tt', {
        item => $general,
        title => "General: " . $general->name
    });
}
```

---

### Template Syntax

#### Mojolicious (.html.ep)
```html
% layout 'default';
% title 'Generals List';

<h1><%= $title %></h1>

<% if (@{$items}) { %>
  <ul>
    % foreach my $item (@{$items}) {
      <li>
        <a href="<%= url_for('/Generals/' . $item->id) %>">
          <%= $item->name %>
        </a>
      </li>
    % }
  </ul>
<% } else { %>
  <p>No generals found.</p>
<% } %>

<!-- Include partial -->
<%= include 'partials/footer' %>

<!-- Raw HTML -->
<%== $html_content %>
```

#### Template Toolkit (.tt)
```html
[%# Layout set in controller %]

<h1>[% title %]</h1>

[% IF items.size -%]
  <ul>
    [% FOREACH item IN items -%]
      <li>
        <a href="/Generals/[% item.id %]">
          [% item.name %]
        </a>
      </li>
    [% END -%]
  </ul>
[% ELSE -%]
  <p>No generals found.</p>
[% END -%]

[%# Include partial %]
[% INCLUDE partials/footer.tt %]

[%# Raw HTML - TT doesn't escape by default %]
[% html_content %]
```

---

### Background Jobs

#### Mojolicious + Minion
```perl
# Enqueue job
$app->minion->enqueue(
    load_all_generals => [{}] => {
        priority => 10,
        attempts => 3,
    }
);

# Define job
package Game::EvonyTKR::External::General::LoadAll;

sub register($class, $minion, $app) {
    $minion->add_task(
        load_all_generals => sub ($job, $args) {
            my @generals;

            # Process files sequentially
            for my $file (@files) {
                my $data = load_yaml($file);
                push @generals, parse_general($data);
            }

            # Save to database
            save_generals(@generals);

            $job->finish({ count => scalar @generals });
        }
    );
}
```

#### PAGI + Future::AsyncAwait
```perl
# Call async function
async sub on_startup() {
    my $result = await load_all_generals_async();
    say "Loaded $result->{count} generals";
}

# Define async function
async sub load_all_generals_async() {
    # Process files in parallel
    my @futures = map {
        load_and_parse_async($_)
    } @files;

    my @generals = await Future->wait_all(@futures);

    # Save to database (async)
    await save_generals_async(@generals);

    return { count => scalar @generals };
}

# Helper for single file
async sub load_and_parse_async($file) {
    my $data = await read_yaml_async($file);
    return parse_general($data);
}
```

---

### Database Access

#### Mojolicious (Blocking)
```perl
sub load_general($id) {
    my $dbh = get_dbh();

    my $row = $dbh->selectrow_hashref(
        "SELECT * FROM generals WHERE id = ?",
        undef,
        $id
    );

    return Game::EvonyTKR::Model::General->new($row);
}
```

#### PAGI (Async)
```perl
async sub load_general_async($id) {
    my $row = await db_query_one_async(
        "SELECT * FROM generals WHERE id = ?",
        [$id]
    );

    return Game::EvonyTKR::Model::General->new($row);
}

# Helper function
async sub db_query_one_async($sql, $params) {
    my $future = Future->new;

    # Non-blocking query
    $dbh->do_async($sql, $params, sub ($err, $result) {
        return $future->fail($err) if $err;
        $future->done($result->[0]);
    });

    return await $future;
}
```

---

### Error Handling

#### Mojolicious
```perl
sub details($c) {
    my $id = $c->param('id');

    eval {
        my $general = load_general($id);
        $c->render(
            template => 'generals/details',
            item => $general
        );
    };

    if ($@) {
        $c->log->error("Error loading general: $@");
        $c->render(
            template => 'error',
            status => 500,
            message => "Failed to load general"
        );
    }
}
```

#### PAGI (Async)
```perl
async sub details($req, $params) {
    my $id = $params->[0];

    try {
        my $general = await load_general_async($id);
        return render_template('generals/details.tt', {
            item => $general
        });
    }
    catch ($err) {
        log_error("Error loading general: $err");
        return render_error(500, "Failed to load general");
    }
}
```

---

### Session Handling

#### Mojolicious
```perl
sub details($c) {
    # Get session
    my $user_id = $c->session('user_id');

    # Set session
    $c->session(last_viewed => time());

    # Flash message
    $c->flash(message => 'General updated');
}
```

#### PAGI
```perl
async sub details($req, $params) {
    # Get session
    my $session = get_session($req);
    my $user_id = $session->{user_id};

    # Set session
    $session->{last_viewed} = time();
    save_session($req, $session);

    # Flash message (need to implement)
    set_flash($session, message => 'General updated');
}
```

---

### Helpers

#### Mojolicious
```perl
# Register helper
$app->helper(format_date => sub ($c, $date) {
    return strftime('%Y-%m-%d', localtime($date));
});

# Use in controller
sub details($c) {
    my $formatted = $c->format_date(time());
}

# Use in template
%= format_date($item->created_at)
```

#### PAGI
```perl
# Define helper function
sub format_date($date) {
    return strftime('%Y-%m-%d', localtime($date));
}

# Use in controller
async sub details($req, $params) {
    my $formatted = format_date(time());
}

# Use in template (register as TT filter or function)
[% USE date %]
[% date.format(item.created_at, '%Y-%m-%d') %]

# Or pass formatted data
return render_template('details.tt', {
    formatted_date => format_date($item->created_at)
});
```

---

### Static Files

#### Mojolicious
```perl
# Automatic from public/ directory
# /public/css/style.css → /css/style.css
```

#### PAGI
```perl
# In bin/server.pl
$app->add_route(
    GET => qr{^/(css|js|images)/(.+)$},
    sub ($req, $params) {
        my ($dir, $file) = @$params;
        return serve_static("public/$dir/$file");
    }
);
```

---

### Testing

#### Mojolicious
```perl
use Test::Mojo;

my $t = Test::Mojo->new('Game::EvonyTKR');

$t->get_ok('/Generals/123')
  ->status_is(200)
  ->text_is('h1' => 'General Details')
  ->content_like(qr/Attack/);
```

#### PAGI
```perl
use Test2::V0;
use HTTP::Request;

my $app = Game::EvonyTKR::WebServer->new;

my $req = HTTP::Request->new(GET => '/Generals/123');
my $res = $app->handle_request($req);

is($res->code, 200, 'status ok');
like($res->content, qr/<h1>General Details<\/h1>/, 'title present');
like($res->content, qr/Attack/, 'content present');
```

---

## Key Differences Summary

| Feature | Mojolicious | PAGI + Future::AsyncAwait |
|---------|-------------|---------------------------|
| **Concurrency Model** | Fork-based (4 workers × 5 clients = 20 max) | Event loop (100+ concurrent) |
| **Request Handling** | Blocking by default | Non-blocking with `async/await` |
| **Template Engine** | Embedded Perl (.ep) | Template Toolkit (.tt) |
| **Background Jobs** | Minion (Postgres queue) | Future::AsyncAwait (in-process) |
| **Route Matching** | String patterns | Regex patterns |
| **Session** | Built-in | Manual implementation |
| **Helpers** | App-level helpers | Regular functions |
| **Static Files** | Automatic serving | Manual route setup |
| **Testing** | Test::Mojo | Test2::V0 + manual |

---

## Performance Comparison

### Current (Mojolicious)
- **Max concurrent:** 20 requests
- **Worker model:** 4 processes (fork overhead)
- **I/O:** Blocking (request queuing)
- **Memory:** ~50MB per worker = 200MB base

### Target (PAGI)
- **Max concurrent:** 100+ requests
- **Worker model:** Single event loop (no fork)
- **I/O:** Non-blocking (no queuing)
- **Memory:** ~100MB total (estimated)

### Expected Improvements
- **3-5x** increase in concurrent request handling
- **40-60%** reduction in memory usage
- **30-50%** improvement in response time (p95)
- **Better** resource utilization on T4g.large

---

## Common Patterns

### Loading Data in Parallel

#### Sequential (Slow)
```perl
my @items;
for my $id (@ids) {
    push @items, await load_item_async($id);  # Each waits for previous
}
```

#### Parallel (Fast)
```perl
my @futures = map { load_item_async($_) } @ids;
my @items = await Future->wait_all(@futures);  # All run concurrently
```

### Caching Pattern

```perl
use CHI;

my $cache = CHI->new(
    driver => 'Memory',
    max_size => 100_000_000,  # 100MB
);

async sub get_cached($key, $loader) {
    my $cached = $cache->get($key);
    return $cached if defined $cached;

    my $value = await $loader->();
    $cache->set($key, $value, { expires_in => '1 hour' });
    return $value;
}

# Usage
my $general = await get_cached(
    "general:$id",
    async sub { await load_general_async($id) }
);
```

### Error Recovery

```perl
async sub load_with_retry($loader, $max_attempts = 3) {
    for my $attempt (1..$max_attempts) {
        try {
            return await $loader->();
        }
        catch ($err) {
            if ($attempt == $max_attempts) {
                die "Failed after $max_attempts attempts: $err";
            }
            log_warn("Attempt $attempt failed, retrying: $err");
            await Future->sleep(0.5 * $attempt);  # Exponential backoff
        }
    }
}
```

---

## Migration Checklist per Controller

- [ ] Convert `register()` method to PAGI route definitions
- [ ] Add `async` keyword to handler subs
- [ ] Replace `$c->param()` with `$params` array
- [ ] Replace `$c->render()` with `render_template()`
- [ ] Convert blocking DB calls to async
- [ ] Update template path (.ep → .tt)
- [ ] Test route produces identical output
- [ ] Benchmark performance improvement

---

## Gotchas and Tips

### 1. Always `await` Futures
```perl
# WRONG - returns Future object, not value
my $general = load_general_async($id);

# RIGHT - waits for completion
my $general = await load_general_async($id);
```

### 2. Regex Capture Groups
```perl
# Route: qr{^/Generals/([^/]+)$}
async sub details($req, $params) {
    my $id = $params->[0];  # First capture group
    # NOT: $c->param('id')
}
```

### 3. Template Variables
```perl
# Pass as hash ref
return render_template('page.tt', {
    item => $general,
    title => "General",
});

# NOT: individual params like Mojolicious
```

### 4. Error Handling
```perl
# Async errors need try/catch
try {
    await $operation;
}
catch ($err) {
    log_error($err);
}

# NOT: eval { ... } if ($@)
```

### 5. Parallel Operations
```perl
# Create all futures first
my @futures = map { async_operation($_) } @items;

# Then wait for all
my @results = await Future->wait_all(@futures);

# NOT: await in loop (sequential)
```

---

## Resources

- **Future::AsyncAwait:** https://metacpan.org/pod/Future::AsyncAwait
- **Template Toolkit:** https://template-toolkit.org/
- **PAGI Docs:** (In PAGI-WebServer/docs/)
- **Migration Plan:** See MIGRATION_PLAN.md
