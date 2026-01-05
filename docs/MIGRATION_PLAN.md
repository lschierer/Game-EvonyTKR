# Game-EvonyTKR Migration Plan: Mojolicious → PAGI/Future::AsyncAwait

## Executive Summary

Migration from Mojolicious to PAGI-WebServer with Future::AsyncAwait to improve performance and scalability on EC2 T4g.large instances. Current deployment handles ~500 users (5-10 concurrent) but suffers from performance constraints under load.

**Primary Goals:**
1. Replace Mojolicious HTTP framework with PAGI-WebServer
2. Replace Minion job queue with Future::AsyncAwait async processing
3. Convert Mojolicious templates (.html.ep) to Template Toolkit (.tt)
4. Maintain or improve performance while reducing resource usage
5. Preserve existing functionality

## Current Architecture

### Tech Stack
- **Web Framework:** Mojolicious with Hypnotoad (4 workers, 5 clients/worker)
- **Async Processing:** Minion (Postgres backend)
- **Templates:** Mojolicious Embedded Perl (.html.ep)
- **Routing:** Mojolicious plugin auto-discovery pattern
- **Database:** Postgres (persistence + Minion queue)
- **Compute:** PDL (Perl Data Language) for high-performance calculations

### Application Structure

```
lib/Game/EvonyTKR.pm                    # Main Mojolicious app
lib/Game/EvonyTKR/Controller/           # Route handlers (plugin pattern)
lib/Game/EvonyTKR/External/             # Minion background jobs
lib/Game/EvonyTKR/Model/                # Domain objects
lib/Game/EvonyTKR/Service/              # Shared services (PDL, Postgres)
lib/Game/EvonyTKR/Role/                 # Shared traits
templates/                               # .html.ep templates
share/collections/data/                  # YAML game data
```

### Current Deployment Config
- 4 Hypnotoad workers
- 5 concurrent clients per worker = **20 total concurrent requests**
- 3600s inactivity timeout
- 1000 max accepts
- Postgres for Minion queue + data persistence

## Performance Bottlenecks Identified

### 1. **Synchronous Request Handling**
- Mojolicious uses fork-based workers
- Each worker blocks on synchronous operations
- Limited to 5 concurrent requests per worker
- **Impact:** Request queueing under load, timeouts

### 2. **Minion Overhead**
- Postgres polling for job queue
- Fork overhead for job execution
- Context switching between web workers and job workers
- **Impact:** Latency on data-intensive operations

### 3. **Template Rendering**
- Mojolicious templates compiled but still interpreted
- No caching layer visible
- **Impact:** CPU overhead on each request

### 4. **Database Connection Pool**
- Limited connections shared across workers
- No visible connection pooling strategy
- **Impact:** Connection exhaustion under concurrent load

## Migration Strategy

### Phase 1: Infrastructure Setup ✓ (Already Done for App-Schierer-HPFan)

- [x] PAGI-WebServer base framework
- [x] Template Toolkit integration
- [x] Static asset serving
- [x] Navigation system
- [x] Markdown rendering

### Phase 2: Core Migration (Priority 1)

#### 2A: Main Application Structure

**Replace:** `lib/Game/EvonyTKR.pm` (Mojolicious app)
**With:** `bin/server.pl` (PAGI-WebServer)

Key changes:
- Remove Mojolicious startup logic
- Remove Minion initialization
- Remove Hypnotoad worker spawning
- Add PAGI request handler
- Add Future::AsyncAwait event loop

**Files to create:**
- `bin/server.pl` - Main entry point
- `lib/Game/EvonyTKR/WebServer.pm` - PAGI request handler

#### 2B: Configuration Migration

**Replace:** YAML config files
**With:** Perl-based config or simplified YAML

Current:
```yaml
hypnotoad:
  workers: 4
  clients: 5
  # ... other Hypnotoad settings
```

Target PAGI config:
```perl
# In bin/server.pl or separate config
my $config = {
    listen => '127.0.1.1:3000',
    workers => 4,  # Can be higher with async
    max_concurrent => 100,  # Much higher than 20!
    # ...
};
```

### Phase 3: Controllers to Route Handlers (Priority 1)

Convert Mojolicious controller plugins to PAGI route handlers.

**Pattern Conversion:**

**Before (Mojolicious):**
```perl
package Game::EvonyTKR::Controller::Generals;
use Mojo::Base 'Game::EvonyTKR::Controller::ControllerBase';

sub register($c, $app, $config = {}) {
    my $routes = $app->routes;
    $routes->get('/Generals/:id')->to('generals#details');
}

sub details($c) {
    my $id = $c->param('id');
    my $general = $c->load_general($id);
    $c->render(template => 'generals/details', item => $general);
}
```

**After (PAGI):**
```perl
package Game::EvonyTKR::Controller::Generals;
use Future::AsyncAwait;

sub register($app) {
    $app->add_route(
        GET => qr{^/Generals/([^/]+)$},
        \&details
    );
}

async sub details($req, $params) {
    my $id = $params->[0];
    my $general = await load_general_async($id);

    return render_template('generals/details.tt', {
        item => $general,
        title => "General Details: " . $general->name,
    });
}
```

**Controllers to migrate:**
- [Priority 1] `Generals.pm` - Core functionality
- [Priority 1] `Root.pm` - Homepage
- [Priority 2] `Books.pm`
- [Priority 2] `Covenants.pm`
- [Priority 2] `Pairs.pm`
- [Priority 3] `AscendingAttributes.pm`
- [Priority 3] `Specialties.pm`
- [Priority 3] `ConflictGroups.pm`
- [Priority 3] `Glossary.pm`
- [Priority 3] `Monsters.pm`
- [Priority 3] `PvP.pm`

### Phase 4: Async Job Processing (Priority 1)

Replace Minion with Future::AsyncAwait for background processing.

**Current Minion Pattern:**
```perl
# Enqueue job
$app->minion->enqueue(load_all_generals => [{}]);

# Job definition (in External/)
sub register($class, $minion, $app) {
    $minion->add_task(load_all_generals => sub ($job, $args) {
        # Heavy processing
        $job->finish(\%result);
    });
}
```

**Target Future::AsyncAwait Pattern:**
```perl
# Option 1: Simple async sub
async sub load_all_generals_async() {
    my @generals;
    for my $file (@yaml_files) {
        my $data = await read_file_async($file);
        push @generals, parse_general($data);
    }
    return \@generals;
}

# Option 2: Promise-based for parallelism
sub load_all_generals_parallel() {
    my @futures = map {
        read_and_parse_async($_)
    } @yaml_files;

    return Future->wait_all(@futures);
}

# Usage in route
async sub index($req) {
    my $generals = await load_all_generals_async();
    return render_template('generals/index.tt', { items => $generals });
}
```

**External jobs to migrate:**
- [Critical] `External/Prebuild.pm` - Orchestrator
- [Critical] `External/General/LoadAll.pm`
- [Critical] `External/General/Pair/*.pm` - Pair generation
- [Priority 1] `External/Book/*.pm`
- [Priority 1] `External/Covenant/*.pm`
- [Priority 2] `External/AscendingAttributes/*.pm`
- [Priority 2] `External/Specialty/*.pm`
- [Priority 3] `External/Conflicts/*.pm`

### Phase 5: Template Migration (Priority 2)

Convert Mojolicious .html.ep templates to Template Toolkit .tt

**Syntax Conversion Guide:**

| Mojolicious EP | Template Toolkit | Notes |
|---|---|---|
| `<%= $var %>` | `[% var %]` | HTML-escaped output |
| `<%== $html %>` | `[% html %]` | Raw HTML output |
| `% if ($cond) {` | `[% IF cond -%]` | Control structure |
| `<% foreach my $item (@items) { %>` | `[% FOREACH item IN items -%]` | Iteration |
| `<%= include 'partial' %>` | `[% INCLUDE partials/name.tt %]` | Includes |
| `% layout 'default';` | *Set in controller* | Layout specification |
| `$c->param('id')` | `$params.id` | Template variables |
| `<%= url_for(...) %>` | *Helper function* | URL generation |

**Example Conversion:**

**Before (.html.ep):**
```html
% layout 'default';
% title 'General Details: ' . $item->name;

<div class="general-details">
  <% if( defined($item) ) { %>
    <h2><%= $item->name %></h2>
    <table>
      % foreach my $attr (@{$item->attributes}) {
        <tr>
          <td><%= $attr->name %></td>
          <td><%= $attr->value %></td>
        </tr>
      % }
    </table>
  <% } %>
</div>
```

**After (.tt):**
```html
[%# Layout set in controller %]
<div class="general-details">
  [% IF item -%]
    <h2>[% item.name %]</h2>
    <table>
      [% FOREACH attr IN item.attributes -%]
        <tr>
          <td>[% attr.name %]</td>
          <td>[% attr.value %]</td>
        </tr>
      [% END -%]
    </table>
  [% END -%]
</div>
```

**Templates to migrate:**
- `templates/layouts/default.html.ep` → `templates/layouts/default.tt`
- `templates/generals/*.html.ep` → `templates/generals/*.tt`
- `templates/pairs/*.html.ep` → `templates/pairs/*.tt`
- All other template directories

### Phase 6: Service Layer Updates (Priority 2)

Services likely need minimal changes, but review for Mojolicious dependencies:

- [x] `Service/PDL/Runtime.pm` - Should be framework-agnostic ✓
- [x] `Service/Postgres.pm` - May need connection pool updates
- [x] `Service/Log4perl.pm` - Should work as-is ✓

### Phase 7: Model Layer (Priority 3)

Models should be mostly framework-agnostic, but review for:
- Mojolicious-specific methods
- Blocking I/O that could be made async
- Database access patterns

Files likely OK as-is:
- `Model/General.pm`
- `Model/Buff/*.pm`
- `Model/Book.pm`
- etc.

### Phase 8: Performance Optimizations (Priority 1)

#### 8A: Async Database Access

**Replace:** Blocking Postgres queries
**With:** Non-blocking async queries

```perl
# Before (blocking)
sub load_general($id) {
    my $dbh = get_dbh();
    my $row = $dbh->selectrow_hashref(
        "SELECT * FROM generals WHERE id = ?",
        undef,
        $id
    );
    return Game::EvonyTKR::Model::General->new($row);
}

# After (async)
async sub load_general_async($id) {
    my $row = await db_query_async(
        "SELECT * FROM generals WHERE id = ?",
        [$id]
    );
    return Game::EvonyTKR::Model::General->new($row);
}
```

#### 8B: Parallel Data Loading

**Current:** Sequential file loads
**Target:** Parallel with Future::wait_all

```perl
# Before (sequential)
sub load_all_generals() {
    my @generals;
    for my $file (@files) {
        my $data = load_yaml($file);
        push @generals, parse_general($data);
    }
    return \@generals;
}

# After (parallel)
async sub load_all_generals_parallel() {
    my @futures = map {
        load_and_parse_general_async($_)
    } @files;

    my @generals = await Future->wait_all(@futures);
    return \@generals;
}
```

#### 8C: Caching Strategy

Add caching at multiple levels:
1. **Template caching** - Template Toolkit handles this
2. **Data caching** - Cache parsed YAML in memory
3. **Result caching** - Cache rendered pages/API responses

```perl
use CHI;

my $cache = CHI->new(
    driver => 'Memory',
    global => 1,
    max_size => 100 * 1024 * 1024,  # 100MB
);

async sub get_general_cached($id) {
    my $cached = $cache->get("general:$id");
    return $cached if $cached;

    my $general = await load_general_async($id);
    $cache->set("general:$id", $general, {expires_in => '1 hour'});
    return $general;
}
```

#### 8D: Connection Pooling

Implement proper Postgres connection pooling:

```perl
use DBI;
use DBD::Pg;

my $pool = DBI->connect(
    "dbi:Pg:dbname=$db;host=$host",
    $user, $pass,
    {
        RaiseError => 1,
        AutoCommit => 1,
        pg_enable_utf8 => 1,
        # Connection pooling
        pg_prepare_now => 0,
        pg_server_prepare => 0,
    }
);
```

## Testing Strategy

### Unit Tests
- Convert existing Test2::V0 tests to work with new structure
- Add async test support
- Maintain test coverage

### Integration Tests
- Test PAGI routes match Mojolicious routes
- Verify template output identical
- Check async operations complete correctly

### Performance Tests
- Benchmark before/after migration
- Load testing with concurrent users
- Memory usage profiling

### Acceptance Criteria
- [ ] All routes return identical output
- [ ] Performance improves by ≥30%
- [ ] Can handle ≥50 concurrent users (vs current 20)
- [ ] Memory usage ≤current or better
- [ ] All tests pass

## Rollout Plan

### Development Environment
1. Create `pagi-migration` branch
2. Implement Phase 1-2 (core structure)
3. Convert 1-2 controllers as proof of concept
4. Benchmark and compare

### Staging
1. Deploy complete migration to staging
2. Run integration tests
3. Performance benchmarking
4. Fix issues

### Production
1. Blue-green deployment strategy
2. Monitor metrics closely
3. Rollback plan ready
4. Gradual traffic shift

## Risk Mitigation

### High Risk Items
1. **Database connection pooling** - May need tuning
2. **Async job completion** - Replace Minion reliability
3. **Memory usage** - Ensure no leaks in async code
4. **Error handling** - Async errors need careful handling

### Mitigation Strategies
1. Extensive testing before rollout
2. Keep Mojolicious version running in parallel initially
3. Feature flags for gradual rollout
4. Monitoring and alerting on key metrics
5. Documented rollback procedure

## Expected Performance Improvements

### Current Constraints
- 20 concurrent requests (4 workers × 5 clients)
- Request queueing under load
- Timeouts on heavy computations

### Target Performance
- ≥100 concurrent requests (async event loop)
- No request queueing (non-blocking I/O)
- Faster response times (parallel data loading)
- Lower memory usage (less fork overhead)

### Metrics to Monitor
- Request latency (p50, p95, p99)
- Throughput (requests/second)
- Memory usage (RSS, heap)
- CPU usage
- Database connection pool utilization
- Error rate

## Timeline Estimate

- **Phase 1:** Already complete ✓
- **Phase 2-3:** Core migration - 2-3 weeks
- **Phase 4:** Async processing - 1-2 weeks
- **Phase 5:** Template migration - 1 week
- **Phase 6-7:** Services/Models - 1 week
- **Phase 8:** Performance optimization - 1-2 weeks
- **Testing & Validation:** 1-2 weeks
- **Deployment:** 1 week

**Total estimated:** 8-12 weeks for complete migration

## Next Steps

1. Create detailed task breakdown for Phase 2
2. Set up performance benchmarking baseline
3. Create proof-of-concept with one controller
4. Review and refine plan based on POC results
5. Begin systematic migration

## Notes

- Keep this document updated as migration progresses
- Document any deviations from plan
- Track performance metrics throughout
- Capture lessons learned for future reference
