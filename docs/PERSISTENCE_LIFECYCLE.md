# Persistence Lifecycle & Data Versioning

## Overview

The application uses **mode-based persistence** with intelligent data lifecycle management, however currently all modes are set to use Postgres for persistence.

## Data Versioning Strategy

### Problem Solved

With Postgres persistence, data survives reboots. We need to detect:

1. ✅ **Fresh data** (current git-commit) → Skip rebuild
2. ⚠️ **Stale data** (old git-commit) → Trigger rebuild
3. 🆕 **No data** (first boot) → Full rebuild
4. 🔄 **Force reload** (manual override) → Rebuild regardless

### Implementation

**Version Tracking:**
```perl
# Store git-commit when data is loaded
$persistence->set_data_version($git_commit);

# On startup, compare versions
my $stored = $persistence->get_data_version();
if ($stored eq $current_git_commit && data_exists) {
  skip_rebuild();
}
```

**Key Files:**
- `lib/Game/EvonyTKR/External/Prebuild.pm` - Orchestrator with version checking
- `lib/Game/EvonyTKR/Service/*Persistence.pm` - Version storage methods

## Prebuild Lifecycle Flow

### Startup Checks (Prebuild.pm:175-224)

```
1. Check prerequisites (all loaders registered)
   └─ Retry if not ready

2. Check FORCE_DATA_RELOAD env var
   └─ If set, skip to step 5

3. Get current git-commit from config
   └─ game-evony_t_k_r.yml -> version.git-commit

4. Compare to stored data version
   ├─ Match + data exists → FINISH (skip rebuild)
   ├─ Mismatch → LOG version change, continue
   └─ No stored version → LOG first run, continue

5. Launch loader jobs (parallel)
   ├─ load_all_generals
   ├─ load_all_books
   ├─ load_all_specialties
   ├─ load_all_covenants
   └─ ... etc

6. Launch monitors/coordinators (depends on loaders)
   └─ Pair building, conflict detection

7. Store current git-commit as data_version
   └─ Prebuild.pm:363-371
```

## Loader Idempotency

Individual loaders (e.g., `LoadAll.pm`) still check if data exists:

```perl
# From General/LoadAll.pm:65
if ($job->get_general($general_name)) {
  $job->log_debug('Skipping - already in persistence');
  next;
}
```

**Why?** This provides **retry safety**:
- If a loader job fails mid-way and retries, it won't duplicate data

## Force Reload Mechanism

**Environment Variable:**
```bash
export FORCE_DATA_RELOAD=1
```

**Use Cases:**
- Manual data refresh without changing git-commit
- Testing data pipeline
- Recovering from data corruption

**Behavior:**
- Bypasses version check

- Triggers full rebuild
- Updates stored version to current git-commit

## Development vs Production

### Development (Postgres)

- Need a easy way to flush data to restart testing
- Postgres sometimes persists _too much_ for testing every senario

### Production (Postgres)

- Data persists across reboots
- Version check critical for performance (EC2 instances have limited capacity)
- Rebuild only on deploy (git-commit change)

## Configuration

- stage specific config files
  - game-evony_t_k_r.development.yml
  - game-evony_t_k_r.staging.yml
  - ectera
- global config file: game-evony_t_k_r.yml

## Metadata Storage

Both backends store version in `metadata` table:

```perl
# Stored as JSON
{
  "data_version": "a81b322a"  # git-commit short hash
}
```

**Postgres structure:**
```
pk: "metadata"
sk: "data_version"
data: "a81b322a"
updated_at: 1702345678.123
```

## Troubleshooting

### Data not reloading after deploy

**Check:**
1. Did git-commit actually change? `cat game-evony_t_k_r.yml | grep git-commit`
2. Is stored version accessible? Check Postgres `metadata` table or SQLite query
3. Check logs for version comparison: `grep "Data version" logs/`

**Solution:**
```bash
# Force rebuild
export FORCE_DATA_RELOAD=1
systemctl restart mojolicious
```


### First deploy to new environment

**Expected behavior:**
1. No stored version → full rebuild
2. Version stored after completion
3. Subsequent reboots → fast startup (skips rebuild)

## Performance Impact

### With Versioning (Production)

- **First boot**: ~20-30 min (full data load, 1-4 jobs at a time on EC2)
- **Reboot (no deploy)**: ~10-30 sec (skips data load)
  - this speed not achieved yet. In practice we cannot reliably detect 
    which steps are necessary with good reliability yet
- **Deploy (new code)**: ~5-30 min (git-commit changed, reload)
  - wide variation based on what data has changed, if any.

## Future Enhancements

Potential improvements:

1. **Incremental updates**: Track file-level changes, only reload changed YAML
2. **TTL-based expiry**: Auto-reload data older than N days
3. **Manual version bump**: Allow version override via config
4. **Health check**: Verify data integrity on version match

## Related Files

- `lib/Game/EvonyTKR/External/Prebuild.pm` - Orchestration
- `lib/Game/EvonyTKR/Service/Persistence.pm` - Factory
- `lib/Game/EvonyTKR/Service/PostgresPersistence.pm` - Postgres backend
- `lib/Game/EvonyTKR/Role/Persistence/Core.pm` - Role composition
- `game-evony_t_k_r.yml` - Config with git-commit
