# SQLite EBS Compatibility Fixes

## Problem
SQLite databases were getting corrupted on EC2 instances due to incompatibility between SQLite's memory-mapped I/O and AWS EBS volumes.

## Solution Overview

### 1. EBS-Compatible SQLite Settings
Applied to both `minion.db` and `persistence.db`:

- `PRAGMA mmap_size=0` - Disables memory-mapped I/O (causes corruption on EBS)
- `PRAGMA locking_mode=NORMAL` - Ensures proper file locking
- `PRAGMA journal_mode=WAL` - Write-Ahead Logging for concurrency
- `PRAGMA synchronous=NORMAL` - Balance between safety and performance
- `PRAGMA busy_timeout=30000` - Wait up to 30 seconds for locks
- `PRAGMA cache_size=-64000` - 64MB cache for minion.db, 32MB for persistence.db

### 2. Automatic Integrity Checking & Recovery

Both databases now check integrity on startup:
- Runs `PRAGMA integrity_check` before initialization
- Automatically backs up corrupted databases to `var/backup/`
- Removes corrupted files and recreates fresh database
- Logs all recovery actions

**Files Modified:**
- `lib/Game/EvonyTKR/Plugins/Sqlite.pm` - Minion database
- `lib/Game/EvonyTKR/Service/Persistence.pm` - Persistence database
- `lib/Game/EvonyTKR.pm` - Connection event handlers

### 3. Manual Recovery Tool

**Location:** `share/infrastructure/mojobin/fix-databases.sh`

This script is deployed to `/opt/mojo/bin/` on EC2 instances.

**Usage:**
```bash
# On EC2
/opt/mojo/bin/fix-databases.sh

# In development
./share/infrastructure/mojobin/fix-databases.sh
```

**What it does:**
- Checks integrity of both databases
- Backs up corrupted files with timestamp
- Removes corrupted database files
- Provides instructions for restart

## Recovery Steps on EC2

If you encounter a corrupted database error:

```bash
# SSH into the EC2 instance
ssh luke@dev2.evonytkrtips.net

# Run the fix script
/opt/mojo/bin/fix-databases.sh

# If databases were corrupted, restart the service
sudo systemctl restart mojolicious
```

## Why This Happens

SQLite's default memory-mapped I/O (`mmap`) optimization doesn't work well with network-attached storage like EBS because:
1. EBS has different flush semantics than local disk
2. Page cache invalidation timing is different
3. Lock files may not be properly synchronized

By disabling `mmap` and using proper WAL mode, we avoid these issues.

## Automatic vs Manual Recovery

**Automatic Recovery (on startup):**
- Detects corruption during application startup
- Backs up and recreates database automatically
- No manual intervention needed

**Manual Recovery (using script):**
- For when you want to check databases proactively
- When automatic recovery fails
- When you want to preserve backup before restart

## Monitoring

Check logs for these messages:

**Healthy startup:**
```
[INFO] minion.db integrity check passed
[INFO] persistence.db integrity check passed
```

**Automatic recovery:**
```
[ERROR] Persistence database integrity check failed
[WARN] Attempting to recover by recreating database...
[INFO] Backed up corrupted database to var/backup/persistence.db.corrupt.1234567890
[INFO] Recreated persistence database
```

## Prevention

These settings should prevent corruption, but if it persists:
1. Check EBS volume health in AWS console
2. Consider switching to instance store if available
3. Increase `busy_timeout` if you see lock timeout errors
4. Review application logs for unusual access patterns
