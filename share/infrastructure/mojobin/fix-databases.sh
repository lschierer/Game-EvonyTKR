#!/bin/bash
# Fix corrupted SQLite databases (minion.db and persistence.db)

set -e

# Determine project root
if [ -d "/opt/mojo/app" ]; then
    # Running on EC2
    PROJECT_ROOT="/opt/mojo/app"
else
    # Running in development
    SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
    PROJECT_ROOT="$( cd "$SCRIPT_DIR/../.." && pwd )"
fi

VAR_DIR="$PROJECT_ROOT/var"
BACKUP_DIR="$VAR_DIR/backup"
TIMESTAMP=$(date +%Y%m%d_%H%M%S)

echo "SQLite Database Integrity Checker and Repair Tool"
echo "=================================================="
echo "Project root: $PROJECT_ROOT"
echo ""

# Create backup directory
mkdir -p "$BACKUP_DIR"

# Function to check and fix a database
check_and_fix_db() {
    local db_name=$1
    local db_path="$VAR_DIR/$db_name"

    echo "Checking $db_name..."

    # Check if database exists
    if [ ! -f "$db_path" ]; then
        echo "  ✓ Database does not exist (will be created on startup)"
        return 0
    fi

    # Run integrity check
    local integrity=$(sqlite3 "$db_path" "PRAGMA integrity_check;" 2>&1 || echo "error")

    if [ "$integrity" = "ok" ]; then
        echo "  ✓ Integrity check PASSED"
        return 0
    fi

    echo "  ✗ Integrity check FAILED: $integrity"
    echo "  → Backing up corrupted database..."

    # Create backup
    local backup_path="$BACKUP_DIR/${db_name}.corrupt.$TIMESTAMP"
    cp "$db_path" "$backup_path" 2>/dev/null || true
    echo "  → Backup saved to: $backup_path"

    # Remove corrupted database and WAL files
    echo "  → Removing corrupted database files..."
    rm -f "$db_path"
    rm -f "$db_path-shm"
    rm -f "$db_path-wal"

    echo "  ✓ Database files removed (will be recreated on startup)"
    return 1
}

# Check both databases
any_corrupted=0

check_and_fix_db "minion.db" || any_corrupted=1
echo ""
check_and_fix_db "persistence.db" || any_corrupted=1

# If any database was corrupted, we need to restart the service
if [ $any_corrupted -eq 1 ]; then
    echo ""
    echo "=================================================="
    echo "Action Required:"
    echo "=================================================="
    echo "One or more databases were corrupted and have been removed."
    echo "They will be recreated on next startup."
    echo ""

    # Check if we're on EC2 with systemd
    if systemctl is-active --quiet mojolicious 2>/dev/null; then
        echo "Stopping Mojolicious service..."
        sudo systemctl stop mojolicious
        echo ""
        echo "To restart the service:"
        echo "  sudo systemctl start mojolicious"
    else
        echo "Please restart your Mojolicious application."
    fi

    echo ""
    echo "Note: If minion.db was recreated, all pending jobs will be lost."
    echo "      If persistence.db was recreated, cached data will be rebuilt."
else
    echo ""
    echo "=================================================="
    echo "All databases are healthy - no action needed."
    echo "=================================================="
fi
