#!/bin/bash
set -e

# --- Initial setup as root ---
# The log volume path, as mounted during `docker run` or by docker-compose
LOG_VOLUME_PATH="/home/mojo/var"
# The working directory where your app expects to run
APP_DIR="/opt/Game-EvonyTKR"
# Your non-privileged user
APP_USER="mojo"

echo "Running entrypoint script as root to set up permissions..."

# Check if the log directory from the volume exists
if [ -d "$LOG_VOLUME_PATH" ]; then
  echo "Log volume path found already present at $LOG_VOLUME_PATH."

else
  echo "Creating log directory on log volume at $LOG_VOLUME_PATH..."
  # Create necessary subdirectories if they don't exist
  mkdir -p "$LOG_VOLUME_PATH"

fi

echo "Fixing Permissions for $LOG_VOLUME_PATH"
chown -R "$APP_USER":"$APP_USER" "$LOG_VOLUME_PATH"
chmod -R 755 "$LOG_VOLUME_PATH"
echo "Permission setup complete. Switching user and directory."

# --- Drop privileges and run the main command ---
# Change to the application's required directory
cd "$APP_DIR" || { echo "Error: Could not change to $APP_DIR"; exit 1; }

# Execute the application command using gosu to switch user and preserve arguments
echo "Executing application as user '$APP_USER' in directory '$PWD'..."

if [[ -z "${MOJO_MODE}" ]]; then
  MOJO_MODE='production'
fi
pwd
ls -l ./bin/game-evonytkr

exec gosu "$APP_USER" perl ./bin/game-evonytkr prefork -m "${MOJO_MODE}"
