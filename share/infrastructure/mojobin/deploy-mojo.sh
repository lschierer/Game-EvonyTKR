#!/bin/bash
set -e

# Define variables for service and app paths for clarity
SERVICE_NAME="mojolicious"
APP_PATH="/opt/mojo/app"
APP_USER="mojo"

echo "Pulling latest code..."
sudo -u ${APP_USER} -s /bin/bash -l -c "cd ${APP_PATH} && git reset --hard"
sudo -u ${APP_USER} -s /bin/bash -l -c "cd ${APP_PATH} && git pull"

echo "Stopping service..."
# Stop the service cleanly
sudo systemctl stop ${SERVICE_NAME}

echo "Building application..."
sudo -u ${APP_USER} -s /bin/bash -l -c "cd ${APP_PATH} && mise install"
sudo -u ${APP_USER} -s /bin/bash -l -c "cd ${APP_PATH} && mise reshim"
sudo -u ${APP_USER} -s /bin/bash -l -c "cd ${APP_PATH} && perl Build.PL"
sudo -u ${APP_USER} -s /bin/bash -l -c "cd ${APP_PATH} && ./Build installdeps --cpan_client 'cpanm -n'"
sudo -u ${APP_USER} -s /bin/bash -l -c "cd ${APP_PATH} && pnpm config set childConcurrency 2"
sudo -u ${APP_USER} -s /bin/bash -l -c "cd ${APP_PATH} && export NODE_OPTIONS=--max_old_space_size=1536; pnpm install"
sudo -u ${APP_USER} -s /bin/bash -l -c "cd ${APP_PATH} && rsync -a --delete share/images/ share/public/images/"
sudo -u ${APP_USER} -s /bin/bash -l -c "cd ${APP_PATH} && rsync -a --delete share/collections/data/images/generals/ share/public/images/generals/"
sudo -u ${APP_USER} -s /bin/bash -l -c "cd ${APP_PATH} && pnpm config set childConcurrency 1"
sudo -u ${APP_USER} -s /bin/bash -l -c "cd ${APP_PATH} && export NODE_OPTIONS=--max_old_space_size=1536; pnpm tsx ./scripts/build-ts.ts"
sudo -u ${APP_USER} -s /bin/bash -l -c "cd ${APP_PATH} && pnpm config set childConcurrency 2"
sudo -u ${APP_USER} -s /bin/bash -l -c "cd ${APP_PATH} && ./Build manifest"
sudo -u ${APP_USER} -s /bin/bash -l -c "cd ${APP_PATH} && perl ./scripts/update_git_meta.pl"
sudo -u ${APP_USER} -s /bin/bash -l -c "cd ${APP_PATH} && ./Build"

echo "Restarting workers"
sudo systemctl restart "${SERVICE_NAME}-worker"
sudo systemctl status "${SERVICE_NAME}-worker"

# Start the new version
sudo systemctl start ${SERVICE_NAME}

echo "Checking service status..."
sudo systemctl status ${SERVICE_NAME}

echo "Deployment complete!"
