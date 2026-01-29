#!/bin/bash
set -e

# Define variables for service and app paths for clarity
SERVICE_NAME="evonytkr"
APP_HOME="/opt/prefix"
APP_PATH="${APP_HOME}/app"
PAGI_PATH="${APP_HOME}/PAGI-WebServer"
APP_USER="appuser"

echo "Pulling latest code..."
sudo -u ${APP_USER} -s /bin/bash -l -c "cd ${PAGI_PATH} && git reset --hard"
sudo -u ${APP_USER} -s /bin/bash -l -c "cd ${PAGI_PATH} && git pull"

sudo -u ${APP_USER} -s /bin/bash -l -c "cd ${APP_PATH} && git reset --hard"
sudo -u ${APP_USER} -s /bin/bash -l -c "cd ${APP_PATH} && git pull"

echo "Stopping service..."
# Stop the service cleanly
sudo systemctl stop ${SERVICE_NAME}

echo "Building application..."
sudo -u ${APP_USER} -s /bin/bash -l -c "cd ${PAGI_PATH} && mise install"
sudo -u ${APP_USER} -s /bin/bash -l -c "cd ${PAGI_PATH} && mise reshim"
sudo -u ${APP_USER} -s /bin/bash -l -c "cd ${PAGI_PATH} && perl Build.PL"
sudo -u ${APP_USER} -s /bin/bash -l -c "cd ${PAGI_PATH} && ./Build installdeps --cpan_client 'cpanm -n'"
sudo -u ${APP_USER} -s /bin/bash -l -c "cd ${PAGI_PATH} && ./Build manifest"
sudo -u ${APP_USER} -s /bin/bash -l -c "cd ${PAGI_PATH} && ./Build"

sudo -u ${APP_USER} -s /bin/bash -l -c "cd ${APP_PATH} && mise install"
sudo -u ${APP_USER} -s /bin/bash -l -c "cd ${APP_PATH} && mise reshim"
sudo -u ${APP_USER} -s /bin/bash -l -c "cd ${APP_PATH} && perl Build.PL"
sudo -u ${APP_USER} -s /bin/bash -l -c "cd ${APP_PATH} && ./Build installdeps --cpan_client 'cpanm -n'"
sudo -u ${APP_USER} -s /bin/bash -l -c "cd ${APP_PATH} && pnpm config set childConcurrency 2"
sudo -u ${APP_USER} -s /bin/bash -l -c "cd ${APP_PATH} && pip install -e scripts"
sudo -u ${APP_USER} -s /bin/bash -l -c "cd ${APP_PATH} && export NODE_OPTIONS=--max_old_space_size=1536; pnpm install"
sudo -u ${APP_USER} -s /bin/bash -l -c "cd ${APP_PATH} && rsync -a --delete share/images/ share/public/images/"
sudo -u ${APP_USER} -s /bin/bash -l -c "cd ${APP_PATH} && rsync -a --delete share/collections/data/images/generals/ share/public/images/generals/"
sudo -u ${APP_USER} -s /bin/bash -l -c "cd ${APP_PATH} && pnpm config set childConcurrency 1"
sudo -u ${APP_USER} -s /bin/bash -l -c "cd ${APP_PATH} && export NODE_OPTIONS=--max_old_space_size=1536; pnpm tsx ./scripts/build-ts.ts"
sudo -u ${APP_USER} -s /bin/bash -l -c "cd ${APP_PATH} && pnpm config set childConcurrency 2"
sudo -u ${APP_USER} -s /bin/bash -l -c "cd ${APP_PATH} && ./Build manifest"
sudo -u ${APP_USER} -s /bin/bash -l -c "cd ${APP_PATH} && perl ./scripts/update_git_meta.pl"
sudo -u ${APP_USER} -s /bin/bash -l -c "cd ${APP_PATH} && ./Build"
sudo -u ${APP_USER} -s /bin/bash -l -c "cd ${APP_PATH} && perl bin/extract_conflict_features.pl --mode=training --output=training_data.csv"
sudo -u ${APP_USER} -s /bin/bash -l -c "cd ${APP_PATH} && python bin/train_conflict_model.py   --training=training_data.csv   --model=conflict_model.pkl   --importance=feature_importance.csv"
sudo -u ${APP_USER} -s /bin/bash -l -c "cd ${APP_PATH} && perl bin/extract_conflict_features.pl --mode=predict --output=all_pairs.csv"
sudo -u ${APP_USER} -s /bin/bash -l -c "cd ${APP_PATH} && python bin/predict_conflicts.py   --model=conflict_model.pkl   --pairs=all_pairs.csv   --output=conflicts.json"


# remove the old state
# to prevent stale cache problems with new data
sudo -u ${APP_USER} rm -rf /opt/prefix/app/var

# workers now launched from the main package
#echo "Restarting workers"
#sudo systemctl restart "${SERVICE_NAME}-worker"
#sudo systemctl status "${SERVICE_NAME}-worker"

# Start the new version
sudo systemctl start ${SERVICE_NAME}

echo "Checking service status..."
sudo systemctl status ${SERVICE_NAME}

echo "Deployment complete!"
