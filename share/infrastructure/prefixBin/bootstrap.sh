#! /bin/bash -x
set -e

APP_HOME="/opt/prefix"
APP_PATH="${APP_HOME}/app"
PAGI_PATH="${APP_HOME}/PAGI-WebServer"
export PATH="/opt/prefix/.local/bin/:$HOME/bin:$PATH"


# Helper function to retry commands with exponential backoff
retry_with_backoff() {
  local max_attempts=5
  local timeout=1
  local attempt=1
  local exitCode=0

  while (( attempt <= max_attempts ))
  do
    if "$@"
    then
      return 0
    else
      exitCode=$?
    fi

    echo "Command failed (attempt $attempt/$max_attempts). Retrying in $timeout seconds..."
    sleep $timeout
    timeout=$(( timeout * 2 ))
    attempt=$(( attempt + 1 ))
  done

  echo "Command failed after $max_attempts attempts: $*"
  return $exitCode
}

# Install mise with retry
retry_with_backoff curl -fsSL https://mise.run | sh

# Add mise binary to PATH
export PATH="/opt/prefix/.local/bin:$HOME/bin:$PATH"

# Clone repositories with retry (need repos before mise install can read .mise.toml)
retry_with_backoff git clone -b main https://github.com/lschierer/PAGI-WebServer.git /opt/prefix/PAGI-WebServer
retry_with_backoff git clone -b PAGI https://github.com/lschierer/Game-EvonyTKR.git /opt/prefix/app

# Build PAGI-WebServer first
cd $PAGI_PATH
mise trust
mise install
mise reshim

# Add mise shims to PATH (more reliable than mise activate in scripts)
export PATH="/opt/prefix/.local/share/mise/shims:$PATH"

# Set up bash_profile for future interactive sessions
cat > ~/.bash_profile << 'EOF'
export PATH="/opt/prefix/.local/share/mise/shims:/opt/prefix/.local/bin:$HOME/bin:$PATH"
eval "$(/opt/prefix/.local/bin/mise activate bash)"
EOF

# Install cpanm (not included with mise's perl by default)
curl -L https://cpanmin.us | perl - App::cpanminus
mise reshim
cpanm --self-upgrade -q

# Pre-install HTML::Tree family to avoid circular dependency issues
cpanm -nq HTML::Tagset HTML::Parser HTML::Tree

cpanm Module::Build utf8::all
perl Build.PL
./Build installdeps --cpan_client 'cpanm -nq --with-recommends'
./Build manifest
./Build

# Build Game-EvonyTKR
cd $APP_PATH
mise trust
mise install
mise reshim

pip install -e scripts

pnpm config set childConcurrency 2
export NODE_OPTIONS=--max_old_space_size=1536; pnpm install

rsync -a --delete share/images/ share/public/images/
rsync -a --delete share/collections/data/images/generals/ share/public/images/generals/

mkdir -p share/public/css
mkdir -p ./share/tmp/css/
pnpm config set childConcurrency 1
export NODE_OPTIONS=--max_old_space_size=2560; pnpm tsx ./scripts/build-css.ts ./share/public/css/

mkdir -p share/public/js
mkdir -p share/public/types

export NODE_OPTIONS=--max_old_space_size=2560; pnpm tsx ./scripts/build-ts.ts

pip install -e scripts

pnpm config set childConcurrency 2

perl Build.PL
./Build installdeps --cpan_client 'cpanm -nq --with-recommends'
./Build manifest
perl ./scripts/update_git_meta.pl
./Build

perl bin/extract_conflict_features.pl --mode=training --output=training_data.csv
python bin/train_conflict_model.py   --training=training_data.csv   --model=conflict_model.pkl   --importance=feature_importance.csv
perl bin/extract_conflict_features.pl --mode=predict --output=all_pairs.csv
python bin/predict_conflicts.py   --model=conflict_model.pkl   --pairs=all_pairs.csv   --output=conflicts.json


# Start the application service now that build is complete
sudo systemctl start evonytkr

echo 'bootstrap complete - SUCCESS' | tee -a ${HOME}/var/log/bootstrap.log
exit 0
