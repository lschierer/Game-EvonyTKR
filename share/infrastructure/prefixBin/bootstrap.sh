#! /bin/bash -x
set -e

APP_HOME="/opt/prefix"
APP_PATH="${APP_HOME}/app"
PAGI_PATH="${APP_HOME}/PAGI-WebServer"

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

# Set up PATH and mise activation
export PATH="/opt/prefix/.local/bin:$HOME/bin:$PATH"
eval "$(/opt/prefix/.local/bin/mise activate bash)"

# Add to bash_profile for future sessions
echo 'export PATH="/opt/prefix/.local/bin:$HOME/bin:$PATH"' >> ~/.bash_profile
echo 'eval "$(/opt/prefix/.local/bin/mise activate bash)"' >> ~/.bash_profile

# Clone repositories with retry
retry_with_backoff git clone -b main https://github.com/lschierer/PAGI-WebServer.git /opt/prefix/PAGI-WebServer
retry_with_backoff git clone -b PAGI https://github.com/lschierer/Game-EvonyTKR.git /opt/prefix/app

# Build PAGI-WebServer first
cd $PAGI_PATH
mise trust
mise install
mise reshim

perl Build.PL
./Build installdeps --cpan_client 'cpanm -n'
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

pnpm config set childConcurrency 2

perl Build.PL
./Build installdeps --cpan_client 'cpanm -n'
./Build manifest
perl ./scripts/update_git_meta.pl
./Build

echo 'bootstrap complete - SUCCESS'
exit 0
