#! /bin/bash -x
set -e

curl https://mise.run | sh

eval "$(/opt/mojo/.local/bin/mise activate bash)"

mise reshim

#diagnostic, not actually part of the install
mise doctor

echo 'eval "$(/opt/mojo/.local/bin/mise activate bash)"' >> ~/.bash_profile
echo 'export PATH="/opt/mojo/.local/bin/:$HOME/bin:$PATH"' >> /opt/mojo/.bash_profile

export PATH="/opt/mojo/.local/bin/:$HOME/bin:$PATH"

git clone -b streaming https://github.com/lschierer/Game-EvonyTKR.git /opt/mojo/app

cd /opt/mojo/app

mise trust
mise install
mise reshim

cd /opt/mojo/app

which cpanm

cpanm -n utf8::all Module::Build
cpanm -n IO::Socket::SSL

perl Build.PL
./Build installdeps --cpan_client 'cpanm -n'

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
pnpm config set childConcurrency 1
export NODE_OPTIONS=--max_old_space_size=2560; pnpm tsx ./scripts/build-ts.ts

./Build manifest
perl ./scripts/update_git_meta.pl
./Build

pnpm config set childConcurrency 2
echo 'bootstrap complete'
