find-perl-deps:
  ./scripts/finddeps.sh


prepare:
  mise install
  perl Build.PL

[working-directory: 'share']
npmdeps:
  pnpm install
  rm -rf public
  mkdir -p public/assets
  mkdir -p public/css
  mkdir -p public/images
  mkdir -p public/js
  mkdir -p public/types
  pnpm build:ts
  rsync -a node_modules/evonytkrtips-data/data/ collections/ --delete
  touch collections/.gitkeep
  mkdir 'collections/general conflict groups'
  touch 'collections/general conflict groups/.gitkeep'
  find node_modules/evonytkrtips-data/share/ -name '*.csv' -exec sh -c 'iconv -f macroman -t utf-8 "$1" > "$(basename "$1")"' _ {} \;


deps: prepare npmdeps
  ./Build installdeps --cpan_client 'cpanm -n'


[working-directory: 'share']
css: npmdeps
  pnpm build:css

build: deps css
  ./Build manifest
  ./Build

dev: deps css
  rm -f "${HOME}/var/log/Perl/dist/Game-Evony/*.log"
  morbo --watch ./share --watch ./lib ./scripts/game-evonytkr
