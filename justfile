find-perl-deps:
  ./scripts/finddeps.sh

tidy:
  find lib t -name '*.pm' -exec perltidy -b -pro=.perltidyrc {} \;
  perltidy -b -pro=.perltidyrc Build.PL
  perltidy -b -pro=.perltidyrc scripts/check_broken_links scripts/create-general scripts/game-evonytkr scripts/update_git_meta.pl
  find . -name '*.bak' -delete

prepare:
  mise install
  perl Build.PL

npmdeps:
  pnpm install
  rm -rf share/public/js
  rm -rf share/public/types
  mkdir -p share/public/js
  mkdir -p share/public/types
  pnpm build:css
  pnpm build:ts

#find node_modules/evonytkrtips-data/share/ -name '*.csv' -exec sh -c 'iconv -f macroman -t utf-8 "$1" > "$(basename "$1")"' _ {} \;


deps: prepare npmdeps
  ./Build installdeps --cpan_client 'cpanm -n'
  perl ./scripts/update_git_meta.pl

[working-directory: 'share']
images:
  rsync -a --delete images/ public/images/
  rsync -a --delete collections/data/images/generals/ public/images/generals/

css: npmdeps images
  rm -rf share/public/css
  mkdir -p share/public/css
  pnpm build:css


build: prepare deps css images
  ./Build manifest
  ./Build

dev: deps css images
  rm -f "${HOME}/var/log/Perl/dist/Game-Evony/*.log"
  morbo --watch ./share --watch ./lib ./bin/game-evonytkr

quickdev:
  echo "${HOME}/var/log/Perl/dist/Game-Evony/*.log"
  rm -f "${HOME}/var/log/Perl/dist/Game-Evony/*.log"
  morbo --watch ./share --watch ./lib ./bin/game-evonytkr

deploy-dev: build
watchexec --exts css,pm,ep,js -w lib/ -w bin/ -w share/templates/ -w share/public/ -w share/collections/data/ --restart morbo ./bin/game-evonytkr

deploy-prod: build
  pnpm cdk --profile personal deploy --context env=prod
