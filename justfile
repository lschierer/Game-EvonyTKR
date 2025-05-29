find-perl-deps:
  ./scripts/finddeps.sh


prepare:
  perl Build.PL

deps: prepare
  ./Build installdeps --cpan_client 'cpanm -n'

build: deps
  ./Build manifest
  ./Build

dev: deps
  morbo --watch ./share --watch ./lib ./scripts/game-evonytkr
