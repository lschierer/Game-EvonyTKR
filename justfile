find-perl-deps:
  ./scripts/finddeps.sh

build:
  perl Build.PL
  ./Build manifest
  ./Build

dev:
  morbo --watch ./share --watch ./lib ./scripts/game-evonytkr
