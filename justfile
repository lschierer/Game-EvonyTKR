find-perl-deps:
    ./scripts/finddeps.sh

brewpostgres:
    LC_ALL="en_US.UTF-8" /opt/homebrew/opt/postgresql@14/bin/postgres -D /opt/homebrew/var/postgresql@14

tidy:
    find lib -name '*.pm' -exec perltidy -b -pro=.perltidyrc {} \;
    find t -name '*.t' -exec perltidy -b -pro=.perltidyrc {} \;
    perltidy -b -pro=.perltidyrc Build.PL
    perltidy -b -pro=.perltidyrc scripts/create-general bin/game-evonytkr scripts/update_git_meta.pl
    find . -name '*.bak' -delete

prepare:
    mise install
    perl Build.PL

npmdeps:
    pnpm install

#find node_modules/evonytkrtips-data/share/ -name '*.csv' -exec sh -c 'iconv -f macroman -t utf-8 "$1" > "$(basename "$1")"' _ {} \;

deps: prepare npmdeps
    ./Build installdeps --cpan_client 'cpanm -n'
    pip install -e scripts
    #./scripts/AIBoost_install.sh
    perl ./scripts/update_git_meta.pl

[working-directory('share')]
images:
    rsync -a --delete images/ public/images/
    rsync -a --delete collections/data/images/generals/ public/images/generals/

# final trusted build of css after
css: npmdeps images
    rm -rf share/public/css
    mkdir -p share/public/css
    pnpm build:css

ts: npmdeps css
    rm -rf share/public/js
    mkdir -p share/public/js
    rm -rf share/public/types
    mkdir -p share/public/types
    pnpm build:ts

build: prepare deps css images ts
    ./Build manifest
    ./Build

dev: deps css images build
    truncate -s 0 "${HOME}/var/log/Perl/dist/Game-EvonyTKR/system.log"
    truncate -s 0 "${HOME}/var/log/Perl/dist/WebFramework/system.log"
    truncate -s 0 "${HOME}/var/log/Perl/dist/WebFramework/access.log"
    ./bin/server2.pl --mode development

quickdev:
    truncate -s 0 "${HOME}/var/log/Perl/dist/Game-EvonyTKR/system.log"
    truncate -s 0 "${HOME}/var/log/Perl/dist/WebFramework/system.log"
    truncate -s 0 "${HOME}/var/log/Perl/dist/WebFramework/access.log"
    ./scripts/dev.sh

deploy-dev: build
    pnpm cdk --profile personal deploy --context env=dev evonytkrtips-dev-stack2

deploy-prod: build
    pnpm cdk --profile personal deploy --context env=prod

[working-directory('share/infrastructure')]
dev-image: build
    ./bin/build-image.sh -d

mlModel:
    perl bin/extract_conflict_features.pl --mode=training --output=training_data.csv
    python bin/train_conflict_model.py   --training=training_data.csv   --model=conflict_model.pkl   --importance=feature_importance.csv
    perl bin/extract_conflict_features.pl --mode=predict --output=all_pairs.csv
    python bin/predict_conflicts.py   --model=conflict_model.pkl   --pairs=all_pairs.csv   --output=conflicts.json
