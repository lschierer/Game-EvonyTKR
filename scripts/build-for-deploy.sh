#!/bin/bash
set -e

# Install Python package for scripts
pip install -e scripts

# Configure pnpm for limited memory environment
pnpm config set childConcurrency 2
export NODE_OPTIONS=--max_old_space_size=1536
pnpm install

# Sync image assets to public directory
rsync -a --delete share/images/ share/public/images/
rsync -a --delete share/collections/data/images/generals/ share/public/images/generals/

# Build CSS
mkdir -p share/public/css
mkdir -p ./share/tmp/css/
pnpm config set childConcurrency 1
export NODE_OPTIONS=--max_old_space_size=2560
pnpm tsx ./scripts/build-css.ts ./share/public/css/

# Build TypeScript
mkdir -p share/public/js
mkdir -p share/public/types
export NODE_OPTIONS=--max_old_space_size=2560
pnpm tsx ./scripts/build-ts.ts

# Reset pnpm concurrency
pnpm config set childConcurrency 2

# Build Perl module
# File::Find::Rule isn't part of a standard perl install, 
# make sure it is available before trying to run Build.PL
cpanm -n File::Find::Rule
perl Build.PL
./Build installdeps --cpan_client 'cpanm -nq --with-recommends'
./Build manifest

#perl ./scripts/update_git_meta.pl
./Build

# Train conflict detection model
perl bin/extract_conflict_features.pl --mode=training --output=training_data.csv
python bin/train_conflict_model.py --training=training_data.csv --model=conflict_model.pkl --importance=feature_importance.csv
perl bin/extract_conflict_features.pl --mode=predict --output=all_pairs.csv
python bin/predict_conflicts.py --model=conflict_model.pkl --pairs=all_pairs.csv --output=conflicts.json

