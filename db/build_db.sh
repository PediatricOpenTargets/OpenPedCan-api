#!/bin/bash
#
# This is the main script for building OpenPedCan-api database. The build_db
# process has the following steps:
#
# 1. Run R scripts to process OpenPedCan-analysis release data and output rds
#    objects. This step uses > 20GB memory, because R garbage collection does
#    not work well.
# 2. Run another R script to load the rds files output by step 1 and build a
#    postgres database.
# 3. Run pg_dump on the database built in step 2.
#
# This script takes the following env vars:
#
# - DOWN_SAMPLE_DB_GENES:
#   - unset or 0: do not down sample genes for database.
#   - 1: down sample genes for database
#   - other: error
set -e
set -u
set -o pipefail

# This script should always run as if it were being called from
# the directory it lives in.
#
# Adapted from https://stackoverflow.com/a/3355423/4638182
cd "$(dirname "$0")" || exit

# Change workdir to git root dir.
cd ..

echo "Download OpenPedCan-analysis data release..."
# The commit ID to checkout to build data model.
#
# 96132ae1e7485d9ab129380898fac5e255ccb36f points to v9 release with updated
# OpenPedCan-analysis/analyses/long-format-table-utils/annotator.
OPEN_PED_CAN_ANALYSIS_COMMIT="96132ae1e7485d9ab129380898fac5e255ccb36f"

# If submodule repo url changes, e.g. rename, this will update the URL according
# to the one in .gitmodules.
git submodule sync --recursive
# Initialize submodule if not initialized. A newly cloned repo will not have
# submodules initialized.
#
# Note that this retrievs changes from the main remote repo, rather than
# submodule remote repo.
git submodule update --init --recursive

# Download data
cd OpenPedCan-analysis

git checkout -q "${OPEN_PED_CAN_ANALYSIS_COMMIT}"
./download-data.sh
git switch -q -

# Build data model
#
# NOTE: if more files in OpenPedCan-analysis are used, exclude them in the
# .dockerignore file.
#
# Change workdir to git root dir.
cd ..

printf "\n\nBuild data model using docker...\n"

docker build --no-cache -f db/build_tools/build_db.Dockerfile \
  -t open-ped-can-api-build-db .

docker run --rm -it \
  --env-file ../OpenPedCan-api-secrets/access_db.env \
  --env-file ../OpenPedCan-api-secrets/common_db.env \
  --env-file ../OpenPedCan-api-secrets/load_db.env \
  --env DOWN_SAMPLE_DB_GENES \
  -v "$(pwd)"/db/build_outputs/:/home/open-ped-can-api-db/db/build_outputs \
  open-ped-can-api-build-db

cd db/build_outputs

printf "\n\nCheck data model sha256sum...\n"

# Try different sha sum commands.
#
# Adapted from https://stackoverflow.com/a/26759734/4638182
if [[ -x $(command -v sha256sum) ]]; then
  sha256sum -c sha256sum.txt
elif [[ -x $(command -v shasum) ]]; then
  shasum -a 256 --strict -c sha256sum.txt
else
  echo "Error: sha256sum or shasum command not found. Please install either one and rerun." 1>&2
  exit 1
fi

printf '\n\nDone running %s.\n' "$0"
