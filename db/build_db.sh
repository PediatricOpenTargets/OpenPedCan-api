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

docker build --no-cache -f db/build_tools/build_db.Dockerfile -t open-ped-can-api-build-db .

# Copy db files from image to host.
#
# Adapted from https://stackoverflow.com/a/31316636/4638182
cd db/build_outputs

docker_container_id=$(docker create open-ped-can-api-build-db)
docker cp "${docker_container_id}:/home/open-ped-can-api-db/db/build_outputs/tpm_data_lists.rds" .
docker cp "${docker_container_id}:/home/open-ped-can-api-db/db/build_outputs/sha256sum.txt" .
docker rm -v "${docker_container_id}" > /dev/null

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
