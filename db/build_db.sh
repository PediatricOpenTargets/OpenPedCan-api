#!/bin/bash
set -e
set -u
set -o pipefail

# This script should always run as if it were being called from
# the directory it lives in.
#
# Adapted from https://stackoverflow.com/a/3355423/4638182
cd "$(dirname "$0")" || exit

cd ..

echo "Download OpenPedCan-analysis data release..."
# The commit ID to checkout to build data model.
#
# 61e23154a34e1d8b3fc1c50a67dd8f79c2067776 points to v8 release with updated
# OpenPedCan-analysis/analyses/long-format-table-utils/annotator.
OPEN_PED_CAN_ANALYSIS_COMMIT="61e23154a34e1d8b3fc1c50a67dd8f79c2067776"

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
cd ..

printf "\n\nBuild data model using docker...\n"

docker build --no-cache -f db/build_db.Dockerfile -t open-ped-can-api-build-db .

# Copy db files from image to host.
#
# Adapted from https://stackoverflow.com/a/31316636/4638182
docker_container_id=$(docker create open-ped-can-api-build-db)
docker cp "${docker_container_id}:/home/OpenPedCan-api/db/tpm_data_lists.rds" ./db/
docker cp "${docker_container_id}:/home/OpenPedCan-api/db/sha256sum.txt" ./db/
docker rm -v "${docker_container_id}"

# check sha256sum
cd db

printf "\n\nCheck data model sha256sum...\n"

# Try different sha sum commands.
#
# Adapted from https://stackoverflow.com/a/26759734/4638182
if [[ -x $(command -v sha256sum) ]]; then
  sha256sum -c sha256sum.txt
elif [[ -x $(command -v shasum) ]]; then
  shasum -a 256 --strict -c sha256sum.txt
else
  echo "sha256sum or shasum command not found. Please install either one and rerun." 1>&2
  exit 1
fi

printf '\n\nDone running %s.\n' "$0"
