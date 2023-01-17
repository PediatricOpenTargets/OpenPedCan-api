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
# 1eedaf5825fc999415ff5fd712f98a6f17718b92 points to v11 release.
OPEN_PED_CAN_ANALYSIS_COMMIT="1eedaf5825fc999415ff5fd712f98a6f17718b92"

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

# Add read access to OpenPedCan-analysis data.
#
# Files under ./data/ are not tracked.
chmod -R "a+r" ./data/
# ./data/v* are directories that have files.
chmod "a+x" ./data/v*/

# Download differential expression DESeq results.
cd ../db/build_outputs

curl "https://s3.amazonaws.com/d3b-openaccess-us-east-1-prd-pbta/open-targets/api/test/differential_gene_expression_v10/deseq_v10_all.rds" \
  -o deseq_v10_all.rds

sha256sum -c diff_gene_exp_res_sha256sum.txt

# Build data model
#
# NOTE: if more files in OpenPedCan-analysis are used, exclude them in the
# .dockerignore file.
#
# Change workdir to git root dir.
cd ../..

printf "\n\nBuild data model using docker...\n"

# The relative path of host db/build_outputs/ and relative/absolute path of
# container /home/open-ped-can-api-db/db/build_outputs/ are used in various
# scripts. If these paths need to be changed, the complete code base needs to be
# searched for other necessary changes.
#
# Pass env vars for for the following build steps that are split into multiple
# scripts:
#
# - DB_HOME_DIR_PATH is the absolute path of the container db building home dir.
# - BUILD_OUTPUT_DIR_PATH is the absolute path of container build output dir,
#   which is bind mounted to host db/build_outputs/.
export DB_HOME_DIR_PATH="/home/open-ped-can-api-db"
export BUILD_OUTPUT_DIR_PATH="${DB_HOME_DIR_PATH}/db/build_outputs"

docker build --no-cache \
  --build-arg DB_HOME_DIR_PATH="${DB_HOME_DIR_PATH}" \
  --build-arg BUILD_OUTPUT_DIR_PATH="${BUILD_OUTPUT_DIR_PATH}" \
  -f db/build_tools/build_db.Dockerfile \
  -t open-ped-can-api-build-db .

# Bind mount db/build_outputs and OpenPedCan-analysis to avoid passing large
# docker build context to daemon.
docker run --rm -it \
  --env-file ../OpenPedCan-api-secrets/access_db.env \
  --env-file ../OpenPedCan-api-secrets/common_db.env \
  --env-file ../OpenPedCan-api-secrets/load_db.env \
  --env DOWN_SAMPLE_DB_GENES \
  -v "$(pwd)"/OpenPedCan-analysis/:"$DB_HOME_DIR_PATH"/OpenPedCan-analysis/ \
  -v "$(pwd)"/db/build_outputs/:"$BUILD_OUTPUT_DIR_PATH" \
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
