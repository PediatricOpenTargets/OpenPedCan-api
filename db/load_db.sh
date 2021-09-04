#!/bin/bash
#
# This script is called by the web application Dockerfile, which is in the
# root directory of this repository, to load pre-built database.
set -e
set -u
set -o pipefail

# This script should always run as if it were being called from
# the directory it lives in.
#
# Adapted from https://stackoverflow.com/a/3355423/4638182
cd "$(dirname "$0")" || exit

# Use DB_LOCATION to determine where to get the database.
#
# - aws_s3: download database from aws s3 bucket.
# - local: use local database in ./db dir COPY. If database is not built
#   locally, report an error.
DB_LOCATION=${DB_LOCATION:-aws_s3}

printf '\n\nLoad database from %s...\n' "$DB_LOCATION"

if [[ "${DB_LOCATION}" == "local" ]]; then
  if [[ ! -f "sha256sum.txt" ]]; then
    echo "./db/sha256sum.txt does not exist." 1>&2
    exit 1
  fi
else
  if [[ "${DB_LOCATION}" != "aws_s3" ]]; then
    echo "Invalid DB_LOCATION ${DB_LOCATION}" 1>&2
    exit 1
  fi
fi

API_DB_BASE_URL="https://s3.amazonaws.com/kf-openaccess-us-east-1-prd-pbta/open-targets/api/dev"

if [[ "${DB_LOCATION}" == "aws_s3" ]]; then \
  curl "${API_DB_BASE_URL}/sha256sum.txt" -o sha256sum.txt
  curl "${API_DB_BASE_URL}/tpm_data_lists.rds" -o tpm_data_lists.rds
fi

printf "\n\nCheck database sha256sum...\n"

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
