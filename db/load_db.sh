#!/bin/bash
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

echo "Load database from ${DB_LOCATION}"

if [[ "${DB_LOCATION}" == "local" ]]; then
  if [[ -f "sha256sum.txt" ]]; then
    sha256sum -c --strict sha256sum.txt
  else
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
  sha256sum -c --strict sha256sum.txt
fi
