#!/bin/bash
#
# This script is called by the web application Dockerfile, which is in the
# root directory of this repository, to load pre-built database.
set -e
set -u
set -o pipefail

# Use DB_LOCATION to determine where to get the database.
#
# - aws_s3: download database from aws s3 bucket. Default.
# - local: use local database in ./db dir COPY. If database is not built
#   locally, report an error.
# - other: raise an error.
DB_LOCATION=${DB_LOCATION:-aws_s3}

# Path env vars from db.Dockerfile.
#
# - BUILD_OUTPUT_DIR_PATH
# - DB_LOAD_TOOLS_DIR_PATH

printf '\n\nInitialize empty database and schema(s)...\n'

"$DB_LOAD_TOOLS_DIR_PATH"/init_db_pwfile.sh

# Unset default PGPASSWORD password. Otherwise, psql with other usernames will
# always use PGPASSWORD password.
unset PGPASSWORD

"$DB_LOAD_TOOLS_DIR_PATH"/init_db.sh

printf '\n\nLoad database from %s...\n' "$DB_LOCATION"

if [[ "${DB_LOCATION}" == "local" ]]; then
  cd "${BUILD_OUTPUT_DIR_PATH}"

  if [[ ! -f "sha256sum.txt" ]]; then
    echo "Error: local file ${PWD}/sha256sum.txt does not exist." 1>&2
    exit 1
  fi
else
  if [[ "${DB_LOCATION}" != "aws_s3" ]]; then
    echo "Invalid DB_LOCATION ${DB_LOCATION}" 1>&2
    exit 1
  fi
fi

db_dump_fn="open_ped_can_db_postgres_pg_dump.sql.gz"

API_DB_BASE_URL="https://s3.amazonaws.com/kf-openaccess-us-east-1-prd-pbta/open-targets/api/db-dev"

if [[ "${DB_LOCATION}" == "aws_s3" ]]; then \
  mkdir -p "$DB_LOAD_TOOLS_DIR_PATH"
  cd "$DB_LOAD_TOOLS_DIR_PATH"
  curl "${API_DB_BASE_URL}/sha256sum.txt" -o "sha256sum.txt"
  curl "${API_DB_BASE_URL}/${db_dump_fn}" -o "${db_dump_fn}"
fi

printf "\n\nCheck database sha256sum...\n"

# Try different sha sum commands.
#
# Adapted from https://stackoverflow.com/a/26759734/4638182
if [[ -x $(command -v sha256sum) ]]; then
  sha256sum -c --strict sha256sum.txt
elif [[ -x $(command -v shasum) ]]; then
  shasum -a 256 --strict -c sha256sum.txt
else
  echo "sha256sum or shasum command not found. Please install either one and rerun." 1>&2
  exit 1
fi

# Load db dump
printf "\n\nLoad database dump sql...\n"

gunzip -c "$db_dump_fn" | psql -v ON_ERROR_STOP=1 \
  --dbname="$DB_NAME" --username="$DB_READ_WRITE_USERNAME"

# Clean up files downloaded from aws s3.
if [[ "${DB_LOCATION}" == "aws_s3" ]]; then \
  printf "\n\nClean up files downloaded from aws s3...\n"
  rm "sha256sum.txt"
  rm "${db_dump_fn}"
fi

# Reset read-only previleges, because the schema may be dropped by the dump sql.
printf "\n\nReset read-only previleges...\n"
psql -v ON_ERROR_STOP=1 --username "${DB_READ_WRITE_USERNAME}" --dbname "${DB_NAME}" <<EOSQL
-- Give DB_USERNAME readonly access to the shema and all tables in it.
GRANT CONNECT ON DATABASE ${DB_NAME} TO ${DB_USERNAME};
GRANT USAGE ON SCHEMA ${BULK_EXP_SCHEMA} TO ${DB_USERNAME};
GRANT SELECT ON ALL TABLES IN SCHEMA ${BULK_EXP_SCHEMA} TO ${DB_USERNAME};

-- "Assign permissions to read all newly tables created in the future".
ALTER DEFAULT PRIVILEGES IN SCHEMA ${BULK_EXP_SCHEMA} GRANT SELECT ON TABLES TO ${DB_USERNAME};
EOSQL

# TODO: print a message of the number of total genes and samples.

printf '\n\nDone running %s.\n' "$0"
