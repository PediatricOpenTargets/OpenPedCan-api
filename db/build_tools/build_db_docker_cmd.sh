#!/bin/bash
set -e
set -u
set -o pipefail

# This script should always run as if it were being called from
# the directory it lives in.
#
# Adapted from https://stackoverflow.com/a/3355423/4638182
cd "$(dirname "$0")" || exit

# Create a database cluster
export DB_CLUSTER_NAME=open_ped_can

printf "\n\nInitialize database...\n"

# Adapted from postgres docker image
# shellcheck disable=SC1004
eval 'pg_createcluster --port=5432 11 "$DB_CLUSTER_NAME" -- \
  --username="$POSTGRES_USER" --pwfile=<(echo "$POSTGRES_PASSWORD") \
  $POSTGRES_INITDB_ARGS'


echo "host all all all $POSTGRES_HOST_AUTH_METHOD" \
  >> "/etc/postgresql/11/${DB_CLUSTER_NAME}/pg_hba.conf"

# postgres password file for password authentication
touch ~/.pgpass
chmod 600 ~/.pgpass
# .pgpass format: hostname:port:database:username:password
#
# "If an entry needs to contain : or \, escape this character with \."
#
# Set username and password environment variables without special characters to
# simplify local development.
#
# https://www.postgresql.org/docs/current/libpq-pgpass.html
{
  echo "*:*:*:${DB_USERNAME}:${DB_PASSWORD}"
  echo "*:*:*:${POSTGRES_USER}:${POSTGRES_PASSWORD}"
  echo "*:*:*:${DB_READ_WRITE_USERNAME}:${DB_READ_WRITE_PASSWORD}"
} >> ~/.pgpass

pg_ctlcluster 11 "$DB_CLUSTER_NAME" start

../init_user_db.sh

printf "\n\nWrite R objects into database compatible csv file(s)...\n"

Rscript --vanilla tpm_data_lists.R

# Overwrite env vars in env file
#
# Use database read-write user
export DB_USERNAME="$DB_READ_WRITE_USERNAME"
export DB_PASSWORD="$DB_READ_WRITE_PASSWORD"

# Use localhost
export DB_HOST="localhost"

# build_db.R take env var DOWN_SAMPLE_DB_GENES. See build_db.R for details.
#
# Output csv file paths have the following conventions:
#
# - filename has the format of ${schema_name}_${table_name}.csv
# - output directory is ../build_outputs
#
# Use .csv rather than .csv.gz to speed up database COPY command.
#
# build_db.R does not take explicit options of output paths, by design, because
# build_db.R does not need to be run anywhere else.
#
# This script hangs when readr::write_csv tries to print progress, if run with
# --vanilla, so disable progress printing.
Rscript --vanilla build_db.R

printf "\n\nLoad the csv file(s) into database...\n"

psql -v ON_ERROR_STOP=1 --username "$POSTGRES_USER" --dbname "$DB_NAME" <<EOSQL
COPY ${BULK_EXP_SCHEMA}.${BULK_EXP_TPM_HISTOLOGY_TBL}
FROM '${BUILD_OUTPUT_DIR_PATH}/${BULK_EXP_SCHEMA}_${BULK_EXP_TPM_HISTOLOGY_TBL}.csv'
WITH (FORMAT csv, HEADER);
EOSQL

cd "$BUILD_OUTPUT_DIR_PATH"

db_dump_out_path="postgres_db_${DB_NAME}_schema_${BULK_EXP_SCHEMA}.sql.gz"

printf "\n\nDump database schema(s) and table(s)...\n"

pg_dump --clean --if-exists --no-owner --no-privileges \
  --schema="$BULK_EXP_SCHEMA" --dbname="$DB_NAME" \
  --host="$DB_HOST" --port="$DB_PORT" --username="$DB_USERNAME" \
  | gzip --no-name -c > "$db_dump_out_path"

# Append SQL line(s) to create index(es).
#
# "Multiple  compressed  files can be concatenated." -- gzip manual page
echo "CREATE INDEX ensg_id_idx ON ${BULK_EXP_SCHEMA}.${BULK_EXP_TPM_HISTOLOGY_TBL} (\"Gene_Ensembl_ID\");" \
  | gzip --no-name -c >> "$db_dump_out_path"

# To restore from dump, run:
# gunzip -c "$db_dump_out_path" | psql -v ON_ERROR_STOP=1 \
#   --dbname="$DB_NAME" --username="$DB_USERNAME" \
#   --host="$DB_HOST" --port="$DB_PORT"

printf "\n\nChecksum...\n"

sha256sum "$db_dump_out_path" > sha256sum.txt

printf "\n\nDone building database locally.\n"
