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
DB_CLUSTER_NAME=open_ped_can

# Adapted from postgres docker image
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
echo "*:*:*:${DB_USERNAME}:${DB_PASSWORD}" >> ~/.pgpass
echo "*:*:*:${POSTGRES_USER}:${POSTGRES_PASSWORD}" >> ~/.pgpass
echo "*:*:*:${DB_READ_WRITE_USERNAME}:${DB_READ_WRITE_PASSWORD}" >> ~/.pgpass

pg_ctlcluster 11 "$DB_CLUSTER_NAME" start

../init_user_db.sh

Rscript --vanilla tpm_data_lists.R

# Overwrite env vars in env file
#
# Use database read-write user
export DB_USERNAME="$DB_READ_WRITE_USERNAME"
export DB_PASSWORD="$DB_READ_WRITE_PASSWORD"

# Use localhost
export DB_HOST="localhost"

Rscript --vanilla build_db.R

# TODO: dump database. add checksum. remove intermediate files.
