#!/bin/bash
set -e
set -u
set -o pipefail

# Initialize postgres password file for password authentication.
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
