#!/bin/bash
set -e
set -u
set -o pipefail

# Initialize an empty OpenPedCan-api database with schemas.
#
# This file can be run in any working directory.
#
# Call sequence:
# - build_db process
#   - docker db/build_tools/build_db.Dockerfile image runs
#     db/build_tools/build_db_docker_cmd.sh.
#   - db/build_tools/build_db_docker_cmd.sh runs this file.

# Adapted from https://hub.docker.com/_/postgres

# Environment variables POSTGRES_USER, DB_READ_WRITE_USERNAME, DB_USERNAME, and
# DB_NAME must all be different.
if [[ "${POSTGRES_USER}" == "${DB_READ_WRITE_USERNAME}" ]]; then
    echo "Error: POSTGRES_USER=${POSTGRES_USER} must be different from DB_READ_WRITE_USERNAME=${DB_READ_WRITE_USERNAME}." 1>&2
    exit 1
fi

if [[ "${POSTGRES_USER}" == "${DB_USERNAME}" ]]; then
    echo "Error: POSTGRES_USER=${POSTGRES_USER} must be different from DB_USERNAME=${DB_USERNAME}." 1>&2
    exit 1
fi

if [[ "${DB_READ_WRITE_USERNAME}" == "${DB_USERNAME}" ]]; then
    echo "Error: DB_READ_WRITE_USERNAME=${DB_READ_WRITE_USERNAME} must be different from DB_USERNAME=${DB_USERNAME}." 1>&2
    exit 1
fi

if [[ "${DB_NAME}" == "${POSTGRES_USER}" ]]; then
    echo "Error: DB_NAME=${DB_NAME} must be different from POSTGRES_USER=${POSTGRES_USER}." 1>&2
    exit 1
fi

if [[ "${DB_NAME}" == "${DB_READ_WRITE_USERNAME}" ]]; then
    echo "Error: DB_NAME=${DB_NAME} must be different from DB_READ_WRITE_USERNAME=${DB_READ_WRITE_USERNAME}." 1>&2
    exit 1
fi

if [[ "${DB_NAME}" == "${DB_USERNAME}" ]]; then
    echo "Error: DB_NAME=${DB_NAME} must be different from DB_USERNAME=${DB_USERNAME}." 1>&2
    exit 1
fi


# << starts a *here document*. Ref: https://tldp.org/LDP/abs/html/here-docs.html
psql -v ON_ERROR_STOP=1 --username "${POSTGRES_USER}" --dbname "${POSTGRES_DB}" <<EOSQL
CREATE ROLE ${DB_READ_WRITE_USERNAME} WITH LOGIN PASSWORD '${DB_READ_WRITE_PASSWORD}';
-- Create user default database.
CREATE DATABASE ${DB_READ_WRITE_USERNAME} WITH OWNER ${DB_READ_WRITE_USERNAME} CONNECTION LIMIT 100;

CREATE ROLE ${DB_USERNAME} WITH LOGIN PASSWORD '${DB_PASSWORD}';
-- Create user default database.
CREATE DATABASE ${DB_USERNAME} WITH OWNER ${DB_USERNAME} CONNECTION LIMIT 100;

-- Create OpenPedCan-api database
CREATE DATABASE ${DB_NAME} WITH OWNER ${DB_READ_WRITE_USERNAME} CONNECTION LIMIT 500;
EOSQL

# Create OpenPedCan-api database
#
# Ref: https://stackoverflow.com/a/762649/4638182
psql -v ON_ERROR_STOP=1 --username "${DB_READ_WRITE_USERNAME}" --dbname "${DB_NAME}" <<EOSQL
-- Create TPM schema.
CREATE SCHEMA ${BULK_EXP_SCHEMA};

-- Give DB_USERNAME readonly access to the schema and all tables in it.
GRANT CONNECT ON DATABASE ${DB_NAME} TO ${DB_USERNAME};
GRANT USAGE ON SCHEMA ${BULK_EXP_SCHEMA} TO ${DB_USERNAME};
GRANT SELECT ON ALL TABLES IN SCHEMA ${BULK_EXP_SCHEMA} TO ${DB_USERNAME};

-- Create CNV schema
CREATE SCHEMA ${CNV_SCHEMA};

-- Give DB_USERNAME readonly access to the schema and all tables in it.
GRANT CONNECT ON DATABASE ${DB_NAME} TO ${DB_USERNAME};
GRANT SELECT ON ALL TABLES IN SCHEMA ${CNV_SCHEMA} TO ${DB_USERNAME};
ALTER DEFAULT PRIVILEGES IN SCHEMA ${CNV_SCHEMA} GRANT SELECT ON TABLES TO ${DB_USERNAME};

-- "Assign permissions to read all newly tables created in the future".
ALTER DEFAULT PRIVILEGES IN SCHEMA ${BULK_EXP_SCHEMA} GRANT SELECT ON TABLES TO ${DB_USERNAME};
EOSQL
