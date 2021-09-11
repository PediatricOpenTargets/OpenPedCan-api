#!/bin/bash
set -e
set -u
set -o pipefail

if [[ ! -d "$BUILD_OUTPUT_DIR_PATH" ]]; then
  echo "Error: container directory $BUILD_OUTPUT_DIR_PATH does not exist." 1>&2
  exit 1
fi

echo "Change directory $BUILD_OUTPUT_DIR_PATH owner to postgres:postgres."

prev_owner_uid="$(stat -c '%u' "$BUILD_OUTPUT_DIR_PATH")"
prev_owner_gid="$(stat -c '%g' "$BUILD_OUTPUT_DIR_PATH")"

chown -R postgres:postgres "$BUILD_OUTPUT_DIR_PATH"

# Run docker command(s) as postgres user
gosu postgres "$@" || true

# Change directory owner back to previous owner.
#
# chown requries root permission, even though the file is owned by postgres
# user.
chown -R "$prev_owner_uid":"$prev_owner_gid" "$BUILD_OUTPUT_DIR_PATH"

echo "Change directory $BUILD_OUTPUT_DIR_PATH owner back to original."
