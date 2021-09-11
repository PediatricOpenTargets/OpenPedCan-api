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

# Run hadolint on Dockerfiles
#
# https://github.com/hadolint/hadolint
docker run --rm -i hadolint/hadolint < ./db/build_tools/build_db.Dockerfile
docker run --rm -i hadolint/hadolint < ./db/db.Dockerfile
docker run --rm -i hadolint/hadolint < ./Dockerfile
