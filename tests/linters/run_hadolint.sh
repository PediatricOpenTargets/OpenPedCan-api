#!/bin/bash
set -e
set -u
set -o pipefail

# This script should always run as if it were being called from
# the directory it lives in.
#
# Adapted from https://stackoverflow.com/a/3355423/4638182
cd "$(dirname "$0")" || exit

# Change working dir to git root dir
cd ../../

# Run hadolint on Dockerfiles
#
# https://github.com/hadolint/hadolint

for docker_file in "./db/build_tools/build_db.Dockerfile" \
                   "./db/db.Dockerfile" \
                   "./Dockerfile"; do
  echo "$docker_file"
  docker run --rm -i hadolint/hadolint < "$docker_file"
done
