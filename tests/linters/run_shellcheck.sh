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

# Run shellcheck on .sh files
#
# https://github.com/koalaman/shellcheck
shellcheck ./db/build_db.sh \
  ./db/build_tools/build_db_docker_cmd.sh \
  ./db/build_tools/build_db_docker_entrypoint.sh \
  ./db/init_db.sh \
  ./db/init_db_pwfile.sh \
  ./db/load_db.sh \
  ./tests/git_diff_image.sh \
  ./tests/run_linters.sh \
  ./tests/run_tests.sh
