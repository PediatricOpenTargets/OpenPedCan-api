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
shellcheck ./db/*/*.sh ./db/*.sh ./tests/*.sh
