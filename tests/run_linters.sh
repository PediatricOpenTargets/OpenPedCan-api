#!/bin/bash
set -e
set -u
set -o pipefail

# This script should always run as if it were being called from
# the directory it lives in.
cd "$(dirname "$0")" || exit

# Dockerfile linter
printf "\n\nlint Dockerfiles...\n"
./linters/run_hadolint.sh

# .R file linter
printf "\n\nlint .R files...\n"
./linters/run_r_lintr.sh

# .sh file linter
printf "\n\nlint .sh files...\n"
./linters/run_shellcheck.sh

printf "\n\nDone.\n"
