#!/bin/bash
set -e
set -u
set -o pipefail

# This script should always run as if it were being called from
# the directory it lives in.
cd "$(dirname "$0")" || exit

# Set LOCAL_API_HOST_PORT to 8082 if not set.
#
# Adapted from
#
# - https://google.github.io/styleguide/shellguide.html
# - https://stackoverflow.com/a/13864829/4638182
if [[ -z "${LOCAL_API_HOST_PORT+x}" ]]; then
  LOCAL_API_HOST_PORT="8082"
fi

if [[ -z "${API_HOST+x}" ]]; then
  API_HOST="local"
fi

if [[ "${API_HOST}" == "local" ]]; then
  export BASE_URL="http://localhost:${LOCAL_API_HOST_PORT}"
elif [[ "${API_HOST}" == "qa" ]]; then
  export BASE_URL="https://openpedcan-api-qa.d3b.io"
elif [[ "${API_HOST}" == "dev" ]]; then
  export BASE_URL="https://openpedcan-api-dev.d3b.io"
elif [[ "${API_HOST}" == "prd" ]]; then
  export BASE_URL="https://openpedcan-api.d3b.io"
else
  echo "Invalid API_HOST=${API_HOST}. Please choose from local, qa, and dev and rerun." 1>&2
  exit 1
fi

echo "API base URL: ${BASE_URL}"

Rscript --vanilla -e 'testthat::test_dir("r_test_scripts")'

echo 'Done running run_tests.sh'
