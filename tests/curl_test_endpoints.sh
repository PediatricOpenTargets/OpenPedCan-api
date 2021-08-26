#!/bin/bash
set -e
set -u
set -o pipefail

# This script should always run as if it were being called from
# the directory it lives in.
#
# Adapted from https://stackoverflow.com/a/3355423/4638182
cd "$(dirname "$0")" || exit

# Set API_PORT to 8082 if not set.
#
# Adapted from
#
# - https://google.github.io/styleguide/shellguide.html
# - https://stackoverflow.com/a/13864829/4638182
if [[ -z "${API_PORT+x}" ]]; then
  API_PORT=8082
fi

# Adapted from https://stackoverflow.com/a/17030976/4638182
printf "%0.s\n" {1..50}

set -x
curl -s -v "http://localhost:${API_PORT}/tpm/gene-disease-gtex/json?ensemblId=ENSG00000213420&efoId=EFO_0000621" > results/test-gene-disease-gtex.json

printf "%0.s\n" {1..6}

curl -s -v "http://localhost:${API_PORT}/tpm/gene-disease-gtex/plot?ensemblId=ENSG00000213420&efoId=EFO_0000621" > plots/test-gene-disease-gtex.png

printf "%0.s\n" {1..6}

curl -s -v "http://localhost:${API_PORT}/tpm/gene-all-cancer/json?ensemblId=ENSG00000213420" > results/test-gene-all-cancer.json

printf "%0.s\n" {1..6}

curl -s -v "http://localhost:${API_PORT}/tpm/gene-all-cancer/plot?ensemblId=ENSG00000213420" > plots/test-gene-all-cancer.png
