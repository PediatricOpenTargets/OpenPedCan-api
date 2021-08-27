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
printf "%0.s\n" {1..20}

base_url="http://localhost:${API_PORT}"
http_request_method="GET"

for ensg_id in "ENSG00000213420" "ENSG00000157764"; do
  for efo_id in "EFO_0000621" "EFO_0005543"; do
    query_url="${base_url}/tpm/gene-disease-gtex/json?ensemblId=${ensg_id}&efoId=${efo_id}"
    output_dir="http_response_output_files/json"
    output_fn="test-tpm-gene-disease-gtex-${ensg_id}-${efo_id}.json"
    curl -X "${http_request_method}" -s -w "${http_request_method} ${query_url}\nhttp_code: %{http_code}\ncontent_type: %{content_type}\ntime_total: %{time_total} seconds\n\n\n" -o "${output_dir}/${output_fn}" "${query_url}"
    jq '.[0:3]' "${output_dir}/${output_fn}"

    query_url="${base_url}/tpm/gene-disease-gtex/plot?ensemblId=${ensg_id}&efoId=${efo_id}"
    output_dir="http_response_output_files/png"
    output_fn="test-tpm-gene-disease-gtex-${ensg_id}-${efo_id}.png"
    curl -X "${http_request_method}" -s -w "${http_request_method} ${query_url}\nhttp_code: %{http_code}\ncontent_type: %{content_type}\ntime_total: %{time_total} seconds\n\n\n" -o "${output_dir}/${output_fn}" "${query_url}"
  done
  query_url="${base_url}/tpm/gene-all-cancer/json?ensemblId=${ensg_id}"
  output_dir="http_response_output_files/json"
  output_fn="test-tpm-gene-all-cancer-${ensg_id}.json"
  curl -X "${http_request_method}" -s -w "${http_request_method} ${query_url}\nhttp_code: %{http_code}\ncontent_type: %{content_type}\ntime_total: %{time_total} seconds\n\n\n" -o "${output_dir}/${output_fn}" "${query_url}"
  jq '.' "${output_dir}/${output_fn}"

  query_url="${base_url}/tpm/gene-all-cancer/plot?ensemblId=${ensg_id}"
  output_dir="http_response_output_files/png"
  output_fn="test-tpm-gene-all-cancer-${ensg_id}.png"
  curl -X "${http_request_method}" -s -w "${http_request_method} ${query_url}\nhttp_code: %{http_code}\ncontent_type: %{content_type}\ntime_total: %{time_total} seconds\n\n\n" -o "${output_dir}/${output_fn}" "${query_url}"
done
