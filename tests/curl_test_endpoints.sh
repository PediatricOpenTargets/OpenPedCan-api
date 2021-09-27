#!/bin/bash
set -e
set -u
set -o pipefail

# This script should always run as if it were being called from
# the directory it lives in.
#
# Adapted from https://stackoverflow.com/a/3355423/4638182
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

# Adapted from https://stackoverflow.com/a/17030976/4638182
printf "%0.s\n" {1..20}

if [[ "${API_HOST}" == "local" ]]; then
  base_url="http://localhost:${LOCAL_API_HOST_PORT}"
elif [[ "${API_HOST}" == "qa" ]]; then
  base_url="https://openpedcan-api-qa.d3b.io"
elif [[ "${API_HOST}" == "dev" ]]; then
  base_url="https://openpedcan-api-dev.d3b.io"
else
  echo "Invalid API_HOST=${API_HOST}. Please choose from local, qa, and dev and rerun." 1>&2
  exit 1
fi

http_request_method="GET"

for ensg_id in "ENSG00000213420" "ENSG00000157764" "ENSG00000273032"; do
  for efo_id in "EFO_0000621" "Orphanet_178" "MONDO_0016718" "MONDO_0016680" "MONDO_0016685"; do
    query_url="${base_url}/tpm/gene-disease-gtex/json?ensemblId=${ensg_id}&efoId=${efo_id}"
    output_dir="http_response_output_files/json"
    output_fn="test-tpm-gene-disease-gtex-${ensg_id}-${efo_id}.json"
    curl -X "${http_request_method}" -s -w "${http_request_method} ${query_url}\nhttp_code: %{http_code}\ncontent_type: %{content_type}\ntime_total: %{time_total} seconds\n\n\n" -o "${output_dir}/${output_fn}" "${query_url}"
    # convert json to tsv
    Rscript --vanilla -e "readr::write_tsv(jsonlite::fromJSON('${output_dir}/${output_fn}'), 'results/${output_fn%.json}.tsv')"

    for y_axis_scale in "linear" "log10"; do
      query_url="${base_url}/tpm/gene-disease-gtex/plot?ensemblId=${ensg_id}&efoId=${efo_id}&yAxisScale=${y_axis_scale}"
      output_dir="http_response_output_files/png"
      output_fn="test-tpm-gene-disease-gtex-${ensg_id}-${efo_id}-y-scale-${y_axis_scale}.png"
      curl -X "${http_request_method}" -s -w "${http_request_method} ${query_url}\nhttp_code: %{http_code}\ncontent_type: %{content_type}\ntime_total: %{time_total} seconds\n\n\n" -o "${output_dir}/${output_fn}" "${query_url}"
    done
  done


  query_url="${base_url}/tpm/gene-all-cancer/json?ensemblId=${ensg_id}"
  output_dir="http_response_output_files/json"
  output_fn="test-tpm-gene-all-cancer-${ensg_id}.json"
  curl -X "${http_request_method}" -s -w "${http_request_method} ${query_url}\nhttp_code: %{http_code}\ncontent_type: %{content_type}\ntime_total: %{time_total} seconds\n\n\n" -o "${output_dir}/${output_fn}" "${query_url}"
  # convert json to tsv
  Rscript --vanilla -e "readr::write_tsv(jsonlite::fromJSON('${output_dir}/${output_fn}'), 'results/${output_fn%.json}.tsv')"


  for y_axis_scale in "linear" "log10"; do
    query_url="${base_url}/tpm/gene-all-cancer/plot?ensemblId=${ensg_id}&yAxisScale=${y_axis_scale}"
    output_dir="http_response_output_files/png"
    output_fn="test-tpm-gene-all-cancer-${ensg_id}-y-scale-${y_axis_scale}.png"
    curl -X "${http_request_method}" -s -w "${http_request_method} ${query_url}\nhttp_code: %{http_code}\ncontent_type: %{content_type}\ntime_total: %{time_total} seconds\n\n\n" -o "${output_dir}/${output_fn}" "${query_url}"
  done
done
