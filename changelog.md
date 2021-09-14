# OpenPedCan-api

## v0.2.0-alpha

### Changed

- Updated data model using [`OpenPedCan-analysis` v9 release data](https://github.com/PediatricOpenTargets/OpenPedCan-analysis/pull/103).
- Enabled endpoint Cross-Origin Resource Sharing (CORS) by default.
- Changed `/tpm/gene-disease-gtex` boxplot title from "Disease vs. GTEx tissue bulk gene expression" to "Primary tumor vs GTEx tissue bulk gene expression".
- Changed "cohort =" to "Dataset =" in boxplot and summary table x-axis labels.
- Changed "cohort" to "Dataset" in boxplot summary table columns.
- Increased minimum number of samples required per `Disease` or `GTEx_tissue_subgroup` from 1 to 3.
- Rotated boxplot x-axis labels by 45 degrees.
- Changed `tests/curl_test_endpoints.sh` variable `API_PORT` to `LOCAL_API_HOST_PORT`.
- Updated `README.md`.

### Added

- Implemented HTTP GET method for `/tpm/gene-all-cancer/json` API endpoint.
- Implemented HTTP GET method for `/tpm/gene-all-cancer/plot` API endpoint.
- Added `cors` filter to enable endpoint CORS by default.
- Added `API_HOST` variable in `tests/curl_test_endpoints.sh`, in order to test DEV and QA hosts.
- Added this `changelog.md`.

## v0.1.0-alpha

### Added

- HTTP GET method for `/tpm/gene-disease-gtex/json` API endpoint.
- HTTP GET method for `/tpm/gene-disease-gtex/plot` API endpoint.
- Tools for building API data model in `db` directory.
- Tools for testing local API HTTP server, in `tests` directory.
- `Dockerfile` for running API HTTP server.
- This `README.md` that specifies development procedure, system design, and development roadmap.
- Git submodule [`OpenPedCan-analysis`](https://github.com/PediatricOpenTargets/OpenPedCan-analysis).
