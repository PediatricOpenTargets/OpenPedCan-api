# OpenPedCan-api

## v0.3.0-beta

### Changed

- Changed data model layer backend from memory to database.
- Changed data model building procedure in `db/build_db.sh`.
- Changed data model loading procedure in `db/load_db.sh`.
- Changed `.dockerignore` to ignore `OpenPedCan-analysis` and `db/build_outputs`. The ignored directories are bind mounted to docker containers when necessary, in order to prevent transferring a large amount, potentially over 100GB, of data to docker daemon when building images.
- Changed static code analysis script from `tests/run_r_lintr.sh` to `tests/run_linters.sh`.

### Added

- Added `docker-compose.yml` to test coordinations between `OpenPedCan-api` HTTP server and database server locally.
- Added `db/db.Dockerfile` to test run `OpenPedCan-api` database server locally.
- Added `db/build_tools` to organize tools for building data model.
- Added `db/build_outputs` to organize output files of data model building procedure.
- Added `db/init_db.sh` and `db/init_db_pwfile.sh` to initialize database management system (DBMS) for building and loading data model locally.
- Added `db/r_interfaces` for analysis logic layer to interact with data model layer.
- Added `.env` file for `docker-compose.yml` runtime environment.
- Added `tests/linters` to organize static code analysis tools.
- Added `tests/git_diff_image.sh` to compare images with git `HEAD` for pixel differences.

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
