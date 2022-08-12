# OpenPedCan-api

## v0.5.3-beta

### Changed

- Updated database using `OpenPedCan-analysis` v11 TPM data and independent sample lists.
- Updated `tests/r_test_scripts/test_endpoint_http.R` according to `OpenPedCan-analysis` v11 TPM data.

## v0.5.2-beta

### Changed

- Updated database using v10 differential gene expression (DGE) data.
- Installed `libxml2` in `Dockerfile`.
- Updated tests for DGE heatmap endpoints, according to v10 DGE data.

## v0.5.1-beta

### Changed

- Changed the interfaces of the following endpoints by adding two required parameters, `includeBoxplot` and `boxplotYAxisScale`, to determine whether to add boxplots with certain Y-axis scales on the sides of the differential gene expression (DGE) heatmaps or not.
  - `/dge/top-gene-disease-gtex-diff-exp/plot`
  - `/dge/gene-all-cancer-gtex-diff-exp/plot`

## v0.5.0-beta

### Added

- Added the following differential gene expression (DGE) endpoints to transfer DGE heatmap PNG plots and JSON tables via HTTP `GET` method.
  - `/dge/top-gene-disease-gtex-diff-exp/json`: Transfer a JSON table of one disease and top differentially expressed genes.
  - `/dge/top-gene-disease-gtex-diff-exp/plot`: Transfer a PNG heatmap of one disease and top differentially expressed genes.
  - `/dge/gene-all-cancer-gtex-diff-exp/json`: Transfer a DGE JSON table of one gene and all diseases.
  - `/dge/gene-all-cancer-gtex-diff-exp/plot`: Transfer a DGE PNG heatmap of one gene and all diseases.
- Added DGE database table `BULK_EXP_SCHEMA.BULK_EXP_DIFF_EXP_TBL`, which is declared in `../OpenPedCan-api-secrets/common_db.env` and `db/r_interfaces/db_env_vars.R` and constructed in `db/build_tools/build_db.R` and `db/build_tools/build_db_docker_cmd.sh`.
- Added the following R scripts to implement the added DGE endpoints.
  - `src/get_one_efo_top_ensg_diff_exp_heatmap.R`
  - `src/get_one_efo_top_ensg_diff_exp_heatmap_tbl.R`
  - `src/get_one_efo_top_ensg_diff_exp_tbl.R`
  - `src/get_one_ensg_all_efo_diff_exp_heatmap.R`
  - `src/get_one_ensg_all_efo_diff_exp_heatmap_tbl.R`
  - `src/get_one_ensg_all_efo_diff_exp_tbl.R`
- Added tests in `tests/r_test_scripts/test_endpoint_http.R` to test the added DGE endpoints.

## v0.4.0-beta

### Changed

- Changed the interfaces of the following endpoints by adding a required parameter `includeTumorDesc` to determine how independent primary and relapse tumor samples should be handled.
  - `/tpm/gene-disease-gtex/json`
  - `/tpm/gene-disease-gtex/plot`
  - `/tpm/gene-all-cancer/json`
  - `/tpm/gene-all-cancer/plot`
  - `/tpm/gene-all-cancer-collapsed-gtex/json`
  - `/tpm/gene-all-cancer-collapsed-gtex/plot`
  - `/tpm/gene-all-cancer-gtex/json`
  - `/tpm/gene-all-cancer-gtex/plot`
- Removed `tests/curl_test_endpoints.sh`.
- Changed the interface of `get_gene_tpm_tbl` function:
  - Changed `gtex_sample_group` parameter value choices to `"require"` and `"exclude"`.
  - Added a required parameter `relapse_sample_group`.
  - Removed `min_n_per_sample_group` parameter.
  - Added a column `specimen_descriptor` to returned table.
- Removed `src/add_gene_tpm_box_group.R`. The procedure in `add_gene_tpm_box_group` function is integrated into `get_gene_tpm_boxplot_tbl` function.
- Changed the interface of `get_gene_tpm_boxplot_tbl` function:
  - Added a required parameter `gtex_histology_group`.
  - Added a required parameter `spec_desc_group`.
  - Added a required parameter `min_n_per_box`.
  - Changed the returned table:
    - Renamed `x_labels` column to `x_label`.
    - Removed `box_group` column.
    - Added `histology_group` column.
    - Added `specimen_descriptor` column.
    - Added `specimen_descriptor_fill` column.
    - Added `specimen_descriptor_x_label` column.
    - Added `box_sample_count` column.
    - Added `x_label_sample_count` column.
- Changed the returned table of `tpm_boxplot_summary_tbl` function:
  - Added `specimen_descriptor_fill` column.
  - Added `box_sample_count` column.
- Renamed `src/ggplot2_publication_theme.R` to `src/ggplot2_boxplot_theme.R`.
- Moved `results/test-*.tsv` to `results/tsv/test-*.tsv`.
- Changed boxplot tumor box fill colors from red to blue, orange, and purple for primary, relapse, and primary-and-relapse tumor samples respectively.
- Changed boxplot titles to describe primary and relapse conditions of included tumor samples.
- Changed boxplot title alignment from centered to left-aligned.
- Changed boxplot legend location from right to top-left.
- Changed boxplot right margin and image width.
- Added legend to boxplot with two or more box colors.

### Added

- Added independent relapse tumor samples to OpenPedCan-api database `bulk_expression` schema and `bulk_expression_tpm_histology` table.
- Added `specimen_descriptor` column to OpenPedCan-api database `bulk_expression` schema and `bulk_expression_tpm_histology` table. The `specimen_descriptor` column describes whether a specimen is obtained from primary tumor, or relapse tumor, or normal tissue.
- Added `tests/run_tests.sh` to test API.
- Added `tests/r_test_scripts/test_endpoint_http.R` to test API HTTP server, which outputs `tests/results/endpoint_response_times.tsv` and `tests/plots/endpoint_response_time_boxplot.png` to summarize HTTP response times.
- Added `src/get_tpm_endpoint_tbl.R` to call `get_gene_tpm_tbl` and `get_gene_tpm_boxplot_tbl` with proper parameters according to HTTP request query parameters.

## v0.3.3-beta

### Changed

- Changed the interface of `get_gene_tpm_tbl` function by adding a required parameter `gtex_sample_group`.
- Changed the interface of `add_gene_tpm_box_group` function by adding an optional parameter `gtex_box_group`.
- Changed the behavior of `get_gene_tpm_boxplot_summary_tbl` function by allowing one sample group to have multiple unique `GTEx_tissue_subgroup`s and `GTEx_tissue_subgroup_UBERON`s.

### Added

- Added the following all-cancer and all-GTEx-tissue TPM boxplot and summary table endpoints:
  - `/tpm/gene-all-cancer-collapsed-gtex/plot` transfers boxplot with all GTEx tissues collapsed into one box.
  - `/tpm/gene-all-cancer-collapsed-gtex/json` transfers summary table with all GTEx tissues collapsed into one sample group.
  - `/tpm/gene-all-cancer-gtex/plot` transfers boxplot with each GTEx tissue subgroup as one box.
  - `/tpm/gene-all-cancer-gtex/json` transfers summary table with each GTEx tissue subgroup as one sample group.
- Added `curl` tests for the above all-cancer and all-GTEx-tissue TPM boxplot and summary table endpoints, in `tests/curl_test_endpoints.sh`.

## v0.3.2-beta

### Changed

- Changed OpenPedCan-api database source data from OpenPedCan-analysis v9 release to v10 release.
- Updated `OpenPedCan-analysis` submodule to `dev` branch v10 release commit.
- Changed `db/build_db.sh` `OPEN_PED_CAN_ANALYSIS_COMMIT` from `OpenPedCan-analysis` `dev` branch v9 release commit to v10 release commit.
- Renamed OpenPedCan-api database "RMTL" column to "PMTL".
- Changed OpenPedCan-api database "cohort" column "all_cohorts" values to "All Cohorts".
- Renamed OpenPedCan-api database dump file from "postgres_db_open_ped_can_db_schema_bulk_expression.sql.gz" to "open_ped_can_db_postgres_pg_dump.sql.gz".
- Changed `/tpm/gene-disease-gtex` boxplot title from "Primary tumor and GTEx tissue bulk gene expression" to "Pediatric tumor and GTEx normal adult tissue gene expression".
- Changed `/tpm/gene-all-cancer` boxplot title from "Primary tumor tissue bulk gene expression" to "Pediatric tumor gene expression".

### Added

- Add `API_HOST=prd` option in `tests/curl_test_endpoints.sh` to test production API site URL <https://openpedcan-api.d3b.io>.

## v0.3.1-beta

### Changed

- Changed `tpm/gene-disease-gtex/plot` boxplot title from "Primary tumor vs GTEx tissue bulk gene expression" to "Primary tumor and GTEx tissue bulk gene expression".

## v0.3.0-beta

### Changed

- Changed required parameters in `/tpm/gene-disease-gtex/plot` and `/tpm/gene-all-cancer/plot` endpoints, by adding `yAxisScale` as a required parameter.
- Changed `tests/curl_test_endpoints.sh` to test `yAxisScale` parameter.
- Changed data model layer backend from memory to database.
- Changed data model building procedure in `db/build_db.sh`.
- Changed data model loading procedure in `db/load_db.sh`.
- Changed `.dockerignore` to ignore `OpenPedCan-analysis` and `db/build_outputs`. The ignored directories are bind mounted to docker containers when necessary, in order to prevent transferring a large amount, potentially over 100GB, of data to docker daemon when building images.
- Changed static code analysis script from `tests/run_r_lintr.sh` to `tests/run_linters.sh`.

### Added

- Added `yAxisScale` as a required parameter in `/tpm/gene-disease-gtex/plot` and `/tpm/gene-all-cancer/plot` endpoints.
- Added `docker-compose.yml` to test coordinations between `OpenPedCan-api` HTTP server and database server locally.
- Added `db/db.Dockerfile` to test run `OpenPedCan-api` database server locally.
- Added `db/build_tools` to organize tools for building data model.
- Added `db/build_outputs` to organize output files of data model building procedure.
- Added `db/init_db.sh` and `db/init_db_pwfile.sh` to initialize database management system (DBMS) for building and loading data model locally.
- Added `db/r_interfaces` for analysis logic layer to interact with data model layer.
- Added `.env` file for `docker-compose.yml` runtime environment.
- Added `tests/linters` to organize static code analysis tools.
- Added `tests/git_diff_image.sh` to compare images with git `HEAD` for pixel differences.
- Added `/db-stats` API testing endpoint to get database statistics.
- Added `/db-connection` API testing endpoint to run a simple query.

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
