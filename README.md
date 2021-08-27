# OpenPedCan-api

[![GitHub Super-Linter](https://github.com/PediatricOpenTargets/OpenPedCan-api/workflows/Lint%20Code%20Base/badge.svg)](https://github.com/marketplace/actions/super-linter)

`OpenPedCan-api` implements OpenPedCan (Open Pediatric Cancers) project public API (application programming interface) to transfer [OpenPedCan-analysis](https://github.com/PediatricOpenTargets/OpenPedCan-analysis) results and plots via HTTP, which is publicly available at <https://openpedcan-api-dev.d3b.io/__docs__/>.

- [OpenPedCan-api](#openpedcan-api)
  - [API endpoint specifications](#api-endpoint-specifications)
  - [Deploy `OpenPedCan-api`](#deploy-openpedcan-api)
  - [Test run `OpenPedCan-api` server locally](#test-run-openpedcan-api-server-locally)
  - [API system design](#api-system-design)
  - [API Development roadmap](#api-development-roadmap)

## API endpoint specifications

<https://openpedcan-api-dev.d3b.io/__docs__/> specifies the following API endpoint attributes.

- HTTP request method
- Path
- Parameters
- Response media type

## Deploy `OpenPedCan-api`

Following is a comment by @blackdenc at <https://github.com/PediatricOpenTargets/OpenPedCan-api/issues/5#issuecomment-904824004>.

> As far as building in Jenkins, we build the container and tag it, then push it to ECR and give that tag to the ECS task definition at runtime.

`Rscript --vanilla main.R` needs to be run with the same working directory as the last `WORKDIR` path in `Dockerfile` prior to the docker instruction `ENTRYPOINT ["Rscript", "--vanilla", "main.R"]`.

## Test run `OpenPedCan-api` server locally

Use the following bash command to run `OpenPedCan-api` server locally on port 8082. Note that this has only been tested on linux operating system.

```bash
docker build -t open-ped-can-api . && docker run -p 8082:80 -e DEBUG=1 open-ped-can-api
```

Test the running server with the following command.

```bash
# tests/curl_test_endpoints.sh requires the following tools:
#
# - jq
# - curl
# - R
# - R package jsonlite
# - R package readr
#
# jq installation instructions are at https://stedolan.github.io/jq/download/
#
# Use system package management tools, e.g. conda, ubuntu apt-get, and mac homebrew, to install curl.
bash tests/curl_test_endpoints.sh
```

`tests/curl_test_endpoints.sh` sends multiple HTTP requests to `localhost:8082` by default, with the following steps.

- Send an HTTP request using `curl`.
- Output the HTTP response body to `tests/http_response_output_files/png` or `tests/http_response_output_files/json`.
- Print HTTP response status code, content type, and run time.
- If response body content type is JSON, convert the JSON file to TSV file in `tests/results`.

## API system design

`Dockerfile` builds a docker image to run `main.R` with the following steps:

- Install system and R package dependencies.
- `git clone` [OpenPedCan-analysis](https://github.com/PediatricOpenTargets/OpenPedCan-analysis) repository. Check out a specific commit.
- Download OpenPedCan-analysis release data.
- Run `Rscript --vanilla main.R`.

`main.R` runs the API server with the following steps:

- Build an OpenPedCan-analysis release data model.
- Import R functions defined to generate results and plots using the data model.
- Runs the API server with defined endpoints in `src/plumber.R`.

The API server handles every HTTP request [sequentially](https://www.rplumber.io/articles/execution-model.html#performance-request-processing) with the following steps:

- Pre-process the HTTP request.
- Find the API endpoint for handling the request.
- Run the R function defined for the API endpoint.
- Convert the return value of the endpoint R function to defined response content type, e.g. JSON and PNG.
- Send HTTP response to the request address.

The root directory should only contain starting points.

The `src` directory should contain all code files that are required for running the API server.

The `tests` directory should contain all tools and code for testing the API server. `tests/http_response_output_files` contains the API server response plots and tables. `tests/results` contains results generated during test run.

## API Development roadmap

- [ ] Implement endpoint `GET /tpm/gene-all-cancer/json`.
- [ ] Implement endpoint `GET /tpm/gene-all-cancer/plot`.
- [ ] Build data model in another R process. Load the data model in the API server R process. This may reduce RAM usage.
- [ ] Build data model into a Postgres database. Implement refactor R funtions to interact with the Postgres database. This will reduce RAM usage. This may reduce run time.
- [ ] Add unit tests to R functions.
- [ ] Send more informative response HTTP status code. Currently, all failures use status code 500.
