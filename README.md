# OpenPedCan-api <!-- omit in toc -->

[![GitHub Super-Linter](https://github.com/PediatricOpenTargets/OpenPedCan-api/workflows/Lint%20Code%20Base/badge.svg)](https://github.com/marketplace/actions/super-linter)

`OpenPedCan-api` implements OpenPedCan (Open Pediatric Cancers) project public API (application programming interface) to transfer [OpenPedCan-analysis](https://github.com/PediatricOpenTargets/OpenPedCan-analysis) results and plots via HTTP, which is publicly available at <https://openpedcan-api-qa.d3b.io/__docs__/>.

- [1. API endpoint specifications](#1-api-endpoint-specifications)
- [2. Deploy `OpenPedCan-api`](#2-deploy-openpedcan-api)
- [3. Test run `OpenPedCan-api` server locally](#3-test-run-openpedcan-api-server-locally)
  - [3.1. `git clone` `OpenPedCan-api` repository](#31-git-clone-openpedcan-api-repository)
  - [3.2. Run static R code analysis using R package `lintr`](#32-run-static-r-code-analysis-using-r-package-lintr)
  - [3.3. (Optional) Build data model files](#33-optional-build-data-model-files)
  - [3.4. Build `OpenPedCan-api` docker image](#34-build-openpedcan-api-docker-image)
  - [3.5. Run `OpenPedCan-api` docker image](#35-run-openpedcan-api-docker-image)
  - [3.6. Test `OpenPedCan-api` server using `curl`](#36-test-openpedcan-api-server-using-curl)
- [4. API system design](#4-api-system-design)
  - [4.1. Data model layer](#41-data-model-layer)
  - [4.2. Analysis logic layer](#42-analysis-logic-layer)
  - [4.3. API layer](#43-api-layer)
  - [4.4. HTTP server layer](#44-http-server-layer)
  - [4.5. Testing layer](#45-testing-layer)
  - [4.6. Deployment layer](#46-deployment-layer)
- [5. API Development roadmap](#5-api-development-roadmap)

## 1. API endpoint specifications

<https://openpedcan-api-qa.d3b.io/__docs__/> specifies the following API endpoint attributes.

- HTTP request method
- Path
- Parameters
- Response media type

## 2. Deploy `OpenPedCan-api`

According to comments and messages by @blackdenc :

`OpenPedCan-api` is deployed with the following steps:

- Build and tag `OpenPedCan-api` docker image using `Dockerfile`.
- Push the built image to Amazon Elastic Container Registry (ECR).
- Pass the ECR docker image tag to Amazon Elastic Container Service (ECS) Fargate (?) task definition at runtime.

<https://openpedcan-api-qa.d3b.io/__docs__/> is the QA server that will only deploy the `main` branch of the repository.

<https://openpedcan-api-dev.d3b.io/__docs__/> is the DEV server that will deploy any new branch of the repository, and the QA environment will remain un-changed until a new commit is merged to main.

`Dockerfile` builds the `OpenPedCan-api` docker image to be run on Amazon ECS.

To deploy without using docker:

- `Rscript --vanilla main.R` needs to be run with the same working directory as the last `WORKDIR` path in `Dockerfile` prior to the docker instruction `ENTRYPOINT ["Rscript", "--vanilla", "main.R"]`.
- Build or download `db` files according to the commands in `Dockerfile`.

## 3. Test run `OpenPedCan-api` server locally

Test run `OpenPedCan-api` server with the following steps:

- `git clone` `OpenPedCan-api` repository. Checkout a branch/commit that needs to be tested.
- Run static R code analysis using R package `lintr`.
- (Optional) Build data model files locally. This step takes > 25GB memory. This step is optional, because pre-built `OpenPedCan-api` data model files are publicly available via HTTP.
- Build `OpenPedCan-api` docker image. The docker image can either use local pre-built or remote pre-built data model files. This step takes < 8GB memory.
- Run `OpenPedCan-api` docker image. The `docker run` started docker container runs `OpenPedCan-api` server. This step takes < 8GB memory.
- Test `OpenPedCan-api` server using `curl`.

Note that this test run procedure has only been tested on linux operating system, with the following environment.

```text
Working directory is the git repository root directory, i.e. the directory
that contains the .git directory of the repository.

ubuntu 18
docker 19.03
curl 7.78
git 2.30
ImageMagick 6.9
shellcheck 0.7
sha256sum 8.28 # shasum for Mac OS, but not tested for any version.
md5sum 8.28
R 4.1
R package readr 1.4.0
R package jsonlite 1.7.2
R package lintr 2.0.1
```

### 3.1. `git clone` `OpenPedCan-api` repository

```bash
# Change URL if a fork repo needs to be used
git clone https://github.com/PediatricOpenTargets/OpenPedCan-api.git

cd OpenPedCan-api

git checkout -t origin/the-branch-that-needs-to-be-tested
# Optionally, checkout a commit with the following command
#
# git checkout COMMIT_HASH_ID
```

### 3.2. Run static R code analysis using R package `lintr`

```bash
./tests/run_r_lintr.sh
```

If there is any syntax error, comment in the GitHub pull request with the full error messages.

### 3.3. (Optional) Build data model files

Use the following bash command to build data model files locally to the `db` directory. This step takes > 25GB memory.

```bash
./db/build_db.sh
```

`./db/build_db.sh` runs the following steps:

- Build a docker image `open-ped-can-api-build-db`. The docker build procedure also build data model files.
- Copy data model files from `open-ped-can-api-build-db` docker image to host `db` directory.
- Check `sha256sum` for data model files.

### 3.4. Build `OpenPedCan-api` docker image

Use the following bash commands to Build `OpenPedCan-api` docker image.

- Use remote pre-built data model files. Even if there are local pre-built data model files, the docker image will still use the remote pre-built data model files, assuming that `curl -o` overwrites output destination (this assumption is tested to be true for the `OpenPedCan-api` docker ubuntu image but may not hold for other operating systems).

  ```bash
  docker build --no-cache -t open-ped-can-api .
  ```

- Use local pre-built data model files.

  ```bash
  docker build --no-cache --build-arg DB_LOCATION=local -t open-ped-can-api .
  ```

Note for developers: For `docker build` with docker cache and remote pre-built data model files, pass `--build-arg CACHE_DATE=$(date +%s)` to the `docker build` command to use the latest remote data model files on each build.

### 3.5. Run `OpenPedCan-api` docker image

```bash
docker run --rm -p 8082:80 open-ped-can-api
```

Note for developers: To run extra R `stopifnot(...)` assertions, pass `-e DEBUG=1` to `docker run` command.

### 3.6. Test `OpenPedCan-api` server using `curl`

Test the running server with the following command.

```bash
./tests/curl_test_endpoints.sh
```

`tests/curl_test_endpoints.sh` sends multiple HTTP requests to `localhost:8082` by default, with the following steps. The port number of `localhost` can be changed by passing the `bash` environment variable `LOCAL_API_HOST_PORT` with a different value, but there has to be a `OpenPedCan-api` server listening on the port. The API HTTP server host can be changed to <https://openpedcan-api-qa.d3b.io/__docs__/> or <https://openpedcan-api-dev.d3b.io/__docs__/>, by passing environment variable `API_HOST=qa` or `API_HOST=dev` respectively.

- Send an HTTP request using `curl`.
- Output the HTTP response body to `tests/http_response_output_files/png` or `tests/http_response_output_files/json`.
- Print HTTP response status code, content type, and run time.
- If response body content type is JSON, convert the JSON file to TSV file in `tests/results`.

## 4. API system design

The `OpenPedCan-api` server system has the following layers:

- Data model layer specifies data/database structures.
- Analysis logic layer specifies analytic procedures to generate results and plots using the data model layer.
- API (application programming interface) layer specifies the services/interfaces provided by the system.
- HTTP server layer specifies the procedures to handle every HTTP request.
- Testing layer specifies the procedures to test the `OpenPedCan-api` server.
- Deployment layer specifies the procedures to deploy the `OpenPedCan-api` server.

For more details about implementations, see [Test run `OpenPedCan-api` server locally](#test-run-openpedcan-api-server-locally) section.

The root directory of this repository should only contain starting points of different layer and configuration files.

### 4.1. Data model layer

`db` directory contains files that implement the data model layer.

`db/build_db.sh` builds data model files that are used by analysis logic layer.

`db/load_db.sh` loads local or remote pre-built data model files to the HTTP server layer.

### 4.2. Analysis logic layer

`src` directory contains files that implement the analysis logic layer.

### 4.3. API layer

Discussions in PedOT meetings, Slack work space, GitHub issues, etc specify the API layer.

### 4.4. HTTP server layer

`main.R` runs the `OpenPedCan-api` HTTP server. The HTTP server is implemented using [libuv](http://docs.libuv.org/en/stable/design.html) and [http-parser](https://github.com/nodejs/http-parser) and called by [R package plumber](https://github.com/rstudio/plumber).

The API HTTP server handles every HTTP request [sequentially](https://www.rplumber.io/articles/execution-model.html#performance-request-processing) with the following steps:

- Pre-process the HTTP request.
- Find the API endpoint for handling the request.
- Run the R function defined for the API endpoint.
- Convert the return value of the endpoint R function to defined response content type, e.g. JSON and PNG.
- Send HTTP response to the request address.

### 4.5. Testing layer

The `tests` directory contain all tools and code for testing the API server. `tests/http_response_output_files` contains the API server response plots and tables. `tests/results` contains results generated during test run.

### 4.6. Deployment layer

Jenkinsfile and Dockerfile specify the procedures to deploy the `OpenPedCan-api` server.

## 5. API Development roadmap

Implementation action items:

- Build data model into a Postgres database. Implement/refactor R functions to interact with the Postgres database. This will reduce RAM usage. This may reduce run time.
- Add unit tests to R functions.
- Send more informative response HTTP status code. Currently, all failures use status code 500.
- Speed up docker image build process by specifying only required files in one or more `.dockerignore` files.

Design action items:

- Resolve gene ENSG ID to symbol mapping inconsistencies between [OpenPedCan-analysis](https://github.com/PediatricOpenTargets) and [Pediatric Open Targets (PedOT) QA site](https://ppdc-otp-stage.bento-tools.org). One gene ENSG ID may map to different symbols in OpenPedCan-analysis and PedOT.
  - Get a list of all inconsistent mapping cases.
    - Currently, only one inconsistent mapping case is known, in which one ENSG ID could map to multiple symbols in OpenPedCan-analysis and only one symbol in PedOT. Following is an example of one ENSG ID mapping to multiple symbols in OpenPedCan-analysis:
      - In OpenPedCan-analysis, `ENSG00000273032` maps to `DGCR5` and `DGCR9`, and they have different TPM values.
  
        ```text
        r$> summary(as.numeric(input_df_list$tpm_df['DGCR5',]))                                                       
             Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
          0.00000   0.03414   0.17000   3.41687   1.29000 146.20000 
        
        r$> summary(as.numeric(input_df_list$tpm_df['DGCR9',]))                                                       
            Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
          0.0000   0.1556   0.4467   3.3563   1.2670 189.6000 
        ```
  
      - In PedOT, `ENSG00000273032` only maps to `DGCR5`, <https://ppdc-otp-stage.bento-tools.org/target/ENSG00000273032/associations>. There is no `DGCR9`, <https://ppdc-otp-stage.bento-tools.org/search?q=DGCR9&page=1>.
    - One ENSG ID could also only map to only one symbol in OpenPedCan-analysis that is different from the mapped one on PedOT.
  - Resolve all inconsistent mapping cases.
    - Currently, if one ENSG ID maps to multiple symbols in OpenPedCan-analysis, select the first of sorted gene symbols, but the selected one may not match PedOT.
    - Multiple symbols should probably not be returned for only one ENSG ID, in order to have one box in the boxplot only contains TPM values of one ENSG ID and one symbol.
    - Potential alternative solutions:
      - Completely drop gene symbol in plots and results, as it is also shown on PedOT.
      - Add `geneSymbol` to API endpoint parameters. Concerns: 1) PedOT may not be able to pass `geneSymbol` to the API query; 2) `geneSymbol` may not be URL friendly.
      - Use PedOT gene mappings in OpenPedCan-analysis, as a long term goal. This will involve updating many OpenPedCan-analysis pipelines and analysis modules.
