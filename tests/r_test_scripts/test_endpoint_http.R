# The working directory is the directory that contains this test R file, if this
# file is executed by test_dir
#
# testthat package is loaded, if this file is executed by test_dir
context("tests/r_test_scripts/test_endpoint_http.R")

# Function definitions ---------------------------------------------------------

# Send HTTP GET request and get response
#
# Args:
# - url: a single character value of url.
#
# Returns a list with time and content entries.
http_get <- function(url) {
  stopifnot(is.character(url))
  stopifnot(identical(length(url), 1L))
  stopifnot(all(!is.na(url)))

  res <- httr::GET(url)

  testthat::expect_identical(
    res$status_code, 200L,
    label = paste0("GET ", url, " response code"))

  res_list <- list(
    time = res$times,
    content = httr::content(res, "raw")
  )

  return(res_list)
}



# Test API endpoint through HTTP
#
# Args:
# - endpoint_spec: endpoint specification, with is a list with path and params
#   entries.
#
# Returns a list of endpoint response times.
test_endpoint <- function(endpoint_spec) {
  stopifnot(is.character(endpoint_spec$path))
  stopifnot(identical(length(endpoint_spec$path), 1L))
  stopifnot(all(!is.na(endpoint_spec$path)))
  stopifnot(all(nchar(endpoint_spec$path) > 0))

  stopifnot(is.character(endpoint_spec$params))
  stopifnot(all(!is.na(endpoint_spec$params)))
  stopifnot(all(nchar(endpoint_spec$params) > 0))

  param_val_comb_tbl <- purrr::cross_df(param_val_list[endpoint_spec$params])
  stopifnot(identical(sum(is.na(param_val_comb_tbl)), 0L))

  res_type_match_mat = stringr::str_match(endpoint_spec$path, "/([^/]+)$")
  stopifnot(identical(nrow(res_type_match_mat), 1L))
  stopifnot(identical(ncol(res_type_match_mat), 2L))
  stopifnot(identical(sum(is.na(res_type_match_mat)), 0L))

  res_type <- res_type_match_mat[1, 2]
  stopifnot(is.character(res_type))

  endpoint_output_id = stringr::str_replace_all(
    stringr::str_remove(endpoint_spec$path, "^/"), "/", "-")
  stopifnot(is.character(endpoint_output_id))
  stopifnot(identical(length(endpoint_output_id), 1L))

  test_url_glue_template <- paste0(
    base_url, endpoint_spec$path, "?",
    paste0(
      endpoint_spec$params, "={", endpoint_spec$params, "}", collapse = "&"))

  output_path_glue_template_list <- purrr::map(
    output_spec_list,
    function(output_spec) {
      output_path_glue_template <- file.path(
        output_spec$output_dir,
        paste0(
          "test-", endpoint_output_id, "-",
          paste0("{", endpoint_spec$params, "}", collapse = "-"),
          output_spec$output_sfx))
      return(output_path_glue_template)
    }
  )

  endpoint_test_tbl <- dplyr::mutate(
    param_val_comb_tbl, test_url = glue::glue(test_url_glue_template))

  if (res_type == "json") {
    endpoint_test_tbl <- dplyr::mutate(
      endpoint_test_tbl,
      output_json_path = glue::glue(output_path_glue_template_list$json),
      output_tsv_path = glue::glue(output_path_glue_template_list$tsv))

    res_time_list <- purrr::pmap(
      endpoint_test_tbl[, c("test_url", "output_json_path", "output_tsv_path")],
      function(test_url, output_json_path, output_tsv_path) {
        res_list <- http_get(test_url)
        writeBin(res_list$content, output_json_path)
        readr::write_tsv(jsonlite::fromJSON(output_json_path), output_tsv_path)

        return(res_list$time)
      }
    )

  } else if (res_type == "plot") {
    endpoint_test_tbl <- dplyr::mutate(
      endpoint_test_tbl,
      output_png_path = glue::glue(output_path_glue_template_list$png))

    res_time_list <- purrr::pmap(
      endpoint_test_tbl[, c("test_url", "output_png_path")],
      function(test_url, output_png_path) {
        res_list <- http_get(test_url)
        writeBin(res_list$content, output_png_path)

        return(res_list$time)
      }
    )

  } else {
    stop(paste0("Unknown response type ", res_type))
  }

  return(res_time_list)
}


# Data variable definitaions ---------------------------------------------------

base_url <- Sys.getenv("BASE_URL", unset = NA_character_)
stopifnot(!is.na(base_url))
stopifnot(is.character(base_url))
stopifnot(identical(length(base_url), 1L))

param_val_list <- list(
  ensemblId = c("ENSG00000213420", "ENSG00000157764", "ENSG00000273032"),

  efoId = c("EFO_0000621", "Orphanet_178", "MONDO_0016718",
            "MONDO_0016680", "MONDO_0016685"),

  yAxisScale = c("linear", "log10"),

  includeTumorDesc = c("primaryOnly", "relapseOnly",
                       "primaryAndRelapseInSameBox",
                       "primaryAndRelapseInDifferentBoxes")
)

endpoint_spec_list <- list(
  list(
    path = "/tpm/gene-disease-gtex/json",
    params = c("ensemblId", "efoId", "includeTumorDesc")),
  list(
    path = "/tpm/gene-disease-gtex/plot",
    params = c("ensemblId", "efoId", "yAxisScale", "includeTumorDesc")),
  list(
    path = "/tpm/gene-all-cancer/json",
    params = c("ensemblId", "includeTumorDesc")),
  list(
    path = "/tpm/gene-all-cancer/plot",
    params = c("ensemblId", "yAxisScale", "includeTumorDesc")),
  list(
    path = "/tpm/gene-all-cancer-collapsed-gtex/json",
    params = c("ensemblId", "includeTumorDesc")),
  list(
    path = "/tpm/gene-all-cancer-collapsed-gtex/plot",
    params = c("ensemblId", "yAxisScale", "includeTumorDesc")),
  list(
    path = "/tpm/gene-all-cancer-gtex/json",
    params = c("ensemblId", "includeTumorDesc")),
  list(
    path = "/tpm/gene-all-cancer-gtex/plot",
    params = c("ensemblId", "yAxisScale", "includeTumorDesc"))
)

output_spec_list <- list(
  tsv = list(
    output_dir = file.path("..", "results"),
    output_sfx = ".tsv"),
  json = list(
    output_dir = file.path("..", "http_response_output_files", "json"),
    output_sfx = ".json"),
  png = list(
    output_dir = file.path("..", "http_response_output_files", "png"),
    output_sfx = ".png")
)



# Run tests and summarise results ----------------------------------------------
endpoint_res_time_list <- purrr::map(endpoint_spec_list, test_endpoint)

print(endpoint_res_time_list)
