# plumber.R defines plumber API
#
# This file should be run with the directory that contains this file as working
# directory. ../main.R calls plumber::pr("path/to/this/plumber.R"), which runs
# this plumber.R file  with the directory that contains this file as working
# directory.


# Get %>% without loading the whole library
`%>%` <- magrittr::`%>%`

# Function definitions ---------------------------------------------------------

# Read and process data --------------------------------------------------------
data_dir <- file.path("..", "OpenPedCan-analysis", "data")

input_df_list <- list(
  histology_df = readr::read_tsv(
    file.path(data_dir, "histologies.tsv"),
    col_types = readr::cols(), guess_max = 1e6),
  all_cohorts_indep_samples = readr::read_tsv(
    file.path(data_dir, "independent-specimens.rnaseq.primary.tsv"),
    col_types = readr::cols()),
  each_cohort_indep_samples = readr::read_tsv(
    file.path(data_dir, "independent-specimens.rnaseq.primary.eachcohort.tsv"),
    col_types = readr::cols()),
  tpm_df = readRDS(
    file.path(data_dir, "gene-expression-rsem-tpm-collapsed.rds")),
  ensg_symbol_rmtl_df = readr::read_tsv(
    file.path(data_dir, "ensg-hugo-rmtl-mapping.tsv"),
    col_types = readr::cols())
)

purrr::walk(
  input_df_list[c("histology_df", "all_cohorts_indep_samples",
                  "each_cohort_indep_samples")],
  function(x) {
    stopifnot(!is.null(x$Kids_First_Participant_ID))
    stopifnot(!is.null(x$Kids_First_Biospecimen_ID))
    stopifnot(identical(sum(is.na(x$Kids_First_Participant_ID)), as.integer(0)))
    stopifnot(identical(sum(is.na(x$Kids_First_Biospecimen_ID)), as.integer(0)))
  }
)

stopifnot(!is.null(colnames(input_df_list$tpm_df)))
stopifnot(!is.null(rownames(input_df_list$tpm_df)))
stopifnot(identical(sum(is.na(colnames(input_df_list$tpm_df))), as.integer(0)))
stopifnot(identical(sum(is.na(rownames(input_df_list$tpm_df))), as.integer(0)))

stopifnot(identical(
  is.na(input_df_list$ensg_symbol_rmtl_df$rmtl),
  is.na(input_df_list$ensg_symbol_rmtl_df$version)))

input_df_list$ensg_symbol_rmtl_df <- input_df_list$ensg_symbol_rmtl_df %>%
  dplyr::mutate(
    RMTL = dplyr::if_else(
      is.na(rmtl), true = NA_character_,
      false = paste0(rmtl, " (", version, ")"))) %>%
  dplyr::select(ensg_id, gene_symbol, RMTL) %>%
  dplyr::rename(Gene_Ensembl_ID = ensg_id, Gene_symbol = gene_symbol) %>%
  dplyr::distinct()

stopifnot(!is.null(input_df_list$ensg_symbol_rmtl_df$Gene_Ensembl_ID))
stopifnot(identical(
  sum(is.na(input_df_list$ensg_symbol_rmtl_df$Gene_Ensembl_ID)),
  as.integer(0)))
stopifnot(!is.null(input_df_list$ensg_symbol_rmtl_df$Gene_symbol))
stopifnot(identical(
  sum(is.na(input_df_list$ensg_symbol_rmtl_df$Gene_symbol)),
  as.integer(0)))

# Annotate histology df --------------------------------------------------------
# Rename columns to annotator columns
input_df_list$histology_df <- input_df_list$histology_df %>%
  dplyr::rename(
    Disease = cancer_group, GTEx_tissue_group = gtex_group,
    GTEx_tissue_subgroup = gtex_subgroup)
# annotator only when working directory is OpenPedCan-analysis or its subdir
setwd(file.path("..", "OpenPedCan-analysis"))
source(file.path(
  "analyses", "long-format-table-utils", "annotator", "annotator-api.R"))
input_df_list$histology_df <- annotate_long_format_table(
  input_df_list$histology_df,
  columns_to_add = c(
    "EFO", "MONDO", "GTEx_tissue_group_UBERON", "GTEx_tissue_subgroup_UBERON"))
# change working directory back to src
setwd(file.path("..", "src"))


# Subset independent samples ---------------------------------------------------
tpm_data_lists <- list(
  all_cohorts = list(
    tpm_df = input_df_list$tpm_df,
    independent_samples = input_df_list$all_cohorts_indep_samples,
    histology_df = input_df_list$histology_df
  ),
  each_cohort = list(
    tpm_df = input_df_list$tpm_df,
    independent_samples = input_df_list$each_cohort_indep_samples,
    histology_df = input_df_list$histology_df
  )
)

# TPM subset data lists
tpm_ss_data_lists <- lapply(tpm_data_lists, function(xl) {
  tpm_sids <- colnames(xl$tpm_df)
  subset_sids <- tpm_sids[
    tpm_sids %in% xl$independent_samples$Kids_First_Biospecimen_ID]
  subset_tpm_df <- xl$tpm_df[, subset_sids]
  stopifnot(all(subset_sids %in% xl$histology_df$Kids_First_Biospecimen_ID))

  subset_indep_samples <- xl$independent_samples %>%
    dplyr::filter(Kids_First_Biospecimen_ID %in% subset_sids)

  subset_histology_df <- xl$histology_df %>%
    dplyr::filter(Kids_First_Biospecimen_ID %in% subset_sids)

  # TODO: subset GTEx TPM list
  subset_data_list <- list(
    tpm_df = subset_tpm_df,
    independent_samples = subset_indep_samples,
    histology_df = subset_histology_df
  )
})

# Plumber API definitions ------------------------------------------------------


# logger adapted from
# https://www.rplumber.io/articles/routing-and-input.html#forward-to-another-handler

#* Log some information about the incoming request
#* @filter logger
function(req){
  cat(as.character(Sys.time()), "-\n",
    req$REQUEST_METHOD, req$PATH_INFO, "-\n",
    "body ", req$body, "-\n",
    "QUERY_STRING ", req$QUERY_STRING, "-\n",
    req$HTTP_USER_AGENT, "@", req$REMOTE_ADDR, "\n")
  str(req)
  cat("--------------------------")
  plumber::forward()
}

#* Get single-cancer GTEx plot and table
#* @serializer json
#* @get /single-cancer-gtex
function() {
  input_df_list$histology_df[1:3, 1:3]
}


#* Echo back the input
#* @param msg The message to echo
#* @get /echo
function(msg="") {
  list(msg = paste0("The message is: '", msg, "'"))
}

#* Plot a histogram
#* @serializer png
#* @get /plot
function() {
  rand <- rnorm(100)
  hist(rand)
}

#* Return the sum of two numbers
#* @param a The first number to add
#* @param b The second number to add
#* @post /sum
function(a, b) {
  as.numeric(a) + as.numeric(b)
}
