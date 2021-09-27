# tpm_data_lists.R is called by build_db.Dockerfile to output a variable,
# tpm_data_lists, into tpm_data_lists.rds that is used by other functions or
# procedures.
#
# This file should be run with the directory that contains this file as working
# directory.
#
# Call sequence:
#
# - docker build db/build_tools/build_db.Dockerfile runs tpm_data_lists.R



# Get %>% without loading the whole library
`%>%` <- magrittr::`%>%`



# Define input and output directory --------------------------------------------

# Helper function to get env vars
get_env_var <- function(env_var_name) {
  env_var_val <- Sys.getenv(
    env_var_name, unset = NA_character_, names = FALSE)

  # Assert env_var_val is character of length 1
  stopifnot(is.character(env_var_val))
  stopifnot(identical(length(env_var_val), 1L))

  if (is.na(env_var_val)) {
    stop(paste(
      "Error: Environment variable", env_var_name, "cannot be unset."))
  }

  return(env_var_val)
}

# Input dirs
opc_analysis_dir <- file.path(
  get_env_var("DB_HOME_DIR_PATH"), "OpenPedCan-analysis")
stopifnot(dir.exists(opc_analysis_dir))

data_dir <- file.path(opc_analysis_dir, "data")
stopifnot(dir.exists(data_dir))

# Output dir
output_dir <- get_env_var("BUILD_OUTPUT_DIR_PATH")
stopifnot(dir.exists(output_dir))



# Read and process data --------------------------------------------------------

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
    stopifnot(identical(sum(is.na(x$Kids_First_Participant_ID)), 0L))
    stopifnot(identical(sum(is.na(x$Kids_First_Biospecimen_ID)), 0L))
  }
)

stopifnot(identical(
  ncol(input_df_list$tpm_df),
  length(unique(colnames(input_df_list$tpm_df)))))

stopifnot(identical(
  sum(is.na(input_df_list$histology_df$cohort)), 0L))

stopifnot(!is.null(colnames(input_df_list$tpm_df)))
stopifnot(!is.null(rownames(input_df_list$tpm_df)))
stopifnot(identical(sum(is.na(colnames(input_df_list$tpm_df))), 0L))
stopifnot(identical(sum(is.na(rownames(input_df_list$tpm_df))), 0L))

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
  0L))
stopifnot(!is.null(input_df_list$ensg_symbol_rmtl_df$Gene_symbol))
stopifnot(identical(
  sum(is.na(input_df_list$ensg_symbol_rmtl_df$Gene_symbol)),
  0L))
stopifnot(identical(
  nrow(input_df_list$ensg_symbol_rmtl_df),
  nrow(dplyr::distinct(
    dplyr::select(
      input_df_list$ensg_symbol_rmtl_df,
      Gene_Ensembl_ID, Gene_symbol)))))

# Annotate histology df --------------------------------------------------------
# Rename columns to annotator columns
input_df_list$histology_df <- input_df_list$histology_df %>%
  dplyr::rename(
    Disease = cancer_group, GTEx_tissue_group = gtex_group,
    GTEx_tissue_subgroup = gtex_subgroup)
# annotator only when working directory is OpenPedCan-analysis or its subdir
prev_wd <- setwd(opc_analysis_dir)
source(file.path(
  "analyses", "long-format-table-utils", "annotator", "annotator-api.R"))
input_df_list$histology_df <- annotate_long_format_table(
  input_df_list$histology_df,
  columns_to_add = c(
    "EFO", "MONDO", "GTEx_tissue_group_UBERON", "GTEx_tissue_subgroup_UBERON"),
  replace_na_with_empty_string = FALSE)
# change working directory back to previous wd
setwd(prev_wd)

# Assert one Disease only maps to one EFO ID
stopifnot(identical(
  unique(
    dplyr::summarise(
      dplyr::group_by(input_df_list$histology_df, Disease),
      n = length(unique(EFO))
    )$n
  ),
  1L
))
# Assert one Disease only maps to one MONDO ID
stopifnot(identical(
  unique(
    dplyr::summarise(
      dplyr::group_by(input_df_list$histology_df, Disease),
      n = length(unique(MONDO))
    )$n
  ),
  1L)
)
# Assert one GTEx_tissue_subgroup only maps to one GTEx_tissue_subgroup_UBERON
# ID
stopifnot(identical(
  unique(
    dplyr::summarise(
      dplyr::group_by(input_df_list$histology_df, GTEx_tissue_subgroup),
      n = length(unique(GTEx_tissue_subgroup_UBERON))
    )$n
  ),
  1L)
)


# Subset independent samples ---------------------------------------------------
# Initialize tpm_data_lists
#
# - Remove primary tumor samples that have Disease as NA.
# - Remove GTEx samples that have GTEx_tissue_subgroup as NA.
#
# These two columns are used for boxplot x labels.
#
# - Remove primary tumor samples that have EFO as NA. EFO is used to subset
#   samples in API.
tpm_data_lists <- list(
  # primary tumor all-cohorts independent samples
  pt_all_cohorts = list(
    tpm_df = input_df_list$tpm_df,
    sample_subset_df = input_df_list$all_cohorts_indep_samples,
    histology_df = dplyr::filter(
      input_df_list$histology_df, !is.na(Disease), !is.na(EFO))
  ),
  # primary tumor each-cohort independent samples
  pt_each_cohort = list(
    tpm_df = input_df_list$tpm_df,
    sample_subset_df = input_df_list$each_cohort_indep_samples,
    histology_df = dplyr::filter(
      input_df_list$histology_df, !is.na(Disease), !is.na(EFO))
  ),
  # gtex all samples. sample_subset_df entry is a tibble of all gtex samples.
  gtex = list(
    tpm_df = input_df_list$tpm_df,
    sample_subset_df = dplyr::select(
      dplyr::filter(input_df_list$histology_df, cohort == "GTEx"),
      Kids_First_Participant_ID, Kids_First_Biospecimen_ID),
    histology_df = dplyr::filter(
      input_df_list$histology_df, !is.na(GTEx_tissue_subgroup))
  )
)

# Subset tpm_data_lists
tpm_data_lists <- lapply(tpm_data_lists, function(xl) {
  overlap_sids <- purrr::reduce(
    list(
      tpm_sids = colnames(xl$tpm_df),
      subset_sids = xl$sample_subset_df$Kids_First_Biospecimen_ID,
      histology_sids = xl$histology_df$Kids_First_Biospecimen_ID),
    dplyr::intersect
  )
  stopifnot(is.character(overlap_sids))
  stopifnot(identical(sum(is.na(overlap_sids)), 0L))
  stopifnot(!identical(length(overlap_sids), 0L))

  overlap_tpm_df <- xl$tpm_df[, overlap_sids]

  overlap_sample_subset_df <- xl$sample_subset_df %>%
    dplyr::filter(Kids_First_Biospecimen_ID %in% overlap_sids)

  overlap_histology_df <- xl$histology_df %>%
    dplyr::filter(Kids_First_Biospecimen_ID %in% overlap_sids) %>%
    dplyr::select(
      Kids_First_Biospecimen_ID, cohort, EFO, MONDO, Disease,
      GTEx_tissue_subgroup_UBERON, GTEx_tissue_subgroup)

  stopifnot(identical(
    sort(colnames(overlap_tpm_df)),
    sort(overlap_sids)))
  stopifnot(identical(
    sort(colnames(overlap_tpm_df)),
    sort(overlap_sample_subset_df$Kids_First_Biospecimen_ID)))
  stopifnot(identical(
    sort(colnames(overlap_tpm_df)),
    sort(overlap_histology_df$Kids_First_Biospecimen_ID)))

  # Convert overlap_tpm_df to tibble, with rownames added as a new column named
  # Gene_symbol
  overlap_tpm_tbl <- tibble::as_tibble(overlap_tpm_df, rownames = "Gene_symbol")
  stopifnot(identical(overlap_tpm_tbl$Gene_symbol, rownames(xl$tpm_df)))
  # Add ENSG IDs and RMTL
  overlap_tpm_tbl <- dplyr::left_join(
    overlap_tpm_tbl,
    input_df_list$ensg_symbol_rmtl_df,
    by = "Gene_symbol")

  stopifnot(identical(
    sort(colnames(overlap_tpm_tbl)),
    sort(c("Gene_symbol", "Gene_Ensembl_ID", "RMTL", colnames(overlap_tpm_df)))
  ))

  stopifnot(identical(
    sum(is.na(dplyr::select(overlap_tpm_tbl, -RMTL))), 0L))

  overlap_data_list <- list(
    tpm_df = overlap_tpm_tbl,
    histology_df = overlap_histology_df
  )

  return(overlap_data_list)
})

stopifnot(identical(
  sort(tpm_data_lists$gtex$tpm_df$Gene_Ensembl_ID),
  sort(tpm_data_lists$pt_all_cohorts$tpm_df$Gene_Ensembl_ID)
))

stopifnot(identical(
  sort(tpm_data_lists$gtex$tpm_df$Gene_Ensembl_ID),
  sort(tpm_data_lists$pt_each_cohort$tpm_df$Gene_Ensembl_ID)
))



# Converting tpm_data_lists to a single long plotting table takes > 30 GB RAM,
# so plotting long table is created for each plot.

cat("---------------------------------\n",
    as.character(Sys.time()), "\n",
    "Primary tumor all-cohorts independent n samples: ",
    nrow(tpm_data_lists$pt_all_cohorts$histology_df), "\n",
    "Primary tumor each-cohort independent n samples: ",
    nrow(tpm_data_lists$pt_each_cohort$histology_df), "\n",
    "GTEx all n samples: ", nrow(tpm_data_lists$gtex$histology_df), "\n",
    "Number of genes: ", nrow(tpm_data_lists$pt_all_cohorts$tpm_df),
    "\n---------------------------------\n")

# Assert tpm_data_lists is valid -----------------------------------------------
purrr::iwalk(tpm_data_lists, function(xl, xname) {
  stopifnot(identical(
    ncol(xl$tpm_df),
    length(unique(colnames(xl$tpm_df)))
  ))

  stopifnot(identical(sum(is.na(xl$histology_df$cohort)), 0L))
  stopifnot(!"all_cohorts" %in% xl$histology_df$cohort)

  if (identical(xname, "gtex")) {
    stopifnot(identical(
      sum(is.na(xl$histology_df$GTEx_tissue_subgroup)), 0L))

    stopifnot(identical(sum(!is.na(xl$histology_df$EFO)), 0L))
    stopifnot(identical(sum(!is.na(xl$histology_df$Disease)), 0L))
  } else {
    stopifnot(identical(sum(is.na(xl$histology_df$EFO)), 0L))
    stopifnot(identical(sum(is.na(xl$histology_df$Disease)), 0L))

    stopifnot(identical(
      sum(!is.na(xl$histology_df$GTEx_tissue_subgroup)), 0L))
  }
})



# Output -----------------------------------------------------------------------
saveRDS(tpm_data_lists, file.path(output_dir, "tpm_data_lists.rds"))
