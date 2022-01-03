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


# Define functions -------------------------------------------------------------
# Get %>% without loading the whole library
`%>%` <- magrittr::`%>%`

# specimen_descriptor values are "Primary Tumor", "Relapse Tumor", and "GTEx
# Normal". Changing or adding specimen_descriptor value(s) require reviewing
# entire code base, because they are used in both database building and API
# code.

# Combine primary and relapse independent sample data frame
#
# Args:
# - prm_indep_sdf: primary independent specimen data frame.
# - rlp_indep_sdf: primary independent specimen data frame.
#
# Returns a tibble of combined primary and relapse independent specimen data
# frame.
#
# TODO: Unit test for empty tibbles. Should work.
combine_prm_rlp_indep_sdf <- function(prm_indep_sdf, rlp_indep_sdf) {
  stopifnot(tibble::is_tibble(prm_indep_sdf))
  stopifnot(tibble::is_tibble(rlp_indep_sdf))

  stopifnot(identical(
    colnames(prm_indep_sdf),
    c("Kids_First_Participant_ID", "Kids_First_Biospecimen_ID")
  ))

  stopifnot(identical(
    colnames(rlp_indep_sdf),
    c("Kids_First_Participant_ID", "Kids_First_Biospecimen_ID")
  ))

  # Assert no overlapping between primary and relapse specimen IDs.
  stopifnot(identical(
    length(dplyr::intersect(
      prm_indep_sdf$Kids_First_Biospecimen_ID,
      rlp_indep_sdf$Kids_First_Biospecimen_ID)),
    0L
  ))

  # The exact specimen_descriptor values are used in multple places. Changing
  # any of them needs to completely review code base.
  prm_indep_sdf <- dplyr::mutate(
    prm_indep_sdf, specimen_descriptor = "Primary Tumor")

  rlp_indep_sdf <- dplyr::mutate(
    rlp_indep_sdf, specimen_descriptor = "Relapse Tumor")

  stopifnot(identical(
    colnames(prm_indep_sdf),
    c("Kids_First_Participant_ID", "Kids_First_Biospecimen_ID",
      "specimen_descriptor")
  ))

  stopifnot(identical(
    colnames(rlp_indep_sdf),
    c("Kids_First_Participant_ID", "Kids_First_Biospecimen_ID",
      "specimen_descriptor")
  ))

  prm_rlp_indep_sdf <- dplyr::bind_rows(prm_indep_sdf, rlp_indep_sdf)

  stopifnot(identical(
    colnames(prm_rlp_indep_sdf),
    c("Kids_First_Participant_ID", "Kids_First_Biospecimen_ID",
      "specimen_descriptor")
  ))

  return(prm_rlp_indep_sdf)
}


# Format specimen descriptor counts for printing
#
# Args:
# - spec_desc_vec: a character vector of specimen descriptor.
#
# Return a single character value.
format_spec_desc_counts <- function(spec_desc_vec) {
  stopifnot(is.character(spec_desc_vec))
  stopifnot(all(!is.na(spec_desc_vec)))

  spec_desc_cnt_tbl <- tibble::tibble(spec_desc = spec_desc_vec) %>%
    dplyr::count(spec_desc, name = "n")

  fmt_spec_desc_cnt_chr <- paste(
    paste0(
      "  - ",
      paste(spec_desc_cnt_tbl$spec_desc, spec_desc_cnt_tbl$n, sep = ": ")),
    collapse = "\n")

  return(fmt_spec_desc_cnt_chr)
}



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
  primary_all_cohorts_indep_samples = readr::read_tsv(
    file.path(data_dir, "independent-specimens.rnaseq.primary.tsv"),
    col_types = readr::cols()),
  primary_each_cohort_indep_samples = readr::read_tsv(
    file.path(data_dir, "independent-specimens.rnaseq.primary.eachcohort.tsv"),
    col_types = readr::cols()),
  relapse_all_cohorts_indep_samples = readr::read_tsv(
    file.path(data_dir, "independent-specimens.rnaseq.relapse.tsv"),
    col_types = readr::cols()),
  relapse_each_cohort_indep_samples = readr::read_tsv(
    file.path(data_dir, "independent-specimens.rnaseq.relapse.eachcohort.tsv"),
    col_types = readr::cols()),
  tpm_df = readRDS(
    file.path(data_dir, "gene-expression-rsem-tpm-collapsed.rds")),
  ensg_symbol_pmtl_df = readr::read_tsv(
    file.path(data_dir, "ensg-hugo-pmtl-mapping.tsv"),
    col_types = readr::cols())
)

purrr::walk(
  input_df_list[c("histology_df",
                  "primary_all_cohorts_indep_samples",
                  "primary_each_cohort_indep_samples",
                  "relapse_all_cohorts_indep_samples",
                  "relapse_each_cohort_indep_samples")],
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
  is.na(input_df_list$ensg_symbol_pmtl_df$pmtl),
  is.na(input_df_list$ensg_symbol_pmtl_df$version)))

# Remove ensg_id Symbol_Not_Found, which has NA gene_symbol
input_df_list$ensg_symbol_pmtl_df <- input_df_list$ensg_symbol_pmtl_df %>%
  dplyr::mutate(
    PMTL = dplyr::if_else(
      is.na(pmtl), true = NA_character_,
      false = paste0(pmtl, " (", version, ")"))) %>%
  dplyr::select(ensg_id, gene_symbol, PMTL) %>%
  dplyr::filter(ensg_id != "Symbol_Not_Found") %>%
  dplyr::rename(Gene_Ensembl_ID = ensg_id, Gene_symbol = gene_symbol) %>%
  dplyr::distinct()

stopifnot(!is.null(input_df_list$ensg_symbol_pmtl_df$Gene_Ensembl_ID))
stopifnot(identical(
  sum(is.na(input_df_list$ensg_symbol_pmtl_df$Gene_Ensembl_ID)),
  0L))
stopifnot(!is.null(input_df_list$ensg_symbol_pmtl_df$Gene_symbol))
stopifnot(identical(
  sum(is.na(input_df_list$ensg_symbol_pmtl_df$Gene_symbol)),
  0L))
stopifnot(identical(
  nrow(input_df_list$ensg_symbol_pmtl_df),
  nrow(dplyr::distinct(
    dplyr::select(
      input_df_list$ensg_symbol_pmtl_df,
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
# - Combine primary and relapse independent sample lists. Before combining, add
#   "Primary Tumor" and "Relapse Tumor" to independent sample lists.
# - Add "GTEx Normal" to all GTEx sample list.
# - Subset samples.

sample_subset_df_list <- list(
  gtex = dplyr::filter(input_df_list$histology_df, cohort == "GTEx") %>%
    dplyr::select(Kids_First_Participant_ID, Kids_First_Biospecimen_ID) %>%
    dplyr::mutate(specimen_descriptor = "GTEx Normal") %>%
    dplyr::select(Kids_First_Participant_ID, Kids_First_Biospecimen_ID,
                  specimen_descriptor),

  prm_rlp_all_cohorts_indep = combine_prm_rlp_indep_sdf(
    input_df_list$primary_all_cohorts_indep_samples,
    input_df_list$relapse_all_cohorts_indep_samples),

  prm_rlp_each_cohort_indep = combine_prm_rlp_indep_sdf(
    input_df_list$primary_each_cohort_indep_samples,
    input_df_list$relapse_each_cohort_indep_samples)
)


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
  # primary and relapse tumor all-cohorts independent samples
  prm_rlp_all_cohorts = list(
    tpm_df = input_df_list$tpm_df,
    sample_subset_df = sample_subset_df_list$prm_rlp_all_cohorts_indep,
    histology_df = dplyr::filter(
      input_df_list$histology_df, !is.na(Disease), !is.na(EFO))
  ),
  # primary and relapse tumor each-cohort independent samples
  prm_rlp_each_cohort = list(
    tpm_df = input_df_list$tpm_df,
    sample_subset_df = sample_subset_df_list$prm_rlp_each_cohort_indep,
    histology_df = dplyr::filter(
      input_df_list$histology_df, !is.na(Disease), !is.na(EFO))
  ),
  # gtex all samples. sample_subset_df entry is a tibble of all gtex samples.
  gtex = list(
    tpm_df = input_df_list$tpm_df,
    sample_subset_df = sample_subset_df_list$gtex,
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
      GTEx_tissue_subgroup_UBERON, GTEx_tissue_subgroup) %>%
    dplyr::left_join(
      dplyr::select(
        overlap_sample_subset_df, Kids_First_Biospecimen_ID,
        specimen_descriptor),
      by = "Kids_First_Biospecimen_ID")

  # overlap_sids is asserted above to have no NA
  stopifnot(identical(
    sort(overlap_sids),
    sort(colnames(overlap_tpm_df), na.last = TRUE),
  ))

  stopifnot(identical(
    sort(overlap_sids),
    sort(overlap_sample_subset_df$Kids_First_Biospecimen_ID, na.last = TRUE)
  ))

  stopifnot(identical(
    sort(overlap_sids),
    sort(overlap_histology_df$Kids_First_Biospecimen_ID, na.last = TRUE)
  ))

  # Convert overlap_tpm_df to tibble, with rownames added as a new column named
  # Gene_symbol
  overlap_tpm_tbl <- tibble::as_tibble(overlap_tpm_df, rownames = "Gene_symbol")
  stopifnot(identical(overlap_tpm_tbl$Gene_symbol, rownames(xl$tpm_df)))
  # Add ENSG IDs and PMTL
  overlap_tpm_tbl <- dplyr::left_join(
    overlap_tpm_tbl,
    input_df_list$ensg_symbol_pmtl_df,
    by = "Gene_symbol")

  stopifnot(identical(
    sort(colnames(overlap_tpm_tbl)),
    sort(c("Gene_symbol", "Gene_Ensembl_ID", "PMTL", colnames(overlap_tpm_df)))
  ))

  stopifnot(identical(
    sum(is.na(dplyr::select(overlap_tpm_tbl, -PMTL))), 0L))

  overlap_data_list <- list(
    tpm_df = overlap_tpm_tbl,
    histology_df = overlap_histology_df,
    sample_subset_df = overlap_sample_subset_df
  )

  return(overlap_data_list)
})

stopifnot(identical(
  sort(tpm_data_lists$gtex$tpm_df$Gene_Ensembl_ID),
  sort(tpm_data_lists$prm_rlp_all_cohorts$tpm_df$Gene_Ensembl_ID)
))

stopifnot(identical(
  sort(tpm_data_lists$gtex$tpm_df$Gene_Ensembl_ID),
  sort(tpm_data_lists$prm_rlp_each_cohort$tpm_df$Gene_Ensembl_ID)
))

cat("---------------------------------\n",
    as.character(Sys.time()), "\n",
    "All-cohorts independent n tumor samples:\n",
    format_spec_desc_counts(
      tpm_data_lists$prm_rlp_all_cohorts$histology_df$specimen_descriptor),
    "\n",
    "Each-cohort independent n tumor samples:\n",
    format_spec_desc_counts(
      tpm_data_lists$prm_rlp_each_cohort$histology_df$specimen_descriptor),
    "\n",
    "GTEx all n samples: ", nrow(tpm_data_lists$gtex$histology_df), "\n",
    "Number of genes: ", nrow(tpm_data_lists$prm_rlp_all_cohorts$tpm_df),
    "\n---------------------------------\n", sep = "")

# Assert tpm_data_lists is valid -----------------------------------------------
all_cohorts_str_id <- "All Cohorts"

purrr::iwalk(tpm_data_lists, function(xl, xname) {
  stopifnot(identical(
    ncol(xl$tpm_df),
    length(unique(colnames(xl$tpm_df)))
  ))

  stopifnot(identical(sum(is.na(xl$histology_df$cohort)), 0L))
  stopifnot(!all_cohorts_str_id %in% xl$histology_df$cohort)

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
