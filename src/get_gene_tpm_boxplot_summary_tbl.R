# get_gene_tpm_boxplot_summary_tbl.R defines a function
# get_gene_tpm_boxplot_summary_tbl to return a ggplot2 boxplot.
#
# Call sequence:
#
# - docker run calls Rscript --vanilla main.R
# - ../main.R calls source("src/get_gene_tpm_boxplot_summary_tbl.R")
#
# Defined variables:
#
# - get_gene_tpm_boxplot_summary_tbl



# Get a summary tibble of the TPM of a single-gene, one or more diseasees, and
# zero or more GTEx tissue(s).
#
# Args:
# - gene_tpm_boxplot_tbl: a tibble of a single-gene, one or more diseasees, and
#   zero or more GTEx tissue(s), returned by get_gene_tpm_boxplot_tbl.
#
# Returns a tibble with all NAs replaced by blank string "".
get_gene_tpm_boxplot_summary_tbl <- function(gene_tpm_boxplot_tbl) {
  uniq_x_label_vec <- unique(gene_tpm_boxplot_tbl$x_label)
  stopifnot(is.factor(uniq_x_label_vec))
  stopifnot(all(!is.na(uniq_x_label_vec)))

  # Summarise GTEx tissue subgroup descriptions or UBERON IDs
  #
  # Args:
  # - gtex_sg_vec: a character vector of GTEx tissue subgroup descriptions or
  #   UBERON IDs.
  #
  # Returns a character vector:
  #
  # - If gtex_sg_vec is empty, return an empty character vector.
  # - If gtex_sg_vec has only one unique value, NA or not, return the only
  #   unique value.
  # - If gtex_sg_vec has two or more unique values:
  #   - If unique values contain NA, remove NA, to be consistent with "NA as
  #     blank string" result rule.
  #   - Return a single character value with all unique character values
  #     collapsed with `,`.
  summarise_gtex_subgroups <- function(gtex_sg_vec) {
    stopifnot(is.character(gtex_sg_vec))

    if (length(gtex_sg_vec) == 0) {
      return(character(0))
    }

    uniq_gtex_sg_vec <- unique(gtex_sg_vec)

    if (length(uniq_gtex_sg_vec) == 1) {
      return(uniq_gtex_sg_vec)
    } else if (length(uniq_gtex_sg_vec) > 1) {
      return(paste(purrr::discard(uniq_gtex_sg_vec, is.na), collapse = ","))
    } else {
      stop("gtex_sg_vec should have >= 1 unique values.")
    }
  }

  gene_tpm_boxplot_summary_tbl <- dplyr::summarise(
    dplyr::group_by(gene_tpm_boxplot_tbl, x_label, specimen_descriptor_fill),
    Gene_Ensembl_ID = unique(Gene_Ensembl_ID),
    Gene_symbol = unique(Gene_symbol),
    PMTL = unique(PMTL),
    Dataset = unique(cohort),
    Disease = unique(Disease),
    GTEx_tissue_subgroup = summarise_gtex_subgroups(GTEx_tissue_subgroup),
    EFO = unique(EFO),
    MONDO = unique(MONDO),
    GTEx_tissue_subgroup_UBERON = summarise_gtex_subgroups(
      GTEx_tissue_subgroup_UBERON),
    TPM_mean = round(mean(TPM), digits = 2),
    TPM_sd = round(sd(TPM), digits = 2),
    TPM_min = round(min(TPM), digits = 2),
    TPM_25th_percentile = round(quantile(TPM, 0.25), digits = 2),
    TPM_median = round(median(TPM), digits = 2),
    TPM_75th_percentile = round(quantile(TPM, 0.75), digits = 2),
    TPM_max = round(max(TPM), digits = 2),
    .groups = "drop"
  )

  # Only replace NA with empty string in columns that have NA in them. This will
  # not change the value types of the columns that have no NA.
  gene_tpm_boxplot_summary_tbl <- dplyr::mutate_if(
    gene_tpm_boxplot_summary_tbl,
    function(x) sum(is.na(x)) > 0,
    function(x) tidyr::replace_na(x, replace = ""))

  return(gene_tpm_boxplot_summary_tbl)
}
