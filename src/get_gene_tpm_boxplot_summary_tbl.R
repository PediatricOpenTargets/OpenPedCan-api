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
  uniq_x_label_vec <- unique(gene_tpm_boxplot_tbl$x_labels)
  stopifnot(is.factor(uniq_x_label_vec))
  stopifnot(all(!is.na(uniq_x_label_vec)))

  gene_tpm_boxplot_summary_tbl <- dplyr::summarise(
    dplyr::group_by(gene_tpm_boxplot_tbl, x_labels),
    Gene_Ensembl_ID = unique(Gene_Ensembl_ID),
    Gene_symbol = unique(Gene_symbol),
    PMTL = unique(RMTL),
    Dataset = unique(cohort),
    Disease = unique(Disease),
    GTEx_tissue_subgroup = unique(GTEx_tissue_subgroup),
    EFO = unique(EFO),
    MONDO = unique(MONDO),
    GTEx_tissue_subgroup_UBERON = unique(GTEx_tissue_subgroup_UBERON),
    TPM_mean = round(mean(TPM), digits = 2),
    TPM_sd = round(sd(TPM), digits = 2),
    TPM_min = round(min(TPM), digits = 2),
    TPM_25th_percentile = round(quantile(TPM, 0.25), digits = 2),
    TPM_median = round(median(TPM), digits = 2),
    TPM_75th_percentile = round(quantile(TPM, 0.75), digits = 2),
    TPM_max = round(max(TPM), digits = 2)
  )

  # Only replace NA with empty string in columns that have NA in them. This will
  # not change the value types of the columns that have no NA.
  gene_tpm_boxplot_summary_tbl <- dplyr::mutate_if(
    gene_tpm_boxplot_summary_tbl,
    function(x) sum(is.na(x)) > 0,
    function(x) tidyr::replace_na(x, replace = ""))

  return(gene_tpm_boxplot_summary_tbl)
}
