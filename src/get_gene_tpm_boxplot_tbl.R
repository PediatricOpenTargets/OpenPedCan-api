# get_gene_tpm_boxplot_tbl.R defines a function get_gene_tpm_boxplot_tbl to
# return a tibble for generating ggplot2 boxplot.
#
# Call sequence:
#
# - docker run calls Rscript --vanilla main.R
# - ../main.R calls source("src/get_gene_tpm_boxplot_tbl.R")
#
# Defined variables:
#
# - get_gene_tpm_boxplot_tbl

# Get a tibble for generating a ggplot boxplot of a single-gene, one or more
# diseasees, and zero or more GTEx tissue(s).
#
# Args:
# - gene_tpm_tbl: a tibble of a single-gene, one or more diseasees, and zero or
#   more GTEx tissue(s), returned by get_gene_tpm_tbl. This tibble must have a
#   column named box_group. The box_group column is used to agglomerate rows
#   into boxes.
#
# Returns a tibble for generating a ggplot boxplot of a single-gene, one or more
# diseasees, and zero or more GTEx tissue(s).
get_gene_tpm_boxplot_tbl <- function(gene_tpm_tbl) {
  if (DEBUG) {
    stopifnot(identical(sum(is.na(gene_tpm_tbl$cohort)), 0L))
    stopifnot(identical(sum(is.na(gene_tpm_tbl$box_group)), 0L))
    # xor evaluates to TRUE if two values are different
    stopifnot(identical(
      sum(!xor(
        is.na(gene_tpm_tbl$Disease),
        is.na(gene_tpm_tbl$GTEx_tissue_subgroup))),
      0L))
  }
  # add cohort box_group count
  gene_tpm_boxplot_tbl <- dplyr::add_count(
    gene_tpm_tbl, cohort, box_group, name = "cohort_box_group_n")

  gene_tpm_boxplot_tbl <- dplyr::mutate(
    gene_tpm_boxplot_tbl,
    x_labels = paste0(
      cohort, " ", box_group, " (N = ", cohort_box_group_n, ")"))

  # If is.na(EFO), sample_type is normal. If !is.na(EFO), sample_type is
  # disease.
  gene_tpm_boxplot_tbl <- dplyr::mutate(
    gene_tpm_boxplot_tbl,
    sample_type = dplyr::if_else(
      is.na(EFO), true = "normal", false = "disease"))

  xlabel_factor <- dplyr::arrange(
    dplyr::distinct(
      dplyr::select(gene_tpm_boxplot_tbl, sample_type, box_group, x_labels)),
    sample_type, box_group)$x_labels

  gene_tpm_boxplot_tbl$x_labels <- factor(
    gene_tpm_boxplot_tbl$x_labels, levels = xlabel_factor)

  return(gene_tpm_boxplot_tbl)
}
