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
#   more GTEx tissue(s), returned by get_gene_tpm_tbl, with an additional column
#   named box_group. The box_group column is used to agglomerate rows into
#   boxes.
# - gtex_box_group: a single character value with the following choices:
#   - "tissue_subgroup": Default. Use GTEx_tissue_subgroup column as box group.
#   - "collapse": Collapse all GTEx samples into one box group with the
#     following name, "All tissue subgroups".
# - min_n_per_sample_group: a single numeric value of the minimum number of
#   samples per box. Default is 1.
#
# Returns a tibble for generating a ggplot boxplot of a single-gene, one or more
# diseasees, and zero or more GTEx tissue(s).
get_gene_tpm_boxplot_tbl <- function(gene_tpm_tbl,
                                     gtex_box_group = "tissue_subgroup",
                                     min_n_per_sample_group = 1L) {
  stopifnot(is.character(gtex_box_group))
  stopifnot(identical(length(gtex_box_group), 1L))
  stopifnot(gtex_box_group %in% c("tissue_subgroup", "collapse"))

  stopifnot(is.numeric(min_n_per_sample_group))
  stopifnot(identical(length(min_n_per_sample_group), 1L))
  stopifnot(!is.na(min_n_per_sample_group))

  # Handle GTEx tissue. Separate or collapse.
  if (gtex_box_group == "tissue_subgroup") {
    gene_tpm_tbl <- dplyr::mutate(
      gene_tpm_tbl,
      box_group = dplyr::if_else(
        is.na(Disease), true = GTEx_tissue_subgroup, false = Disease))
  } else if (gtex_box_group == "collapse") {
    gene_tpm_tbl <- dplyr::mutate(
      gene_tpm_tbl,
      box_group = dplyr::if_else(
        is.na(Disease), true = "All tissue subgroups", false = Disease))
  } else {
    stop(paste0("Not implemented gtex_box_group option ", gtex_box_group))
  }

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

  # Add boxplot x-labels and sample counts within each x-label.
  gene_tpm_boxplot_tbl <- dplyr::add_count(
    gene_tpm_tbl, cohort, box_group, name = "x_label_sample_count")

  gene_tpm_boxplot_tbl <- dplyr::mutate(
    gene_tpm_boxplot_tbl,
    x_labels = paste0(
      box_group, " (Dataset = ", cohort, ", N = ", x_label_sample_count, ")"))

  # Filter boxplot x-labels by x_label_sample_count
  gene_tpm_boxplot_tbl <- dplyr::filter(
    gene_tpm_boxplot_tbl,
    .data$x_label_sample_count >= .env$min_n_per_sample_group)
  # Raise error if there is no disease sample
  stopifnot(!all(is.na(gene_tpm_boxplot_tbl$Disease)))

  # If is.na(Disease), sample_type is normal. If !is.na(Disease), sample_type is
  # disease.
  gene_tpm_boxplot_tbl <- dplyr::mutate(
    gene_tpm_boxplot_tbl,
    sample_type = dplyr::if_else(
      is.na(Disease), true = "normal", false = "disease"))

  # Order x-labels
  xlabel_levels <- dplyr::arrange(
    dplyr::distinct(
      dplyr::select(
        gene_tpm_boxplot_tbl, sample_type, box_group, cohort, x_labels),
      x_labels, .keep_all = TRUE),
    sample_type, box_group, cohort)$x_labels

  gene_tpm_boxplot_tbl$x_labels <- factor(
    gene_tpm_boxplot_tbl$x_labels, levels = xlabel_levels)

  return(gene_tpm_boxplot_tbl)
}
