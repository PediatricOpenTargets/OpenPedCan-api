# add_gene_tpm_box_group.R defines a function add_gene_tpm_box_group to
# return a tibble for generating ggplot2 boxplot.
#
# Call sequence:
#
# - docker run calls Rscript --vanilla main.R
# - ../main.R calls source("src/add_gene_tpm_box_group.R")
#
# Defined variables:
#
# - add_gene_tpm_box_group



# Add box_group column to a gene_tpm_tbl
#
# Args:
# - gene_tpm_tbl: a tibble returned by get_gene_tpm_tbl.
# - gtex_box_group: a single character value with the following choices:
#   - "tissue_subgroup": Default. Use GTEx_tissue_subgroup column as box group.
#   - "collapse": Collapse all GTEx samples into one box group with the
#     following name, "All tissue subgroups".
#
# Returns a tibble.
add_gene_tpm_box_group <- function(gene_tpm_tbl,
                                   gtex_box_group = "tissue_subgroup") {
  stopifnot(is.character(gtex_box_group))
  stopifnot(identical(length(gtex_box_group), 1L))
  stopifnot(gtex_box_group %in% c("tissue_subgroup", "collapse"))

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

  return(gene_tpm_tbl)
}
