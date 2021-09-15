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
#
# Returns a tibble.
add_gene_tpm_box_group <- function(gene_tpm_tbl) {
  gene_tpm_tbl <- dplyr::mutate(
    gene_tpm_tbl,
    box_group = dplyr::if_else(
      is.na(Disease), true = GTEx_tissue_subgroup, false = Disease))
  return(gene_tpm_tbl)
}
