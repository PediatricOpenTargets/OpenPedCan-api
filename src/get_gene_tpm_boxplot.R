# get_gene_tpm_boxplot.R defines a function get_gene_tpm_boxplot to return a
# ggplot2 boxplot.
#
# Call sequence:
#
# - docker run calls Rscript --vanilla main.R
# - ../main.R calls source("src/get_gene_tpm_boxplot.R")
#
# Defined variables:
#
# - get_gene_tpm_boxplot



# Get a ggplot boxplot of a single-gene, one or more diseasees, and zero or more
# GTEx tissue(s).
#
# Args:
# - gene_tpm_boxplot_tbl: a tibble of a single-gene, one or more diseasees, and
#   zero or more GTEx tissue(s), returned by get_gene_tpm_boxplot_tbl.
#
# Returns a ggplot boxplot of a single-gene, one or more diseasees, and zero or
# more GTEx tissue(s).
get_gene_tpm_boxplot <- function(gene_tpm_boxplot_tbl) {
  ensg_id <- unique(gene_tpm_boxplot_tbl$Gene_Ensembl_ID)
  stopifnot(is.character(ensg_id))
  stopifnot(!is.na(ensg_id))
  stopifnot(identical(length(ensg_id), 1L))

  gene_symbol <- unique(gene_tpm_boxplot_tbl$Gene_symbol)
  stopifnot(is.character(gene_symbol))
  stopifnot(!is.na(gene_symbol))
  stopifnot(identical(length(gene_symbol), 1L))

  uniq_x_label_vec <- unique(gene_tpm_boxplot_tbl$x_labels)
  stopifnot(is.factor(uniq_x_label_vec))
  stopifnot(all(!is.na(uniq_x_label_vec)))

  uniq_sample_type_vec <- unique(gene_tpm_boxplot_tbl$sample_type)
  stopifnot(is.character(uniq_sample_type_vec))
  stopifnot(all(uniq_sample_type_vec %in% c("disease", "normal")))

  efo_id_vec <- purrr::discard(unique(gene_tpm_boxplot_tbl$EFO), is.na)
  stopifnot(is.character(efo_id_vec))
  stopifnot(length(efo_id_vec) > 0)

  gtex_subgroup_vec <- purrr::discard(
    unique(gene_tpm_boxplot_tbl$GTEx_tissue_subgroup), is.na)
  stopifnot(is.character(gtex_subgroup_vec))

  if (length(gtex_subgroup_vec) > 0) {
    title <- paste(
      paste0(gene_symbol, " (", ensg_id, ")"),
      "Disease vs. GTEx tissue bulk gene expression",
      sep = "\n")
  } else {
    title <- paste(
      paste0(gene_symbol, " (", ensg_id, ")"),
      "Disease tissue bulk gene expression",
      sep = "\n")
  }

  if (identical(length(uniq_sample_type_vec), 1L)) {
    # Only one sample type.
    #
    # Use grey for either one.
    box_fill_colors = c("disease" = "grey80", "normal" = "grey80")
  } else {
    # More than one saple types.
    #
    # Use red for diease and grey for normal.
    box_fill_colors = c("disease" = "red3", "normal" = "grey80")
  }

  gene_tpm_boxplot <- ggplot2::ggplot(gene_tpm_boxplot_tbl,
                                      ggplot2::aes(x = x_labels, y = TPM,
                                                   fill = sample_type)) +
    ggplot2::stat_boxplot(geom = "errorbar", width = 0.2) +
    ggplot2::geom_boxplot(lwd = 0.5, fatten = 0.7, outlier.shape = 1,
                          width = 0.5, outlier.size = 1) +
    ggplot2::ylab("TPM") +
    ggplot2::xlab("") +
    ggplot2_publication_theme(base_size = 12) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90,
                                                       vjust = 0.5,
                                                       hjust = 1)) +
    ggplot2::ggtitle(title) +
    ggplot2::scale_fill_manual(values = box_fill_colors) +
    ggplot2::guides(fill = "none")

  return(gene_tpm_boxplot)
}
