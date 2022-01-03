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
# - y_axis_scale: a single character value of either "linear" or "log10".
#
# Returns a ggplot boxplot of a single-gene, one or more diseasees, and zero or
# more GTEx tissue(s).
get_gene_tpm_boxplot <- function(gene_tpm_boxplot_tbl, y_axis_scale) {
  stopifnot(is.character(y_axis_scale))
  stopifnot(identical(length(y_axis_scale), 1L))
  stopifnot(y_axis_scale %in% c("linear", "log10"))

  ensg_id <- unique(gene_tpm_boxplot_tbl$Gene_Ensembl_ID)
  stopifnot(is.character(ensg_id))
  stopifnot(!is.na(ensg_id))
  stopifnot(identical(length(ensg_id), 1L))

  gene_symbol <- unique(gene_tpm_boxplot_tbl$Gene_symbol)
  stopifnot(is.character(gene_symbol))
  stopifnot(!is.na(gene_symbol))
  stopifnot(identical(length(gene_symbol), 1L))

  efo_id_vec <- purrr::discard(unique(gene_tpm_boxplot_tbl$EFO), is.na)
  stopifnot(is.character(efo_id_vec))
  stopifnot(length(efo_id_vec) > 0)

  gtex_subgroup_vec <- purrr::discard(
    unique(gene_tpm_boxplot_tbl$GTEx_tissue_subgroup), is.na)
  stopifnot(is.character(gtex_subgroup_vec))

  uniq_x_label_vec <- unique(gene_tpm_boxplot_tbl$x_label)
  stopifnot(is.factor(uniq_x_label_vec))
  stopifnot(all(!is.na(uniq_x_label_vec)))

  spec_desc_fill_color_vec <- c(
    "Pediatric Primary and Relapse Tumors" = "#A997DF",
    "Pediatric Primary Tumors" = "#56B4E9",
    "Pediatric Relapse Tumors" = "#E69F00",
    "GTEx Normal Adult Tissues" = "grey80")

  uniq_spec_desc_fill_vec <- unique(
    gene_tpm_boxplot_tbl$specimen_descriptor_fill)
  stopifnot(is.character(uniq_spec_desc_fill_vec))
  stopifnot(all(!is.na(uniq_spec_desc_fill_vec)))
  stopifnot(all(uniq_spec_desc_fill_vec %in% names(spec_desc_fill_color_vec)))

  # Order disease before normal
  spec_desc_fill_color_vec <- spec_desc_fill_color_vec[
    names(spec_desc_fill_color_vec) %in% uniq_spec_desc_fill_vec]

  # When there is "Pediatric Primary and Relapse Tumors", there is no "Pediatric
  # Primary Tumors" or "Pediatric Relapse Tumors"
  if (all(c("Pediatric Primary Tumors", "Pediatric Relapse Tumors") %in%
            uniq_spec_desc_fill_vec) ||
        ("Pediatric Primary and Relapse Tumors" %in% uniq_spec_desc_fill_vec)) {

    tumor_title_desc <- "Pediatric primary and relapse tumor"

  } else if ("Pediatric Primary Tumors" %in% uniq_spec_desc_fill_vec) {
    tumor_title_desc <- "Pediatric primary only tumor"

  } else if ("Pediatric Relapse Tumors" %in% uniq_spec_desc_fill_vec) {
    tumor_title_desc <- "Pediatric relapse only tumor"

  } else {
    stop(paste0(
      "Internal error: not supported unique tumor descriptors ",
      paste(uniq_spec_desc_fill_vec, collapse = ", ")))
  }

  # Set title
  if (length(gtex_subgroup_vec) > 0) {
    title <- paste0(
      gene_symbol, " (", ensg_id, ")\n",
      tumor_title_desc, " and GTEx normal adult tissue gene expression")
  } else {
    title <- paste0(
      gene_symbol, " (", ensg_id, ")\n",
      tumor_title_desc, " gene expression")
  }

  # Set y-axis label
  if (y_axis_scale == "linear") {
    y_axis_label <- "TPM"
  } else if (y_axis_scale == "log10") {
    y_axis_label <- "log10(TPM + 1)"
    gene_tpm_boxplot_tbl <- dplyr::mutate(
      gene_tpm_boxplot_tbl, TPM = log10(.data$TPM + 1))
  } else {
    stop(paste0("y_axis_scale = ", y_axis_scale, " is not implemented."))
  }

  # Set fill guide/legend
  if (length(spec_desc_fill_color_vec) > 1) {
    fill_guide <- ggplot2::guide_legend(title = "")
  } else {
    fill_guide <- "none"
  }

  # Set margin
  plot_margin <- ggplot2::theme_get()$plot.margin

  if (!identical(length(plot_margin), 4L)) {
    plot_margin <- rep(grid::unit(x = 5.5, units = "points"), 4)
  }

  # The x-axis labels are long and rotated 45 degrees, so they are out of the
  # plot in the default margin. Increase right margin to fit all text.
  plot_margin[2] <- grid::unit(x = 29, units = "char")

  gene_tpm_boxplot <- ggplot2::ggplot(gene_tpm_boxplot_tbl,
                                      ggplot2::aes(
                                        x = x_label, y = TPM,
                                        fill = specimen_descriptor_fill)) +
    ggplot2::stat_boxplot(
      geom = "errorbar", width = 0.25,
      position = ggplot2::position_dodge(0.5)) +
    ggplot2::geom_boxplot(
      lwd = 0.5, fatten = 0.7, outlier.shape = 1,
      width = 0.5, outlier.size = 1,
      position = ggplot2::position_dodge(0.5)) +
    ggplot2::ylab(y_axis_label) +
    ggplot2::xlab("") +
    ggplot2_boxplot_theme(base_size = 12) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = -45, vjust = 1, hjust = 0),
      plot.margin = plot_margin,
      legend.position = "top", legend.justification = "left",
      legend.box.just = "left",
      legend.box.margin = ggplot2::margin(-0.6, 0, -1, 0, unit = "line")) +
    ggplot2::ggtitle(title) +
    ggplot2::scale_fill_manual(values = spec_desc_fill_color_vec) +
    ggplot2::guides(fill = fill_guide)

  return(gene_tpm_boxplot)
}
