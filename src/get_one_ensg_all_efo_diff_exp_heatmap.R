# get_one_ensg_all_efo_diff_exp_heatmap.R defines a function
# get_one_ensg_all_efo_diff_exp_heatmap to return a differential expression
# heatmap of one EFO ID and top differentially expressed ENSG IDs.
#
# Call sequence:
#
# - docker run calls Rscript --vanilla main.R
# - ../main.R calls source("src/get_one_ensg_all_efo_diff_exp_heatmap.R")
#
# Defined variables:
#
# - get_one_ensg_all_efo_diff_exp_heatmap



# Get a differential expression heatmap of one ENSG ID and all EFO IDs
#
# Args:
# - diff_exp_heatmap_tbl: A tibble returned by
#   get_one_efo_top_ensg_diff_exp_heatmap_tbl.
# - y_axis_scale: NOT IMPLEMENTED. A single character value of either "linear"
#   or "log10".
#
# Returns a ggplot of a differential expression heatmap of one ENSG ID and all
# EFO IDs.
get_one_ensg_all_efo_diff_exp_heatmap <- function(diff_exp_heatmap_tbl,
                                                  y_axis_scale) {
  stopifnot(is.character(y_axis_scale))
  stopifnot(identical(length(y_axis_scale), 1L))
  stopifnot(y_axis_scale %in% c("linear", "log10"))

  ensg_id <- unique(diff_exp_heatmap_tbl$Gene_Ensembl_ID)
  stopifnot(is.character(ensg_id))
  stopifnot(identical(length(ensg_id), 1L))
  stopifnot(!is.na(ensg_id))

  gene_symbol <- unique(diff_exp_heatmap_tbl$Gene_symbol)
  stopifnot(is.character(gene_symbol))
  stopifnot(identical(length(gene_symbol), 1L))
  stopifnot(!is.na(gene_symbol))

  diff_exp_heatmap_title <- glue::glue(
    "{gene_symbol} ({ensg_id})\n",
    "Differential gene expression comparing pediatric primary tumors vs ",
    "GTEx normal adult tissues"
  )

  # Set margin
  plot_margin <- ggplot2::theme_get()$plot.margin

  if (!identical(length(plot_margin), 4L)) {
    plot_margin <- rep(grid::unit(x = 5.5, units = "points"), 4)
  }

  # The x-axis labels are long and rotated 50 degrees, so they are out of the
  # plot in the default margin. Increase right margin to fit all text.
  plot_margin[2] <- grid::unit(x = 5, units = "char")

  diff_exp_heatmap <- ggplot2::ggplot(diff_exp_heatmap_tbl,
                                      ggplot2::aes(
                                        x = x_axis_label,
                                        y = y_axis_label,
                                        fill = log2_fold_change)) +
    ggplot2::geom_tile(color = "grey", size = 0.5) +
    ggplot2::scale_x_discrete(position = "bottom") +
    ggplot2::scale_y_discrete(position = "left") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.x.bottom = ggplot2::element_text(
        angle = 310, hjust = 0),
      axis.text.y.right = ggplot2::element_text(
        hjust = 1),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      plot.margin = plot_margin) +
    ggplot2::scale_fill_gradient2(
      low = rgb(0 / 255, 114 / 255, 178 / 255),
      high = rgb(230 / 255, 159 / 255, 0 / 255),
      name = "log2 gene expression\nfold change") +
    ggplot2::ggtitle(diff_exp_heatmap_title)

  return(diff_exp_heatmap)
}
