# get_one_efo_top_ensg_diff_exp_heatmap.R defines a function
# get_one_efo_top_ensg_diff_exp_heatmap to return a differential expression
# heatmap of one EFO ID and top differentially expressed ENSG IDs.
#
# Call sequence:
#
# - docker run calls Rscript --vanilla main.R
# - ../main.R calls source("src/get_one_efo_top_ensg_diff_exp_heatmap.R")
#
# Defined variables:
#
# - get_one_efo_top_ensg_diff_exp_heatmap



# Get a differential expression heatmap of one EFO ID and top differentially
# expressed ENSG IDs
#
# Args:
# - diff_exp_heatmap_tbl: A tibble returned by
#   get_one_efo_top_ensg_diff_exp_heatmap_tbl.
# - y_axis_scale: A single character value of either "linear" or "log10".
#
# Returns a ggplot of a differential expression heatmap of one EFO ID and top
# differentially expressed ENSG IDs
get_one_efo_top_ensg_diff_exp_heatmap <- function(diff_exp_heatmap_tbl,
                                                  y_axis_scale) {

  stopifnot(is.character(y_axis_scale))
  stopifnot(identical(length(y_axis_scale), 1L))
  stopifnot(y_axis_scale %in% c("linear", "log10"))

  rank_genes_by <- unique(diff_exp_heatmap_tbl$rank_genes_by)
  stopifnot(is.character(rank_genes_by))
  stopifnot(identical(length(rank_genes_by), 1L))
  stopifnot(!is.na(rank_genes_by))

  efo_id <- unique(diff_exp_heatmap_tbl$EFO)
  stopifnot(is.character(efo_id))
  stopifnot(identical(length(efo_id), 1L))
  stopifnot(!is.na(efo_id))

  diff_exp_heatmap_title_disease <- tolower(paste0(
    unique(diff_exp_heatmap_tbl$Disease), collapse = " and "))

  diff_exp_heatmap_title_disease_specimen <- tolower(paste0(
    unique(diff_exp_heatmap_tbl$Disease_specimen_descriptor),
    collapse = " and "))

  diff_exp_heatmap_title_rank_genes_by <- list(
    cgc_all_gene_up_reg_rank = "Top upregulated genes",
    cgc_all_gene_down_reg_rank = "Top downregulated genes",
    cgc_all_gene_up_and_down_reg_rank = "Top differentially expressed genes",
    cgc_pmtl_gene_up_reg_rank = "Top upregulated PMTL genes",
    cgc_pmtl_gene_down_reg_rank = "Top downregulated PMTL genes",
    cgc_pmtl_gene_up_and_down_reg_rank =
      "Top differentially expressed PMTL genes"
  )[[rank_genes_by]]

  stopifnot(is.character(diff_exp_heatmap_title_rank_genes_by))

  diff_exp_heatmap_title <- glue::glue(
    "{diff_exp_heatmap_title_rank_genes_by} comparing ",
    "pediatric ",
    "{diff_exp_heatmap_title_disease_specimen}s ",
    "vs GTEx normal adult tissues"
  )

  # Set margin
  plot_margin <- ggplot2::theme_get()$plot.margin

  if (!identical(length(plot_margin), 4L)) {
    plot_margin <- rep(grid::unit(x = 5.5, units = "points"), 4)
  }

  # The x-axis labels are long and rotated 50 degrees, so they are out of the
  # plot in the default margin. Increase right margin to fit all text.
  plot_margin[2] <- grid::unit(x = 12, units = "char")

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
