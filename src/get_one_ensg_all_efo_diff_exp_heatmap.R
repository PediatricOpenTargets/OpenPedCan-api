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
# - include_boxplot: A single character value of either "true" or "false". Note:
#   logical TRUE or FALSE is not used in this parameter by design, to simplify
#   function calling procedure from HTTP URL. Character "true" or "false" can
#   also be extended to specify how boxplots should be generated.
# - boxplot_y_axis_scale: A single character value of either "linear" or
#   "log10".
#
# Returns a ggplot of a differential expression heatmap of one ENSG ID and all
# EFO IDs.
get_one_ensg_all_efo_diff_exp_heatmap <- function(diff_exp_heatmap_tbl,
                                                  include_boxplot,
                                                  boxplot_y_axis_scale) {
  stopifnot(is.character(boxplot_y_axis_scale))
  stopifnot(identical(length(boxplot_y_axis_scale), 1L))
  stopifnot(boxplot_y_axis_scale %in% c("linear", "log10"))

  stopifnot(is.character(include_boxplot))
  stopifnot(identical(length(include_boxplot), 1L))
  stopifnot(include_boxplot %in% c("true", "false"))

  deh_ensg_id <- unique(diff_exp_heatmap_tbl$Gene_Ensembl_ID)
  stopifnot(is.character(deh_ensg_id))
  stopifnot(identical(length(deh_ensg_id), 1L))
  stopifnot(!is.na(deh_ensg_id))

  deh_gene_symbol <- unique(diff_exp_heatmap_tbl$Gene_symbol)
  stopifnot(is.character(deh_gene_symbol))
  stopifnot(identical(length(deh_gene_symbol), 1L))
  stopifnot(!is.na(deh_gene_symbol))

  diff_exp_heatmap_title <- glue::glue(
    "{deh_gene_symbol} ({deh_ensg_id})\n",
    "Differential gene expression comparing pediatric primary tumors vs ",
    "GTEx normal adult tissues"
  )

  custom_blue <- rgb(0 / 255, 114 / 255, 178 / 255)
  custom_orange <- rgb(230 / 255, 159 / 255, 0 / 255)

  diff_exp_heatmap <- ggplot2::ggplot(diff_exp_heatmap_tbl,
                                      ggplot2::aes(
                                        x = x_axis_label,
                                        y = y_axis_label,
                                        fill = log2_fold_change)) +
    ggplot2::geom_tile(color = "grey", size = 0.5) +
    ggplot2::theme_minimal() +
    ggplot2::scale_fill_gradient2(
      low = custom_blue,
      high = custom_orange,
      name = "log2 gene expression\nfold change")

  if (include_boxplot == "false") {
    # Set margin
    plot_margin <- ggplot2::theme_get()$plot.margin

    if (!identical(length(plot_margin), 4L)) {
      plot_margin <- rep(grid::unit(x = 5.5, units = "points"), 4)
    }

    # The x-axis labels are long and rotated 50 degrees, so they are out of the
    # plot in the default margin. Increase right margin to fit all text.
    plot_margin[2] <- grid::unit(x = 5, units = "char")

    diff_exp_heatmap <- diff_exp_heatmap +
      ggplot2::scale_x_discrete(position = "bottom") +
      ggplot2::scale_y_discrete(position = "left") +
      ggplot2::theme(
        panel.grid = ggplot2::element_blank(),
        axis.text.x.bottom = ggplot2::element_text(
          angle = 310, hjust = 0),
        axis.text.y.right = ggplot2::element_text(
          hjust = 1),
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        plot.margin = plot_margin) +
      ggplot2::ggtitle(diff_exp_heatmap_title)

    res_plot <- diff_exp_heatmap

  } else if (include_boxplot == "true") {
    # jscpd:ignore-start

    # TODO: Extract the same plotting procedures that are used in
    # get_one_efo_top_ensg_diff_exp_heatmap.R. The extracted helper functions
    # need to avoid name collisions, whih require function names that are
    # distinct and unique.
    diff_exp_heatmap <- diff_exp_heatmap +
      ggplot2::theme(
        panel.grid = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        legend.position = "none",
        plot.margin = grid::unit(c(0, 0, 0, 0), "cm"))

    diff_exp_heatmap_x_text <- ggplot2::ggplotGrob(
      diff_exp_heatmap +
        ggplot2::scale_x_discrete(position = "top") +
        ggplot2::theme(
          axis.text.x.top = ggplot2::element_text(
            angle = 310, hjust = 0, vjust = 1)
        )
    )

    diff_exp_heatmap_y_text <- ggplot2::ggplotGrob(
      diff_exp_heatmap +
        ggplot2::scale_y_discrete(position = "right") +
        ggplot2::theme(
          axis.text.y.right = ggplot2::element_text(
            hjust = 1
          )
        )
    )

    # Use trim depending on need.
    diff_exp_heatmap_y_text <- gtable::gtable_filter(
      diff_exp_heatmap_y_text, "axis-r|ylab", trim = FALSE)
    diff_exp_heatmap_x_text <- gtable::gtable_filter(
      diff_exp_heatmap_x_text, "axis-t|xlab", trim = FALSE)

    diff_exp_heatmap_legend <- ggpubr::as_ggplot(
      ggpubr::get_legend(
        diff_exp_heatmap + ggplot2::theme(legend.position = "left")))


    # Generate boxplots on the top and right sides.
    tpm_tbl <- get_gene_tpm_tbl(
      deh_ensg_id, gtex_sample_group = "require",
      relapse_sample_group = "require", tcga_sample_group = "exclude",
      efo_id = NULL, gene_symbol = deh_gene_symbol)

    stopifnot(identical(unique(tpm_tbl$Gene_Ensembl_ID), deh_ensg_id))
    stopifnot(identical(unique(tpm_tbl$Gene_symbol), deh_gene_symbol))

    if (boxplot_y_axis_scale == "linear") {
      tpm_tbl <- dplyr::mutate(
        tpm_tbl, y_val = .data$TPM)  # nolint: object_usage_linter

      boxplot_y_title <- "TPM"
    } else if (boxplot_y_axis_scale == "log10") {
      tpm_tbl <- dplyr::mutate(
        tpm_tbl, y_val = log10(.data$TPM + 1))  # nolint: object_usage_linter

      boxplot_y_title <- "log10(TPM + 1)"
    } else {
      stop(paste("Unknown boxplot_y_axis_scale", boxplot_y_axis_scale))
    }

    # Add 0.1 in the y_val vector to handle all-zero case. If
    # boxplot_y_val_max is 0, ylim(0, 0) will generate a plot with (-0.0X,
    # 0.0X) x and y scales. Then, boxplot label texts will be put to the center
    # rather than close to axis tick texts.
    boxplot_y_val_max <- max(c(tpm_tbl$y_val, 0.001), na.rm = TRUE)

    combined_plot_title <- grid::textGrob(
      glue::glue(
        "{diff_exp_heatmap_title}"
      ),
      x = 0, just = "left"
    )

    # Right-side boxplot
    deh_uniq_y_axis_lab_tbl <- dplyr::distinct(
      dplyr::select(
        dplyr::mutate(
          diff_exp_heatmap_tbl,
          chr_y_axis_label = as.character(y_axis_label)),
        Disease, cohort, Disease_specimen_descriptor, Disease_sample_count,
        chr_y_axis_label
      )
    )

    stopifnot(nrow(deh_uniq_y_axis_lab_tbl) > 0)
    stopifnot(identical(
      length(unique(deh_uniq_y_axis_lab_tbl$chr_y_axis_label)),
      nrow(deh_uniq_y_axis_lab_tbl)
    ))
    stopifnot(identical(sum(is.na(deh_uniq_y_axis_lab_tbl)), 0L))

    right_boxplot_tbl <- dplyr::group_modify(
      dplyr::group_by(deh_uniq_y_axis_lab_tbl, chr_y_axis_label),
      function(grp_tbl, grp_key) {
        stopifnot(identical(nrow(grp_tbl), 1L))
        # When tpm_tbl has 0 row after filtering, x_axis_label column will also
        # be added.
        res_tbl <- dplyr::mutate(
          dplyr::filter(
            tpm_tbl,
            .data$Disease == .env$grp_tbl$Disease,
            .data$cohort == .env$grp_tbl$cohort,
            .data$specimen_descriptor ==
              .env$grp_tbl$Disease_specimen_descriptor),
          chr_x_axis_label = .env$grp_key$chr_y_axis_label)

        return(res_tbl)
      }
    )

    right_boxplot_tbl <- dplyr::mutate(
      right_boxplot_tbl,
      x_axis_label = factor(
        .data$chr_x_axis_label,
        levels = levels(diff_exp_heatmap_tbl$y_axis_label)))

    right_boxplot <- ggplot2::ggplot(right_boxplot_tbl,
                                     ggplot2::aes(x = x_axis_label,
                                                  y = y_val)) +
      ggplot2::geom_boxplot(
        width = 0.6, outlier.size = 0.4, fill = "darkgrey",
        col = "black", alpha = 0.5) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        panel.grid = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        legend.position = "none",
        plot.margin = grid::unit(c(0, 0, 0, 0), "cm")) +
      ggplot2::ylim(0, boxplot_y_val_max) +
      ggplot2::scale_x_discrete(
        limits = levels(diff_exp_heatmap_tbl$y_axis_label)) +
      ggplot2::coord_flip()

    right_boxplot_y_label <- ggplot2::ggplot(right_boxplot_tbl,
                                             ggplot2::aes(x = y_val,
                                                          y = y_val)) +
      ggplot2::geom_point(alpha = 0) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        panel.grid = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        axis.text.x.top = ggplot2::element_text(
          angle = -90, vjust = 0.5, hjust = 0, size = 8),
        legend.position = "none",
        plot.margin = grid::unit(c(0, 0, 0, 0), "cm")) +
      ggplot2::ylim(0, boxplot_y_val_max) +
      ggplot2::scale_x_continuous(
        position = "top", limits = c(0, boxplot_y_val_max)) +
      ggplot2::annotate(
        "text", x = boxplot_y_val_max * 0.5,
        y = boxplot_y_val_max, size = 3, vjust = 0,
        label = boxplot_y_title)


    # Top-side boxplot
    #
    # The returned table of group_modify cannot contain the original grouping
    # variables.
    deh_uniq_x_axis_lab_tbl <- dplyr::distinct(
      dplyr::select(
        dplyr::mutate(
          diff_exp_heatmap_tbl,
          deh_chr_x_axis_label = as.character(x_axis_label)),
        GTEx_tissue_subgroup, GTEx_tissue_subgroup_sample_count,
        deh_chr_x_axis_label
      )
    )

    stopifnot(nrow(deh_uniq_x_axis_lab_tbl) > 0)
    stopifnot(identical(
      length(unique(deh_uniq_x_axis_lab_tbl$deh_chr_x_axis_label)),
      nrow(deh_uniq_x_axis_lab_tbl)
    ))
    stopifnot(identical(sum(is.na(deh_uniq_x_axis_lab_tbl)), 0L))

    top_boxplot_tbl <- dplyr::group_modify(
      dplyr::group_by(deh_uniq_x_axis_lab_tbl, deh_chr_x_axis_label),
      function(grp_tbl, grp_key) {
        stopifnot(identical(nrow(grp_tbl), 1L))
        # When tpm_tbl has 0 row after filtering, x_axis_label column will also
        # be added.
        res_tbl <- dplyr::mutate(
          dplyr::filter(
            tpm_tbl,
            .data$GTEx_tissue_subgroup == .env$grp_tbl$GTEx_tissue_subgroup),
          chr_x_axis_label = .env$grp_key$deh_chr_x_axis_label)

        return(res_tbl)
      }
    )

    top_boxplot_tbl <- dplyr::mutate(
      top_boxplot_tbl,
      x_axis_label = factor(
        .data$chr_x_axis_label,
        levels = levels(diff_exp_heatmap_tbl$x_axis_label)))

    top_boxplot <- ggplot2::ggplot(top_boxplot_tbl,
                                   ggplot2::aes(x = x_axis_label,
                                                y = y_val)) +
      ggplot2::geom_boxplot(
        width = 0.6, outlier.size = 0.4, fill = "darkgrey",
        col = "black", alpha = 0.5) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        panel.grid = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        legend.position = "none",
        plot.margin = grid::unit(c(0, 0, 0, 0), "cm")) +
      ggplot2::scale_x_discrete(
        limits = levels(diff_exp_heatmap_tbl$x_axis_label)) +
      ggplot2::ylim(0, boxplot_y_val_max)

    top_boxplot_y_label <- ggplot2::ggplot(top_boxplot_tbl,
                                           ggplot2::aes(x = y_val,
                                                        y = y_val)) +
      ggplot2::geom_point(alpha = 0) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        panel.grid = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_blank(),
        axis.title.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        axis.ticks.y = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_text(
          angle = 0, hjust = 0, size = 8,
          margin = ggplot2::margin(l = 3, unit = "pt")),
        legend.position = "none",
        plot.margin = grid::unit(c(0, 0, 0, 0), "cm")) +
      ggplot2::xlim(0, boxplot_y_val_max) +
      ggplot2::ylim(0, boxplot_y_val_max) +
      ggplot2::annotate(
        "text", x = 0, y = boxplot_y_val_max * 0.5, size = 3, vjust = 1,
        angle = -90, label = boxplot_y_title)

    # Combine plots
    combined_plot_layout_mat <- rbind(
      t(matrix(rep(c(rep(NA, 11), rep(7, 12), rep(NA, 10)), 2),
               ncol = 2)),
      t(matrix(rep(c(rep(NA, 11), rep(10, 16), rep(11, 6)), 5),
               ncol = 5)),
      t(matrix(rep(c(rep(3, 11), rep(1, 16), rep(2, 3), rep(5, 3)), 21),
               ncol = 21)),
      t(matrix(rep(c(rep(NA, 11), rep(4, 16), rep(6, 3), rep(NA, 3)), 9),
               ncol = 9))
    )

    combined_plot <- gridExtra::grid.arrange(
      diff_exp_heatmap, right_boxplot, diff_exp_heatmap_y_text,
      diff_exp_heatmap_x_text, diff_exp_heatmap_legend,
      right_boxplot_y_label, combined_plot_title, top_boxplot,
      top_boxplot_y_label, layout_matrix = combined_plot_layout_mat)

    res_plot <- combined_plot
    # jscpd:ignore-end

  } else {
    stop(paste("Unknown include_boxplot", include_boxplot))
  }

  return(res_plot)
}
