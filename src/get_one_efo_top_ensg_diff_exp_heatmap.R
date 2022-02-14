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
# - include_boxplot: A single character value of either "true" or "false". Note:
#   logical TRUE or FALSE is not used in this parameter by design, to simplify
#   function calling procedure from HTTP URL. Character "true" or "false" can
#   also be extended to specify how boxplots should be generated.
# - boxplot_y_axis_scale: A single character value of either "linear" or
#   "log10".
#
# Returns a ggplot of a differential expression heatmap of one EFO ID and top
# differentially expressed ENSG IDs
get_one_efo_top_ensg_diff_exp_heatmap <- function(diff_exp_heatmap_tbl,
                                                  include_boxplot,
                                                  boxplot_y_axis_scale) {

  stopifnot(is.character(boxplot_y_axis_scale))
  stopifnot(identical(length(boxplot_y_axis_scale), 1L))
  stopifnot(boxplot_y_axis_scale %in% c("linear", "log10"))

  stopifnot(is.character(include_boxplot))
  stopifnot(identical(length(include_boxplot), 1L))
  stopifnot(include_boxplot %in% c("true", "false"))

  deh_tbl_rank_genes_by <- unique(diff_exp_heatmap_tbl$rank_genes_by)
  stopifnot(is.character(deh_tbl_rank_genes_by))
  stopifnot(identical(length(deh_tbl_rank_genes_by), 1L))
  stopifnot(!is.na(deh_tbl_rank_genes_by))

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
  )[[deh_tbl_rank_genes_by]]

  stopifnot(is.character(diff_exp_heatmap_title_rank_genes_by))

  custom_blue <- rgb(0 / 255, 114 / 255, 178 / 255)
  custom_orange <- rgb(230 / 255, 159 / 255, 0 / 255)

  diff_exp_heatmap_title <- glue::glue(
    "{diff_exp_heatmap_title_rank_genes_by} comparing ",
    "pediatric ",
    "{diff_exp_heatmap_title_disease_specimen}s ",
    "vs GTEx normal adult tissues"
  )

  diff_exp_heatmap <- ggplot2::ggplot(diff_exp_heatmap_tbl,
                                      ggplot2::aes(
                                        x = x_axis_label,
                                        y = y_axis_label,
                                        fill = log2_fold_change)) +
    ggplot2::geom_tile(color = "grey", size = 0.5) +
    ggplot2::theme_minimal() +
    ggplot2::scale_fill_gradient2(
      low = custom_blue, high = custom_orange,
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


    # Generate boxplots on the right side.
    ensg_symbol_tbl <- dplyr::distinct(dplyr::select(
      diff_exp_heatmap_tbl, Gene_Ensembl_ID, Gene_symbol))
    stopifnot(is.character(ensg_symbol_tbl$Gene_Ensembl_ID))
    stopifnot(is.character(ensg_symbol_tbl$Gene_symbol))
    stopifnot(all(!is.na(ensg_symbol_tbl$Gene_Ensembl_ID)))
    stopifnot(all(!is.na(ensg_symbol_tbl$Gene_symbol)))


    deh_tbl_efo_id <- unique(diff_exp_heatmap_tbl$EFO)
    stopifnot(is.character(deh_tbl_efo_id))
    stopifnot(identical(length(deh_tbl_efo_id), 1L))
    stopifnot(!is.na(deh_tbl_efo_id))

    # Query database.
    conn <- connect_db(db_env_vars)  # nolint: object_usage_linter.
    # Case insensitive db schema and table names. DBI/glue quotes names. Table
    # columns are case sensitive.
    q_schema <- tolower(
      db_env_vars$BULK_EXP_SCHEMA)  # nolint: object_usage_linter.
    q_table <- tolower(
      db_env_vars$BULK_EXP_TPM_HISTOLOGY_TBL)  # nolint: object_usage_linter.

    tpm_tbl <- purrr::map_dfr(
      unique(ensg_symbol_tbl$Gene_Ensembl_ID),
      function(i_gei) {
        # Use parameterized queries to protect queries from SQL injection
        # attacks.
        #
        # https://db.rstudio.com/best-practices/run-queries-safely/#parameterized-queries
        q_rs <- DBI::dbSendQuery(
          conn,
          glue::glue_sql("
            SELECT *
            FROM {`q_schema`}.{`q_table`}
            WHERE \"Gene_Ensembl_ID\" = ? AND \"EFO\" = ?
          ", .con = conn)
        )
        # Bind ? in the query statment with values.
        DBI::dbBind(q_rs, list(i_gei, deh_tbl_efo_id))
        # "dbFetch() always returns a data.frame with as many rows as records
        # were fetched and as many columns as fields in the result set, even if
        # the result is a single value or has one or zero rows."
        #
        # Ref: https://dbi.r-dbi.org/reference/dbfetch
        q_rs_df <- DBI::dbFetch(q_rs)
        DBI::dbClearResult(q_rs)

        q_rs_tbl <- tibble::as_tibble(q_rs_df)

        return(q_rs_tbl)
      }
    )

    DBI::dbDisconnect(conn)

    stopifnot(identical(unique(tpm_tbl$EFO), deh_tbl_efo_id))

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

    combined_plot_title <- grid::textGrob(
      glue::glue(
        "{diff_exp_heatmap_title}"
      ),
      x = 0, just = "left"
    )

    # Select only TPM values of samples in heatmap row sample groups.
    #
    # If an option to show GTEx or disease samples is required, add conditional
    # filter here.
    deh_uniq_y_axis_lab_tbl <- dplyr::distinct(
      dplyr::select(
        dplyr::mutate(
          diff_exp_heatmap_tbl,
          chr_y_axis_label = as.character(y_axis_label)),
        Gene_symbol, Gene_Ensembl_ID, Disease, EFO, cohort,
        Disease_specimen_descriptor, Disease_sample_count,
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
            .data$Gene_Ensembl_ID == .env$grp_tbl$Gene_Ensembl_ID,
            .data$Gene_symbol == .env$grp_tbl$Gene_symbol,
            .data$Disease == .env$grp_tbl$Disease,
            .data$EFO == .env$grp_tbl$EFO,
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

    # Add 0.1 in the y_val vector to handle all-zero case. If
    # boxplot_y_val_max is 0, ylim(0, 0) will generate a plot with (-0.0X,
    # 0.0X) x and y scales. Then, boxplot label texts will be put to the center
    # rather than close to axis tick texts.
    boxplot_y_val_max <- max(
      c(right_boxplot_tbl$y_val, 0.1), na.rm = TRUE)

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

    combined_plot_layout_mat <- rbind(
      t(matrix(rep(c(rep(NA, 11), rep(7, 12), rep(NA, 10)), 1),
               ncol = 1)),
      t(matrix(rep(c(rep(3, 11), rep(1, 16), rep(2, 3), rep(5, 3)), 22),
               ncol = 22)),
      t(matrix(rep(c(rep(NA, 11), rep(4, 16), rep(6, 3), rep(NA, 3)), 8),
               ncol = 8))
    )

    combined_plot <- gridExtra::grid.arrange(
      diff_exp_heatmap, right_boxplot, diff_exp_heatmap_y_text,
      diff_exp_heatmap_x_text, diff_exp_heatmap_legend,
      right_boxplot_y_label, combined_plot_title,
      layout_matrix = combined_plot_layout_mat)

    res_plot <- combined_plot
    # jscpd:ignore-end

  } else {
    stop(paste("Unknown include_boxplot", include_boxplot))
  }

  return(res_plot)
}
