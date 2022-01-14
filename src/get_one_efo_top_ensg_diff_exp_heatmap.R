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

  diff_exp_heatmap_title_disease <- paste0(
    unique(diff_exp_heatmap_tbl$Disease), collapse = " and ")

  diff_exp_heatmap_title_rank_genes_by <- list(
    cgc_all_gene_up_reg_rank = "Upregulated Genes",
    cgc_all_gene_down_reg_rank = "Downregulated Genes",
    cgc_all_gene_up_and_down_reg_rank = "Top Differentially Expressed Genes",
    cgc_pmtl_gene_up_reg_rank = "Upregulated PMTL Genes",
    cgc_pmtl_gene_down_reg_rank = "Downregulated PMTL Genes",
    cgc_pmtl_gene_up_and_down_reg_rank =
      "Top Differentially Expressed PMTL Genes"
  )[[rank_genes_by]]

  stopifnot(is.character(diff_exp_heatmap_title_rank_genes_by))

  diff_exp_heatmap_title <- glue::glue(
    "{diff_exp_heatmap_title_disease}\n(EFO_ID: {efo_id})\n",
    "Analysis: {diff_exp_heatmap_title_rank_genes_by}"
  )

  # Set margin
  plot_margin <- ggplot2::theme_get()$plot.margin

  if (!identical(length(plot_margin), 4L)) {
    plot_margin <- rep(grid::unit(x = 5.5, units = "points"), 4)
  }

  # The x-axis labels are long and rotated 50 degrees, so they are out of the
  # plot in the default margin. Increase right margin to fit all text.
  plot_margin[2] <- grid::unit(x = 18, units = "char")

  custom_sky_blue <- rgb(0 / 255, 114 / 255, 178 / 255)
  custom_orange <- rgb(230 / 255, 159 / 255, 0 / 255)

  diff_exp_heatmap <- ggplot2::ggplot(diff_exp_heatmap_tbl,
                                      ggplot2::aes(
                                        x = x_axis_label,
                                        y = y_axis_label,
                                        fill = log2_fold_change)) +
    ggplot2::geom_tile(color = "grey", size = 0.5) +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::scale_y_discrete(position = "left") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text.x.top = ggplot2::element_text(
        angle = 50, hjust = 0),
      axis.text.y.right = ggplot2::element_text(
        hjust = 1),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      plot.margin = plot_margin) +
    ggplot2::scale_fill_gradient2(
      low = custom_sky_blue,
      high = custom_orange,
      name = "log2FC") +
    ggplot2::ggtitle(diff_exp_heatmap_title)

  # Generate boxplots on the side
  ensg_symbol_tbl <- dplyr::distinct(dplyr::select(
    diff_exp_heatmap_tbl, Gene_Ensembl_ID, Gene_symbol))
  stopifnot(is.character(ensg_symbol_tbl$Gene_Ensembl_ID))
  stopifnot(is.character(ensg_symbol_tbl$Gene_symbol))
  stopifnot(all(!is.na(ensg_symbol_tbl$Gene_Ensembl_ID)))
  stopifnot(all(!is.na(ensg_symbol_tbl$Gene_symbol)))

  # Query database.
  #
  # TODO: extract this query into a function. This query is also used in
  # get_gene_tpm_tbl function.
  conn <- connect_db(db_env_vars)  # nolint: object_usage_linter.
  # Case insensitive db schema and table names. DBI/glue quotes names. Table
  # columns are case sensitive.
  q_schema <- tolower(
    db_env_vars$BULK_EXP_SCHEMA)  # nolint: object_usage_linter.
  q_table <- tolower(
    db_env_vars$BULK_EXP_TPM_HISTOLOGY_TBL)  # nolint: object_usage_linter.
  # Use parameterized queries to protect queries from SQL injection attacks.
  #
  # https://db.rstudio.com/best-practices/run-queries-safely/#parameterized-queries
  q_rs <- DBI::dbSendQuery(
    conn,
    glue::glue_sql("
      SELECT *
      FROM {`q_schema`}.{`q_table`}
      WHERE \"Gene_Ensembl_ID\" = ?
    ", .con = conn)
  )
  # Bind ? in the query statment with values.
  #
  # This query sends multiple WHERE = statements, and its performance is similar
  # to sending a single WHERE IN statement. However, WHERE IN ? statement will
  # throw an error when binding either a character vector of ENSG IDs or a
  # single character value of concatenated ENSG IDS, e.g.
  # ('ENSG00000267448','ENSG00000262477').
  DBI::dbBind(q_rs, list(unique(ensg_symbol_tbl$Gene_Ensembl_ID)))
  # "dbFetch() always returns a data.frame with as many rows as records were
  # fetched and as many columns as fields in the result set, even if the result
  # is a single value or has one or zero rows."
  #
  # Ref: https://dbi.r-dbi.org/reference/dbfetch
  q_rs_df <- DBI::dbFetch(q_rs)
  DBI::dbClearResult(q_rs)
  DBI::dbDisconnect(conn)

  tpm_tbl <- dplyr::filter(
    tibble::as_tibble(q_rs_df),
    .data$Gene_symbol %in% ensg_symbol_tbl$Gene_symbol,
    !is.na(.data$GTEx_tissue_subgroup)
  )

  if (y_axis_scale == "linear") {
    tpm_tbl <- dplyr::mutate(tpm_tbl, y_val = .data$TPM)
    boxplot_y_title <- "TPM"
  } else if (y_axis_scale == "log10") {
    tpm_tbl <- dplyr::mutate(tpm_tbl, y_val = log10(.data$TPM + 1))
    boxplot_y_title <- "log10(TPM + 1)"
  } else {
    stop(paste("Unknown y_axis_scale", y_axis_scale))
  }

  deh_uniq_y_axis_lab_tbl <- dplyr::distinct(
    dplyr::select(
      dplyr::mutate(
        diff_exp_heatmap_tbl,
        chr_y_axis_label = as.character(y_axis_label)),
      Gene_symbol, Gene_Ensembl_ID, Disease, cohort,
      Disease_specimen_descriptor, Disease_sample_count,
      chr_y_axis_label))

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
      # When tpm_tbl has 0 row after filtering, x_axis_label column will also be
      # added.
      res_tbl <- dplyr::mutate(
        dplyr::filter(
          tpm_tbl,
          .data$Gene_Ensembl_ID == .env$grp_tbl$Gene_Ensembl_ID,
          .data$Gene_symbol == .env$grp_tbl$Gene_symbol),
        chr_x_axis_label = .env$grp_key$chr_y_axis_label)

      return(res_tbl)
    }
  )

  stopifnot(all(
    right_boxplot_tbl$chr_x_axis_label %in%
      levels(diff_exp_heatmap_tbl$y_axis_label)))

  right_boxplot_tbl <- dplyr::mutate(
    right_boxplot_tbl,
    x_axis_label = factor(
      .data$chr_x_axis_label,
      levels = levels(diff_exp_heatmap_tbl$y_axis_label)))

  custom_reddish_purple <- rgb(204 / 255, 121 / 255, 167 / 255)

  right_boxplot <- ggplot2::ggplot(right_boxplot_tbl,
                                   ggplot2::aes(x = x_axis_label, y = y_val)) +
    ggplot2::geom_boxplot(
      width = 0.6, outlier.size = 0.4, fill = "black",
      col = custom_orange, alpha = 0.5) +
    ggplot2::theme_bw() +
    ggplot2::coord_flip()
    # ggplot2::theme(
    #   panel.grid = ggplot2::element_blank(),
    #   axis.text.x = ggplot2::element_blank(),
    #   axis.title.x = ggplot2::element_blank(),
    #   axis.ticks.x = ggplot2::element_blank(),
    #   axis.text.y = ggplot2::element_blank(),
    #   axis.title.y = ggplot2::element_blank(),
    #   axis.ticks.y = ggplot2::element_blank(),
    #   legend.position = "none",
    #   plot.margin = grid::unit(c(0, 0, 0, 0), "cm")) +
    # ggplot2::ylim(0, max(right_boxplot_tbl$y_val, na.rm = TRUE)) +
    # ggplot2::coord_flip()

  return(right_boxplot)

  return(diff_exp_heatmap)
}
