# get_one_ensg_all_efo_diff_exp_heatmap_tbl.R defines a function
# get_one_ensg_all_efo_diff_exp_heatmap_tbl to return a differential expression
# tibble of one EFO ID and top differentially expressed ENSG IDs.
#
# Call sequence:
#
# - docker run calls Rscript --vanilla main.R
# - ../main.R calls source("src/get_one_ensg_all_efo_diff_exp_heatmap_tbl.R")
#
# Defined variables:
#
# - get_one_ensg_all_efo_diff_exp_heatmap_tbl



# Get a differential expression heatmap table of one ENSG ID and all EFO IDs
#
# Args:
# - ensg_id: a single character value of ENSG ID.
# - min_n_samples_per_group: a single numeric value of the minimum number of
#   samples per group. Default is 3.
# - spec_desc_group: NOT IMPLEMENTED. TODO: implement when data have relapse
#   tumor comparisons. A single character value with the following choices for
#   grouping biospecimen descriptors. Raise error if no (Disease, cohort) tuple
#   left after filtering.
#   - "primary_and_relapse_same_group": primary and relapse samples have the
#     same group. Remove any (Disease, cohort) tuple that has no relapse or
#     primary sample.
#   - "primary_and_relapse_different_groups": primary and relapse samples have
#     different groups. Remove any (Disease, cohort) tuple that has no relapse
#     or primary sample.
#   - "primary_only_group": for each (Disease, cohort) tuple, keep only primary
#     samples.
#   - "relapse_only_group": for each (Disease, cohort) tuple, keep only relapse
#     samples.
#
# Returns a differential expression heatmap tibble of one ENSG ID and all EFO
# IDs.
get_one_ensg_all_efo_diff_exp_heatmap_tbl <- function(
  ensg_id, gene_symbol = NULL, min_n_samples_per_group = 3,
  spec_desc_group = "primary_and_relapse_same_group") {

  if (!is.null(gene_symbol)) {
    stopifnot(is.character(gene_symbol))
    stopifnot(identical(length(gene_symbol), 1L))
    stopifnot(!is.na(gene_symbol))
  }

  stopifnot(is.numeric(min_n_samples_per_group))
  stopifnot(identical(length(min_n_samples_per_group), 1L))
  stopifnot(!is.na(min_n_samples_per_group))

  diff_exp_tbl <- get_one_ensg_all_efo_diff_exp_tbl(ensg_id)

  # TODO: extract the procedure of handling one ENSG ID mapping to more than one
  # symbols. The procedure is also used in get_gene_tpm_tbl.
  #
  # Handle one ENSG ID mapping to more than one symbols.
  det_uniq_gene_symbols <- unique(diff_exp_tbl$Gene_symbol)

  if (DEBUG) {
    stopifnot(is.character(det_uniq_gene_symbols))
    stopifnot(length(det_uniq_gene_symbols) > 0)
  }

  if (length(det_uniq_gene_symbols) > 1) {
    # ensg id is mapped to multiple symbols
    if (!is.null(gene_symbol)) {
      diff_exp_tbl <- dplyr::filter(
        diff_exp_tbl, .data$Gene_symbol == .env$gene_symbol)

      if (nrow(diff_exp_tbl) == 0) {
        stop(paste(ensg_id, gene_symbol, "is not available."))
      }
    } else {
      first_sorted_det_uniq_gene_symbol <- dplyr::first(
        sort(det_uniq_gene_symbols))

      diff_exp_tbl <- dplyr::filter(
        diff_exp_tbl,
        .data$Gene_symbol == .env$first_sorted_det_uniq_gene_symbol)
    }
  }

  if (DEBUG) {
    stopifnot(identical(length(unique(diff_exp_tbl$Gene_symbol)), 1L))
  }

  diff_exp_tbl <- dplyr::filter(
    diff_exp_tbl,
    .data$Disease_sample_count >= .env$min_n_samples_per_group,
    .data$GTEx_tissue_subgroup_sample_count >= .env$min_n_samples_per_group,
    !is.na(.data$EFO))

  stopifnot(nrow(diff_exp_tbl) > 0)

  diff_exp_tbl <- dplyr::mutate(
    diff_exp_tbl,
    log2_fold_change = tidyr::replace_na(.data$log2_fold_change, 0),
    y_axis_label = glue::glue(
      paste0(
        "{Disease} {EFO} ",
        "(Dataset = {cohort}, Specimen = {Disease_specimen_descriptor}, ",
        "N = {Disease_sample_count})")),
    x_axis_label = glue::glue(
      paste0(
        "{GTEx_tissue_subgroup} (Dataset = GTEx, ",
        "Specimen = GTEx Normal Adult Tissues, ",
        "N = {GTEx_tissue_subgroup_sample_count})")))

  # TODO: Extract the procedure to cluster x and y axis labels. The procedure is
  # also used in get_one_efo_top_ensg_diff_exp_heatmap_tbl.
  stopifnot(identical(
    sum(is.na(
      dplyr::select(
        diff_exp_tbl, cohort, EFO, Disease, Disease_specimen_descriptor,
        Disease_sample_count, Gene_Ensembl_ID, Gene_symbol, log2_fold_change,
        GTEx_tissue_subgroup, y_axis_label, GTEx_tissue_subgroup_sample_count,
        x_axis_label))),
    0L
  ))

  diff_exp_log2_fc_tbl <- tidyr::pivot_wider(
    diff_exp_tbl,
    id_cols = c(y_axis_label),
    names_from = x_axis_label,
    values_from = log2_fold_change)

  stopifnot(identical(
    length(diff_exp_log2_fc_tbl$y_axis_label),
    length(unique(diff_exp_log2_fc_tbl$y_axis_label))
  ))

  diff_exp_log2_fc_df <- tibble::column_to_rownames(
    diff_exp_log2_fc_tbl, var = "y_axis_label")

  if (DEBUG) {
    stopifnot(identical(
      nrow(diff_exp_log2_fc_df),
      length(unique(rownames(diff_exp_log2_fc_df)))
    ))

    stopifnot(all(!is.na(rownames(diff_exp_log2_fc_df))))

    stopifnot(identical(
      ncol(diff_exp_log2_fc_df),
      length(unique(colnames(diff_exp_log2_fc_df)))
    ))

    stopifnot(all(!is.na(colnames(diff_exp_log2_fc_df))))
  }


  diff_exp_log2_fc_df <- diff_exp_log2_fc_df[
    order(rownames(diff_exp_log2_fc_df), decreasing = FALSE),
    order(colnames(diff_exp_log2_fc_df), decreasing = FALSE)]

  # TODO: run hclust only to improve efficiency.
  diff_exp_log2_fc_pheatmap <- pheatmap::pheatmap(
    diff_exp_log2_fc_df, silent = TRUE)

  diff_exp_tbl <- dplyr::mutate(
    diff_exp_tbl,
    x_axis_label = factor(
      .data$x_axis_label,
      levels = diff_exp_log2_fc_pheatmap$tree_col$labels[
        diff_exp_log2_fc_pheatmap$tree_col$order]
    ),
    y_axis_label = factor(
      .data$y_axis_label,
      levels = diff_exp_log2_fc_pheatmap$tree_row$labels[
        diff_exp_log2_fc_pheatmap$tree_row$order]
    )
  )

  stopifnot(all(!is.na(diff_exp_tbl$x_axis_label)))
  stopifnot(all(!is.na(diff_exp_tbl$y_axis_label)))

  return(diff_exp_tbl)
}
