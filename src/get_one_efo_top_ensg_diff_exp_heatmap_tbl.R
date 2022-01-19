# get_one_efo_top_ensg_diff_exp_heatmap_tbl.R defines a function
# get_one_efo_top_ensg_diff_exp_heatmap_tbl to return a differential expression
# tibble of one EFO ID and top differentially expressed ENSG IDs.
#
# Call sequence:
#
# - docker run calls Rscript --vanilla main.R
# - ../main.R calls source("src/get_one_efo_top_ensg_diff_exp_heatmap_tbl.R")
#
# Defined variables:
#
# - get_one_efo_top_ensg_diff_exp_heatmap_tbl



# Get a differential expression heatmap table of one EFO ID and top
# differentially expressed ENSG IDs
#
# Args:
# - efo_id: a single character value of EFO ID. Default is NULL.
# - rank_genes_by: a single character value of the following.
#   - "cgc_all_gene_up_reg_rank": Ranking of up-regulation among all genes in
#     each cancer_group and cohort, comparing to GTEx tissues. cgc is a
#     shorthand for (cancer_group, cohort) tuple.
#   - "cgc_all_gene_down_reg_rank": Ranking of down-regulation among all genes
#     in each cancer_group and cohort, comparing to GTEx tissues.
#   - "cgc_all_gene_up_and_down_reg_rank": Ranking of up- and down-regulation
#     among all genes in each cancer_group and cohort, comparing to GTEx
#     tissues.
#   - "cgc_pmtl_gene_up_reg_rank": Ranking of up-regulation among PMTL genes in
#     each cancer_group and cohort, comparing to GTEx tissues. If rank genes by
#     this option, only PMTL genes will be included in database query result.
#   - "cgc_pmtl_gene_down_reg_rank": Ranking of down-regulation among PMTL genes
#     in each cancer_group and cohort, comparing to GTEx tissues. If rank genes
#     by this option, only PMTL genes will be included in database query result.
#   - "cgc_pmtl_gene_up_and_down_reg_rank": Ranking of up- and down-regulation
#     among PMTL genes in each cancer_group and cohort, comparing to GTEx
#     tissues. If rank genes by this option, only PMTL genes will be included in
#     database query result.
# - max_gene_rank: a single non-negative integer value. Only genes with rank <=
#   max_gene_rank are included in database query result.
# - cohort: NULL or a single character value of cohort to be included in the
#   heatmap. Default is NULL, which is to include All Cohorts or the only
#   cohort.
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
# Returns a differential expression heatmap tibble of one EFO ID and top
# differentially expressed ENSG IDs.
get_one_efo_top_ensg_diff_exp_heatmap_tbl <- function(
  efo_id, rank_genes_by, max_gene_rank, cohort = NULL,
  min_n_samples_per_group = 3L,
  spec_desc_group = "primary_and_relapse_same_group") {

  all_cohorts_str_id <- "All Cohorts"

  stopifnot(is.numeric(min_n_samples_per_group))
  stopifnot(identical(length(min_n_samples_per_group), 1L))
  stopifnot(!is.na(min_n_samples_per_group))

  diff_exp_tbl <- get_one_efo_top_ensg_diff_exp_tbl(
    efo_id, rank_genes_by, max_gene_rank)

  if (!is.null(cohort)) {
    stopifnot(is.character(cohort))
    stopifnot(length(cohort) > 0)
    stopifnot(all(!is.na(cohort)))

    diff_exp_tbl <- dplyr::filter(
      diff_exp_tbl, .data$cohort == .env$cohort) # nolint: object_usage_linter
  } else {
    if (all_cohorts_str_id %in% diff_exp_tbl$cohort) {
      diff_exp_tbl <- dplyr::filter(
        diff_exp_tbl, .data$cohort == .env$all_cohorts_str_id)
    } else {
      stopifnot(identical(length(unique(diff_exp_tbl$cohort)), 1L))
    }
  }

  stopifnot(nrow(diff_exp_tbl) > 0)

  diff_exp_tbl <- dplyr::filter(
    diff_exp_tbl,
    .data$Disease_sample_count >= .env$min_n_samples_per_group,
    .data$GTEx_tissue_subgroup_sample_count >= .env$min_n_samples_per_group)

  stopifnot(nrow(diff_exp_tbl) > 0)

  diff_exp_tbl <- dplyr::mutate(
    diff_exp_tbl,
    log2_fold_change = tidyr::replace_na(.data$log2_fold_change, 0),
    y_axis_label = glue::glue(
      paste0(
        "{Gene_symbol} {Gene_Ensembl_ID} {Disease} ",
        "(Dataset = {cohort}, ",
        "Specimen = Pediatric {Disease_specimen_descriptor}, ",
        "N = {Disease_sample_count})")),
    x_axis_label = glue::glue(
      paste0(
        "{GTEx_tissue_subgroup} (Dataset = GTEx, ",
        "N = {GTEx_tissue_subgroup_sample_count})")))

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

  diff_exp_tbl$rank_genes_by <- rank_genes_by
  diff_exp_tbl$max_gene_rank <- max_gene_rank

  stopifnot(all(!is.na(diff_exp_tbl$x_axis_label)))
  stopifnot(all(!is.na(diff_exp_tbl$y_axis_label)))

  return(diff_exp_tbl)
}
