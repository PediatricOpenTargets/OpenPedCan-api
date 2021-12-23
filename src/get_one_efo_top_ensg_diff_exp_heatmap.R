# get_one_efo_top_ensg_diff_exp_heatmap.R defines a function
# get_one_efo_top_ensg_diff_exp_heatmap to return a differential expression
# tibble of one EFO ID and top differentially expressed ENSG IDs.
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
# - y_axis_scale: a single character value of either "linear" or "log10".
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
# Returns a
get_one_efo_top_ensg_diff_exp_heatmap <- function(
  efo_id, rank_genes_by, max_gene_rank, y_axis_scale, cohort = NULL,
  spec_desc_group = "primary_and_relapse_same_group") {

  all_cohorts_str_id <- "All Cohorts"

  stopifnot(is.character(y_axis_scale))
  stopifnot(identical(length(y_axis_scale), 1L))
  stopifnot(y_axis_scale %in% c("linear", "log10"))

  diff_exp_tbl <- get_one_efo_top_ensg_diff_exp_tbl(
    efo_id, rank_genes_by, max_gene_rank)

  if (!is.null(cohort)) {
    stopifnot(is.character(cohort))
    stopifnot(length(cohort) > 0)
    stopifnot(all(!is.na(cohort)))

    diff_exp_tbl <- dplyr::filter(diff_exp_tbl, .data$cohort == .env$cohort)
  } else {
    if (all_cohorts_str_id %in% diff_exp_tbl$cohort) {
      diff_exp_tbl <- dplyr::filter(
        diff_exp_tbl, .data$cohort == .env$all_cohorts_str_id)
    } else {
      stopifnot(identical(length(unique(diff_exp_tbl$cohort)), 1L))
    }
  }

  stopifnot(nrow(diff_exp_tbl) > 0)

  return(diff_exp_tbl)
}
