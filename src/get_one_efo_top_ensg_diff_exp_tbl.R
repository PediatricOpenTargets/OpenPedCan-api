# get_one_efo_top_ensg_diff_exp_tbl.R defines a function
# get_one_efo_top_ensg_diff_exp_tbl to return a differential expression tibble
# of one EFO ID and top differentially expressed ENSG IDs.
#
# Call sequence:
#
# - docker run calls Rscript --vanilla main.R
# - ../main.R calls source("src/get_one_efo_top_ensg_diff_exp_tbl.R")
#
# Defined variables:
#
# - get_one_efo_top_ensg_diff_exp_tbl



# Get a differential expression tibble of one EFO ID and top differentially
# expressed ENSG IDs
#
# Args:
# - efo_id: a single character value of EFO ID. Default is NULL.
# - rank_genes_by: a single character value of the following.
#   - "cancer_group_all_gene_up_reg_rank": Ranking of up-regulation among all
#     genes in each cancer_group and cohort, comparing to GTEx tissues.
#   - "cancer_group_all_gene_down_reg_rank": Ranking of down-regulation among
#     all genes in each cancer_group and cohort, comparing to GTEx tissues.
#   - "cancer_group_all_gene_up_and_down_reg_rank": Ranking of up- and
#     down-regulation among all genes in each cancer_group and cohort, comparing
#     to GTEx tissues.
#   - "cancer_group_pmtl_gene_up_reg_rank": Ranking of up-regulation among PMTL
#     genes in each cancer_group and cohort, comparing to GTEx tissues. If rank
#     genes by this option, only PMTL genes will be included in database query
#     result.
#   - "cancer_group_pmtl_gene_down_reg_rank": Ranking of down-regulation among
#     PMTL genes in each cancer_group and cohort, comparing to GTEx tissues. If
#     rank genes by this option, only PMTL genes will be included in database
#     query result.
#   - "cancer_group_pmtl_gene_up_and_down_reg_rank": Ranking of up- and
#     down-regulation among PMTL genes in each cancer_group and cohort,
#     comparing to GTEx tissues. If rank genes by this option, only PMTL genes
#     will be included in database query result.
# - max_gene_rank: a single non-negative integer value. Only genes with rank <=
#   max_gene_rank are included in database query result.
#
# Returns a tibble with the following columns:
# - cohort
# - EFO
# - MONDO
# - Disease: cancer_group in OpenPedCan-analysis.
# - Disease_specimen_descriptor: "Primary Tumor", "Relapse Tumor", or "Primary
#   and Relapse Tumor"
# - Disease_sample_count
# - GTEx_tissue_subgroup_UBERON
# - GTEx_tissue_subgroup
# - GTEx_tissue_subgroup_sample_count
# - Gene_symbol
# - Gene_Ensembl_ID
# - Disease_mean_TPM
# - GTEx_tissue_subgroup_mean_TPM
# - base_mean
# - log2_fold_change
# - lfcSE
# - stat
# - pvalue
# - padj
# - PMTL
# - cancer_group_all_gene_up_reg_rank
# - cancer_group_all_gene_down_reg_rank
# - cancer_group_all_gene_up_and_down_reg_rank
# - cancer_group_pmtl_gene_up_reg_rank
# - cancer_group_pmtl_gene_down_reg_rank
# - cancer_group_pmtl_gene_up_and_down_reg_rank
get_one_efo_top_ensg_diff_exp_tbl <- function(efo_id, rank_genes_by,
                                              max_gene_rank) {

  stopifnot(is.character(efo_id))
  stopifnot(identical(length(efo_id), 1L))
  stopifnot(!is.na(efo_id))

  stopifnot(is.character(rank_genes_by))
  stopifnot(identical(length(rank_genes_by), 1L))
  stopifnot(!is.na(rank_genes_by))
  stopifnot(rank_genes_by %in% c("cancer_group_all_gene_up_reg_rank",
                                 "cancer_group_all_gene_down_reg_rank",
                                 "cancer_group_all_gene_up_and_down_reg_rank",
                                 "cancer_group_pmtl_gene_up_reg_rank",
                                 "cancer_group_pmtl_gene_down_reg_rank",
                                 "cancer_group_pmtl_gene_up_and_down_reg_rank"))

  stopifnot(is.numeric(max_gene_rank))
  stopifnot(identical(length(max_gene_rank), 1L))
  stopifnot(!is.na(max_gene_rank))
  max_gene_rank <- as.integer(max_gene_rank)
  stopifnot(max_gene_rank >= 0)

  # Query database.
  #
  # connect_db and db_env_vars are coming from main.R.
  conn <- connect_db(db_env_vars)  # nolint: object_usage_linter.

  # Case insensitive db schema and table names. DBI/glue quotes names. Table
  # columns are case sensitive.
  q_schema <- tolower(
    db_env_vars$BULK_EXP_SCHEMA)  # nolint: object_usage_linter.
  q_table <- tolower(
    db_env_vars$BULK_EXP_DIFF_EXP_TBL)  # nolint: object_usage_linter.

  # Use parameterized queries to protect queries from SQL injection attacks.
  #
  # https://db.rstudio.com/best-practices/run-queries-safely/#parameterized-queries
  q_rs <- DBI::dbSendQuery(
    conn,
    glue::glue_sql("
      SELECT *
      FROM {`q_schema`}.{`q_table`}
      WHERE \"EFO\" = ? AND {`rank_genes_by`} <= ?
    ", .con = conn)
  )

  DBI::dbBind(q_rs, list(efo_id, max_gene_rank))
  q_rs_df <- DBI::dbFetch(q_rs)

  DBI::dbClearResult(q_rs)
  DBI::dbDisconnect(conn)

  one_efo_top_ensg_diff_exp_tbl <- tibble::as_tibble(q_rs_df)

  return(one_efo_top_ensg_diff_exp_tbl)
}
