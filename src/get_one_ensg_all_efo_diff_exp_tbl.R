# get_one_ensg_all_efo_diff_exp_tbl.R defines a function
# get_one_ensg_all_efo_diff_exp_tbl to return a differential expression tibble
# of one gene ENSG ID and all EFO IDs.
#
# Call sequence:
#
# - docker run calls Rscript --vanilla main.R
# - ../main.R calls source("src/get_one_ensg_all_efo_diff_exp_tbl.R")
#
# Defined variables:
#
# - get_one_ensg_all_efo_diff_exp_tbl



# Get a differential expression tibble of one gene ENSG ID and all EFO IDs
#
# Args:
# - ensg_id: a single character value of gene ENSG ID.
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
# - cgc_all_gene_up_reg_rank: cgc is a shorthand for (cancer_group, cohort)
#   tuple.
# - cgc_all_gene_down_reg_rank
# - cgc_all_gene_up_and_down_reg_rank
# - cgc_pmtl_gene_up_reg_rank
# - cgc_pmtl_gene_down_reg_rank
# - cgc_pmtl_gene_up_and_down_reg_rank
get_one_ensg_all_efo_diff_exp_tbl <- function(ensg_id) {
  stopifnot(is.character(ensg_id))
  stopifnot(identical(length(ensg_id), 1L))
  stopifnot(!is.na(ensg_id))

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
      WHERE \"Gene_Ensembl_ID\" = ?
    ", .con = conn)
  )
  # Bind ? in the query statment with values.
  DBI::dbBind(q_rs, list(ensg_id))
  # "dbFetch() always returns a data.frame with as many rows as records were
  # fetched and as many columns as fields in the result set, even if the result
  # is a single value or has one or zero rows."
  #
  # Ref: https://dbi.r-dbi.org/reference/dbfetch
  q_rs_df <- DBI::dbFetch(q_rs)

  DBI::dbClearResult(q_rs)
  DBI::dbDisconnect(conn)

  one_ensg_all_efo_diff_exp_tbl <- tibble::as_tibble(q_rs_df)

  return(one_ensg_all_efo_diff_exp_tbl)
}
