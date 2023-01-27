# plumber.R defines plumber API
#
# This file should be run with the directory that contains this file as working
# directory.
#
# Call sequence:
#
# - docker run calls Rscript --vanilla main.R
# - ../main.R calls plumber::pr("src/plumber.R")



# API documentation annotations ------------------------------------------------

#* @apiTitle OpenPedCan public API

#* @apiDescription OpenPedCan (Open Pediatric Cancers) project public API
#*   transfers
#*   <a href="https://github.com/PediatricOpenTargets/OpenPedCan-analysis">OpenPedCan-analysis</a>
#*   results and plots via HTTP. This API is under beta phase development at
#*   <a href="https://github.com/PediatricOpenTargets/OpenPedCan-api">OpenPedCan-api</a>.
#*   The OpenPedCan project is a sub-project of
#*   <a href="https://github.com/PediatricOpenTargets">Pediatric Open Targets project</a>.


#* @apiVersion v0.5.3-beta

#* @apiContact list(name = "API Support", url =
#*   "https://github.com/PediatricOpenTargets/OpenPedCan-api/issues")



# Plumber API definitions ------------------------------------------------------

#* Logger adapted from
#* https://www.rplumber.io/articles/routing-and-input.html#forward-to-another-handler
#*
#* Log some information about the incoming request
#* @filter logger
function(req, res) {
  cat(as.character(Sys.time()), "-\n",
     req$REQUEST_METHOD, req$PATH_INFO, "-\n",
     "body ", req$body, "-\n",
     "QUERY_STRING ", req$QUERY_STRING, "-\n",
     req$HTTP_USER_AGENT, "@", req$REMOTE_ADDR, "\n")
  cat("--------------------------\n")
  plumber::forward()
}

#* Cross-Origin Resource Sharing (CORS) workaround.
#*
#* Copied from
#* https://www.rplumber.io/articles/security.html#cross-origin-resource-sharing-cors
#*
#* Enable Cross-Origin Resource Sharing (CORS) by default for all endpoints. To
#* disable for certain endpoints, add preempt annotation to exclude this filter.
#*
#* @filter cors
function(res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  plumber::forward()
}



# TPM boxplots -----------------------------------------------------------------

# The param of plumber endpoint cannot be broken into multiple lines.

#* Get a single-gene single-disease all-GTEx-tissues TPM summary table
#*
#* @tag "Bulk tissue gene expression"
#* @param ensemblId:str one gene ENSG ID.
#* @param efoId:str one EFO ID.
#* @param includeTumorDesc:str primaryOnly, or relapseOnly, or primaryAndRelapseInSameBox, or primaryAndRelapseInDifferentBoxes.
#* @serializer json
#* @get /tpm/gene-disease-gtex/json
function(ensemblId, efoId, includeTumorDesc) {
  gene_tpm_boxplot_tbl <- get_tpm_endpoint_tbl(
    ensg_id = ensemblId, efo_id = efoId, include_tumor_desc = includeTumorDesc,
    gtex_sample_group = "require", min_n_per_box = 3L)

  gene_tpm_boxplot_summary_tbl <- get_gene_tpm_boxplot_summary_tbl(
    gene_tpm_boxplot_tbl)

  return(gene_tpm_boxplot_summary_tbl)
}

#* Get a single-gene single-disease all-GTEx-tissues TPM boxplot
#*
#* @tag "Bulk tissue gene expression"
#* @param ensemblId:str one gene ENSG ID.
#* @param efoId:str one EFO ID.
#* @param yAxisScale:str linear or log10
#* @param includeTumorDesc:str primaryOnly, or relapseOnly, or primaryAndRelapseInSameBox, or primaryAndRelapseInDifferentBoxes.
#* @serializer png list(res = 300, width = 6000, height = 2700)
#* @get /tpm/gene-disease-gtex/plot
function(ensemblId, efoId, yAxisScale, includeTumorDesc) {
  gene_tpm_boxplot_tbl <- get_tpm_endpoint_tbl(
    ensg_id = ensemblId, efo_id = efoId, include_tumor_desc = includeTumorDesc,
    gtex_sample_group = "require", min_n_per_box = 3L)

  res_plot <- get_gene_tpm_boxplot(
    gene_tpm_boxplot_tbl, y_axis_scale = yAxisScale)

  print(res_plot)
}

#* Get a single-gene single-disease all-TCGA TPM summary table
#*
#* @tag "Bulk tissue gene expression"
#* @param ensemblId:str one gene ENSG ID.
#* @param efoId:str one EFO ID.
#* @param includeTumorDesc:str primaryOnly, or relapseOnly, or primaryAndRelapseInSameBox, or primaryAndRelapseInDifferentBoxes.
#* @serializer json
#* @get /tpm/gene-disease-tcga/json
function(ensemblId, efoId, includeTumorDesc) {
  gene_tpm_boxplot_tbl <- get_tpm_endpoint_tbl(
    ensg_id = ensemblId, efo_id = efoId, include_tumor_desc = includeTumorDesc,
    gtex_sample_group = "exclude", min_n_per_box = 3L,
    tcga_sample_group = "require")

  gene_tpm_boxplot_summary_tbl <- get_gene_tpm_boxplot_summary_tbl(
    gene_tpm_boxplot_tbl)

  return(gene_tpm_boxplot_summary_tbl)
}

#* Get a single-gene single-disease all-TCGA TPM boxplot
#*
#* @tag "Bulk tissue gene expression"
#* @param ensemblId:str one gene ENSG ID.
#* @param efoId:str one EFO ID.
#* @param yAxisScale:str linear or log10
#* @param includeTumorDesc:str primaryOnly, or relapseOnly, or primaryAndRelapseInSameBox, or primaryAndRelapseInDifferentBoxes.
#* @serializer png list(res = 300, width = 5000, height = 3300)
#* @get /tpm/gene-disease-tcga/plot
function(ensemblId, efoId, yAxisScale, includeTumorDesc) {
  gene_tpm_boxplot_tbl <- get_tpm_endpoint_tbl(
    ensg_id = ensemblId, efo_id = efoId, include_tumor_desc = includeTumorDesc,
    gtex_sample_group = "exclude", min_n_per_box = 3L,
    tcga_sample_group = "require")

  res_plot <- get_gene_tpm_boxplot(
    gene_tpm_boxplot_tbl, y_axis_scale = yAxisScale)

  print(res_plot)
}

#* Get a single-gene single-disease all-TCGA all-GTEx TPM summary table
#*
#* @tag "Bulk tissue gene expression"
#* @param ensemblId:str one gene ENSG ID.
#* @param efoId:str one EFO ID.
#* @param includeTumorDesc:str primaryOnly, or relapseOnly, or primaryAndRelapseInSameBox, or primaryAndRelapseInDifferentBoxes.
#* @serializer json
#* @get /tpm/gene-disease-tcga-gtex/json
function(ensemblId, efoId, includeTumorDesc) {
  gene_tpm_boxplot_tbl <- get_tpm_endpoint_tbl(
    ensg_id = ensemblId, efo_id = efoId, include_tumor_desc = includeTumorDesc,
    gtex_sample_group = "require", min_n_per_box = 3L,
    tcga_sample_group = "require")

  gene_tpm_boxplot_summary_tbl <- get_gene_tpm_boxplot_summary_tbl(
    gene_tpm_boxplot_tbl)

  return(gene_tpm_boxplot_summary_tbl)
}

#* Get a single-gene single-disease all-TCGA all-GTEx TPM boxplot
#*
#* @tag "Bulk tissue gene expression"
#* @param ensemblId:str one gene ENSG ID.
#* @param efoId:str one EFO ID.
#* @param yAxisScale:str linear or log10
#* @param includeTumorDesc:str primaryOnly, or relapseOnly, or primaryAndRelapseInSameBox, or primaryAndRelapseInDifferentBoxes.
#* @serializer png list(res = 300, width = 11000, height = 3300)
#* @get /tpm/gene-disease-tcga-gtex/plot
function(ensemblId, efoId, yAxisScale, includeTumorDesc) {
  gene_tpm_boxplot_tbl <- get_tpm_endpoint_tbl(
    ensg_id = ensemblId, efo_id = efoId, include_tumor_desc = includeTumorDesc,
    gtex_sample_group = "require", min_n_per_box = 3L,
    tcga_sample_group = "require")

  res_plot <- get_gene_tpm_boxplot(
    gene_tpm_boxplot_tbl, y_axis_scale = yAxisScale)

  print(res_plot)
}

#* Get a single-gene all-diseases TPM summary table
#*
#* @tag "Bulk tissue gene expression"
#* @param ensemblId:str one gene ENSG ID.
#* @param includeTumorDesc:str primaryOnly, or relapseOnly, or primaryAndRelapseInSameBox, or primaryAndRelapseInDifferentBoxes.
#* @serializer json
#* @get /tpm/gene-all-cancer/json
function(ensemblId, includeTumorDesc) {
  gene_tpm_boxplot_tbl <- get_tpm_endpoint_tbl(
    ensg_id = ensemblId, efo_id = NULL, include_tumor_desc = includeTumorDesc,
    gtex_sample_group = "exclude", min_n_per_box = 3L)

  gene_tpm_boxplot_summary_tbl <- get_gene_tpm_boxplot_summary_tbl(
    gene_tpm_boxplot_tbl)

  return(gene_tpm_boxplot_summary_tbl)
}

#* Get a single-gene all-diseases TPM boxplot
#*
#* @tag "Bulk tissue gene expression"
#* @param ensemblId:str one gene ENSG ID.
#* @param yAxisScale:str linear or log10
#* @param includeTumorDesc:str primaryOnly, or relapseOnly, or primaryAndRelapseInSameBox, or primaryAndRelapseInDifferentBoxes.
#* @serializer png list(res = 300, width = 4500, height = 2700)
#* @get /tpm/gene-all-cancer/plot
function(ensemblId, yAxisScale, includeTumorDesc) {
  gene_tpm_boxplot_tbl <- get_tpm_endpoint_tbl(
    ensg_id = ensemblId, efo_id = NULL, include_tumor_desc = includeTumorDesc,
    gtex_sample_group = "exclude", min_n_per_box = 3L)

  res_plot <- get_gene_tpm_boxplot(
    gene_tpm_boxplot_tbl, y_axis_scale = yAxisScale)

  print(res_plot)
}

#* Get a single-gene all-diseases all-GTEx-tissues-collapsed TPM summary table
#*
#* @tag "Bulk tissue gene expression"
#* @param ensemblId:str one gene ENSG ID.
#* @param includeTumorDesc:str primaryOnly, or relapseOnly, or primaryAndRelapseInSameBox, or primaryAndRelapseInDifferentBoxes.
#* @serializer json
#* @get /tpm/gene-all-cancer-collapsed-gtex/json
function(ensemblId, includeTumorDesc) {
  gene_tpm_boxplot_tbl <- get_tpm_endpoint_tbl(
    ensg_id = ensemblId, efo_id = NULL, include_tumor_desc = includeTumorDesc,
    gtex_sample_group = "require", gtex_histology_group = "collapse",
    min_n_per_box = 3L)

  gene_tpm_boxplot_summary_tbl <- get_gene_tpm_boxplot_summary_tbl(
    gene_tpm_boxplot_tbl)

  return(gene_tpm_boxplot_summary_tbl)
}

#* Get a single-gene all-diseases all-GTEx-tissues-collapsed TPM boxplot
#*
#* @tag "Bulk tissue gene expression"
#* @param ensemblId:str one gene ENSG ID.
#* @param yAxisScale:str linear or log10
#* @param includeTumorDesc:str primaryOnly, or relapseOnly, or primaryAndRelapseInSameBox, or primaryAndRelapseInDifferentBoxes.
#* @serializer png list(res = 300, width = 4500, height = 2700)
#* @get /tpm/gene-all-cancer-collapsed-gtex/plot
function(ensemblId, yAxisScale, includeTumorDesc) {
  gene_tpm_boxplot_tbl <- get_tpm_endpoint_tbl(
    ensg_id = ensemblId, efo_id = NULL, include_tumor_desc = includeTumorDesc,
    gtex_sample_group = "require", gtex_histology_group = "collapse",
    min_n_per_box = 3L)

  res_plot <- get_gene_tpm_boxplot(
    gene_tpm_boxplot_tbl, y_axis_scale = yAxisScale)

  print(res_plot)
}

#* Get a single-gene all-diseases all-GTEx-tissues TPM summary table
#*
#* @tag "Bulk tissue gene expression"
#* @param ensemblId:str one gene ENSG ID.
#* @param includeTumorDesc:str primaryOnly, or relapseOnly, or primaryAndRelapseInSameBox, or primaryAndRelapseInDifferentBoxes.
#* @serializer json
#* @get /tpm/gene-all-cancer-gtex/json
function(ensemblId, includeTumorDesc) {
  gene_tpm_boxplot_tbl <- get_tpm_endpoint_tbl(
    ensg_id = ensemblId, efo_id = NULL, include_tumor_desc = includeTumorDesc,
    gtex_sample_group = "require", gtex_histology_group = "tissue_subgroup",
    min_n_per_box = 3L)

  gene_tpm_boxplot_summary_tbl <- get_gene_tpm_boxplot_summary_tbl(
    gene_tpm_boxplot_tbl)

  return(gene_tpm_boxplot_summary_tbl)
}

#* Get a single-gene all-diseases all-GTEx-tissues TPM boxplot
#*
#* @tag "Bulk tissue gene expression"
#* @param ensemblId:str one gene ENSG ID.
#* @param yAxisScale:str linear or log10
#* @param includeTumorDesc:str primaryOnly, or relapseOnly, or primaryAndRelapseInSameBox, or primaryAndRelapseInDifferentBoxes.
#* @serializer png list(res = 300, width = 7800, height = 2700)
#* @get /tpm/gene-all-cancer-gtex/plot
function(ensemblId, yAxisScale, includeTumorDesc) {
  gene_tpm_boxplot_tbl <- get_tpm_endpoint_tbl(
    ensg_id = ensemblId, efo_id = NULL, include_tumor_desc = includeTumorDesc,
    gtex_sample_group = "require", gtex_histology_group = "tissue_subgroup",
    min_n_per_box = 3L)

  res_plot <- get_gene_tpm_boxplot(
    gene_tpm_boxplot_tbl, y_axis_scale = yAxisScale)

  print(res_plot)
}

#* Get a single-gene all-diseases all-TCGA TPM summary table
#*
#* @tag "Bulk tissue gene expression"
#* @param ensemblId:str one gene ENSG ID.
#* @param includeTumorDesc:str primaryOnly, or relapseOnly, or primaryAndRelapseInSameBox, or primaryAndRelapseInDifferentBoxes.
#* @serializer json
#* @get /tpm/gene-all-cancer-tcga/json
function(ensemblId, includeTumorDesc) {
  gene_tpm_boxplot_tbl <- get_tpm_endpoint_tbl(
    ensg_id = ensemblId, efo_id = NULL, include_tumor_desc = includeTumorDesc,
    gtex_sample_group = "exclude", min_n_per_box = 3L,
    tcga_sample_group = "require")

  gene_tpm_boxplot_summary_tbl <- get_gene_tpm_boxplot_summary_tbl(
    gene_tpm_boxplot_tbl)

  return(gene_tpm_boxplot_summary_tbl)
}

#* Get a single-gene all-diseases all-TCGA TPM boxplot
#*
#* @tag "Bulk tissue gene expression"
#* @param ensemblId:str one gene ENSG ID.
#* @param yAxisScale:str linear or log10
#* @param includeTumorDesc:str primaryOnly, or relapseOnly, or primaryAndRelapseInSameBox, or primaryAndRelapseInDifferentBoxes.
#* @serializer png list(res = 300, width = 8000, height = 3300)
#* @get /tpm/gene-all-cancer-tcga/plot
function(ensemblId, yAxisScale, includeTumorDesc) {
  gene_tpm_boxplot_tbl <- get_tpm_endpoint_tbl(
    ensg_id = ensemblId, efo_id = NULL, include_tumor_desc = includeTumorDesc,
    gtex_sample_group = "exclude", min_n_per_box = 3L,
    tcga_sample_group = "require")

  res_plot <- get_gene_tpm_boxplot(
    gene_tpm_boxplot_tbl, y_axis_scale = yAxisScale)

  print(res_plot)
}

#* Get a single-gene all-diseases all-TCGA all-GTEx TPM summary table
#*
#* @tag "Bulk tissue gene expression"
#* @param ensemblId:str one gene ENSG ID.
#* @param includeTumorDesc:str primaryOnly, or relapseOnly, or primaryAndRelapseInSameBox, or primaryAndRelapseInDifferentBoxes.
#* @serializer json
#* @get /tpm/gene-all-cancer-tcga-gtex/json
function(ensemblId, includeTumorDesc) {
  gene_tpm_boxplot_tbl <- get_tpm_endpoint_tbl(
    ensg_id = ensemblId, efo_id = NULL, include_tumor_desc = includeTumorDesc,
    gtex_sample_group = "require", min_n_per_box = 3L,
    tcga_sample_group = "require")

  gene_tpm_boxplot_summary_tbl <- get_gene_tpm_boxplot_summary_tbl(
    gene_tpm_boxplot_tbl)

  return(gene_tpm_boxplot_summary_tbl)
}

#* Get a single-gene all-diseases all-TCGA all-GTEx TPM boxplot
#*
#* @tag "Bulk tissue gene expression"
#* @param ensemblId:str one gene ENSG ID.
#* @param yAxisScale:str linear or log10
#* @param includeTumorDesc:str primaryOnly, or relapseOnly, or primaryAndRelapseInSameBox, or primaryAndRelapseInDifferentBoxes.
#* @serializer png list(res = 300, width = 15000, height = 3300)
#* @get /tpm/gene-all-cancer-tcga-gtex/plot
function(ensemblId, yAxisScale, includeTumorDesc) {
  gene_tpm_boxplot_tbl <- get_tpm_endpoint_tbl(
    ensg_id = ensemblId, efo_id = NULL, include_tumor_desc = includeTumorDesc,
    gtex_sample_group = "require", min_n_per_box = 3L,
    tcga_sample_group = "require")

  res_plot <- get_gene_tpm_boxplot(
    gene_tpm_boxplot_tbl, y_axis_scale = yAxisScale)

  print(res_plot)
}



# Differential gene expression heatmap endpoints -------------------------------

#* Get a table of one disease and top differentially expressed genes
#*
#* @tag "Bulk tissue differential gene expression"
#* @param efoId:str one EFO ID
#* @param rankGenesBy:str cgc_all_gene_up_reg_rank, or cgc_all_gene_down_reg_rank, or cgc_all_gene_up_and_down_reg_rank, or cgc_pmtl_gene_up_reg_rank, or cgc_pmtl_gene_down_reg_rank, or cgc_pmtl_gene_up_and_down_reg_rank
#* @serializer json
#* @get /dge/top-gene-disease-gtex-diff-exp/json
function(efoId, rankGenesBy) {
  # Not implemented parameter:
  # - spec_desc_group
  res_tbl <- get_one_efo_top_ensg_diff_exp_heatmap_tbl(
    efo_id = efoId, rank_genes_by = rankGenesBy, max_gene_rank = 50,
    cohort = NULL, min_n_samples_per_group = 3,
    spec_desc_group = "primary_and_relapse_same_group")

  return(res_tbl)
}

#* Get a heatmap of one disease and top differentially expressed genes
#*
#* @tag "Bulk tissue differential gene expression"
#* @param efoId:str one EFO ID
#* @param rankGenesBy:str cgc_all_gene_up_reg_rank, or cgc_all_gene_down_reg_rank, or cgc_all_gene_up_and_down_reg_rank, or cgc_pmtl_gene_up_reg_rank, or cgc_pmtl_gene_down_reg_rank, or cgc_pmtl_gene_up_and_down_reg_rank
#* @param includeBoxplot:str true or false
#* @param boxplotYAxisScale:str linear or log10
#* @serializer png list(res = 300, width = 7500, height = 3900)
#* @get /dge/top-gene-disease-gtex-diff-exp/plot
function(efoId, rankGenesBy, includeBoxplot, boxplotYAxisScale) {
  # Not implemented parameter:
  # - spec_desc_group
  res_tbl <- get_one_efo_top_ensg_diff_exp_heatmap_tbl(
    efo_id = efoId, rank_genes_by = rankGenesBy, max_gene_rank = 50,
    cohort = NULL, min_n_samples_per_group = 3,
    spec_desc_group = "primary_and_relapse_same_group")

  res_plot <- get_one_efo_top_ensg_diff_exp_heatmap(
    res_tbl, include_boxplot = includeBoxplot,
    boxplot_y_axis_scale = boxplotYAxisScale)

  print(res_plot)
}

#* Get a differential gene expression table of one gene and all diseases
#*
#* @tag "Bulk tissue differential gene expression"
#* @param ensemblId:str one gene ENSG ID.
#* @serializer json
#* @get /dge/gene-all-cancer-gtex-diff-exp/json
function(ensemblId) {
  # Not implemented parameter:
  # - spec_desc_group
  res_tbl <- get_one_ensg_all_efo_diff_exp_heatmap_tbl(
    ensg_id = ensemblId, gene_symbol = NULL, min_n_samples_per_group = 3,
    spec_desc_group = "primary_and_relapse_same_group")

  return(res_tbl)
}

#* Get a differential gene expression heatmap of one gene and all diseases
#*
#* @tag "Bulk tissue differential gene expression"
#* @param ensemblId:str one gene ENSG ID.
#* @param includeBoxplot:str true or false
#* @param boxplotYAxisScale:str linear or log10
#* @serializer png list(res = 300, width = 5900, height = 3900)
#* @get /dge/gene-all-cancer-gtex-diff-exp/plot
function(ensemblId, includeBoxplot, boxplotYAxisScale) {
  # Not implemented parameter:
  # - spec_desc_group
  res_tbl <- get_one_ensg_all_efo_diff_exp_heatmap_tbl(
    ensg_id = ensemblId, gene_symbol = NULL, min_n_samples_per_group = 3,
    spec_desc_group = "primary_and_relapse_same_group")

  res_plot <- get_one_ensg_all_efo_diff_exp_heatmap(
    res_tbl, include_boxplot = includeBoxplot,
    boxplot_y_axis_scale = boxplotYAxisScale)

  print(res_plot)
}



# Testing endpoints ------------------------------------------------------------
# Simple testing endpoints. Source: https://github.com/rstudio/plumber/ .

#* Echo back the input
#*
#* @tag "API testing"
#* @param msg The message to echo
#* @get /echo
function(msg="") {
  list(msg = paste0("The message is: '", msg, "'"))
}

#* Get database statistics
#*
#* @tag "API testing"
#* @serializer unboxedJSON
#* @get /db-stats
function() {
  # jscpd:ignore-start
  # Query database.
  #
  # Database queries are handled in a case-by-case manner, by design, rather
  # than developing an interface, mainly for the following reasons:
  #
  # - Security. Simpler query procedures can be more straightforwardly checked
  #   for security issues.
  # - Flexibility. The developers have to specify schema, table, query statment,
  #   e.g. WHERE, LIMIT, etc, for each query. Different queries may be very
  #   different, so abstracting the statements is going to be very complex.
  conn <- connect_db(db_env_vars)

  # Case insensitive db schema and table names. DBI/glue quotes names. Table
  # columns are case sensitive.
  q_schema <- tolower(db_env_vars$BULK_EXP_SCHEMA)  # nolint: object_usage_linter.
  q_table <- tolower(db_env_vars$BULK_EXP_TPM_HISTOLOGY_TBL)  # nolint: object_usage_linter.

  q_rs <- DBI::dbSendQuery(
    conn,
    glue::glue_sql("
      SELECT *
      FROM {`q_schema`}.{`q_table`}
      LIMIT 1
    ", .con = conn)
  )
  # "dbFetch() always returns a data.frame with as many rows as records were
  # fetched and as many columns as fields in the result set, even if the result
  # is a single value or has one or zero rows."
  #
  # Ref: https://dbi.r-dbi.org/reference/dbfetch
  q_rs_df <- DBI::dbFetch(q_rs)
  DBI::dbClearResult(q_rs)

  stopifnot(nrow(q_rs_df) == 1)
  stopifnot("Gene_Ensembl_ID" %in% colnames(q_rs_df))

  ensg_id <- q_rs_df$Gene_Ensembl_ID
  stopifnot(is.character(ensg_id))

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
  # jscpd:ignore-end

  stopifnot("Kids_First_Biospecimen_ID" %in% colnames(q_rs_df))
  db_bids <- q_rs_df$Kids_First_Biospecimen_ID
  stopifnot(is.character(db_bids))

  db_stats <- list(
    n_unique_biospecimens = length(unique(db_bids))
  )

  return(db_stats)
}

#* Test database connection with a simple query
#*
#* @tag "API testing"
#* @serializer json
#* @get /db-connection
function() {
  # Get the first row of a database table that should exist.

  # jscpd:ignore-start
  # Query database.
  conn <- connect_db(db_env_vars)

  # Case insensitive db schema and table names. DBI/glue quotes names. Table
  # columns are case sensitive.
  q_schema <- tolower(db_env_vars$BULK_EXP_SCHEMA)  # nolint: object_usage_linter.
  q_table <- tolower(db_env_vars$BULK_EXP_TPM_HISTOLOGY_TBL)  # nolint: object_usage_linter.

  q_rs <- DBI::dbSendQuery(
    conn,
    glue::glue_sql("
      SELECT *
      FROM {`q_schema`}.{`q_table`}
      LIMIT 1
    ", .con = conn)
  )
  # "dbFetch() always returns a data.frame with as many rows as records were
  # fetched and as many columns as fields in the result set, even if the result
  # is a single value or has one or zero rows."
  #
  # Ref: https://dbi.r-dbi.org/reference/dbfetch
  q_rs_df <- DBI::dbFetch(q_rs)
  DBI::dbClearResult(q_rs)
  DBI::dbDisconnect(conn)
  # jscpd:ignore-end

  return(q_rs_df)
}
