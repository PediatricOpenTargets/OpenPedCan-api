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


#* @apiVersion v0.3.2-beta

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

#* Get a single-gene single-disease all-GTEx-tissues TPM summary table
#*
#* @tag "Bulk tissue gene expression"
#* @param ensemblId:str one gene ENSG ID.
#* @param efoId:str one EFO ID.
#* @serializer json
#* @get /tpm/gene-disease-gtex/json
function(ensemblId, efoId) {
  gene_tpm_tbl <- get_gene_tpm_tbl(
    ensg_id = ensemblId, gtex_sample_group = "include",
    efo_id = efoId, min_n_per_sample_group = 3)

  gene_tpm_tbl <- add_gene_tpm_box_group(gene_tpm_tbl)

  gene_tpm_boxplot_tbl <- get_gene_tpm_boxplot_tbl(gene_tpm_tbl)

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
#* @serializer png list(res = 300, width = 3900, height = 2700)
#* @get /tpm/gene-disease-gtex/plot
function(ensemblId, efoId, yAxisScale) {
  gene_tpm_tbl <- get_gene_tpm_tbl(
    ensg_id = ensemblId, gtex_sample_group = "include",
    efo_id = efoId, min_n_per_sample_group = 3)

  gene_tpm_tbl <- add_gene_tpm_box_group(gene_tpm_tbl)

  gene_tpm_boxplot_tbl <- get_gene_tpm_boxplot_tbl(gene_tpm_tbl)

  res_plot <- get_gene_tpm_boxplot(
    gene_tpm_boxplot_tbl, y_axis_scale = yAxisScale)

  print(res_plot)
}

#* Get a single-gene all-diseases TPM summary table
#*
#* @tag "Bulk tissue gene expression"
#* @param ensemblId:str one gene ENSG ID.
#* @serializer json
#* @get /tpm/gene-all-cancer/json
function(ensemblId) {
  gene_tpm_tbl <- get_gene_tpm_tbl(
    ensg_id = ensemblId, gtex_sample_group = "exclude",
    min_n_per_sample_group = 3)

  gene_tpm_tbl <- add_gene_tpm_box_group(gene_tpm_tbl)

  gene_tpm_boxplot_tbl <- get_gene_tpm_boxplot_tbl(gene_tpm_tbl)

  gene_tpm_boxplot_summary_tbl <- get_gene_tpm_boxplot_summary_tbl(
    gene_tpm_boxplot_tbl)

  return(gene_tpm_boxplot_summary_tbl)
}

#* Get a single-gene all-diseases TPM boxplot
#*
#* @tag "Bulk tissue gene expression"
#* @param ensemblId:str one gene ENSG ID.
#* @param yAxisScale:str linear or log10
#* @serializer png list(res = 300, width = 3900, height = 2700)
#* @get /tpm/gene-all-cancer/plot
function(ensemblId, yAxisScale) {
  gene_tpm_tbl <- get_gene_tpm_tbl(
    ensg_id = ensemblId, gtex_sample_group = "exclude",
    min_n_per_sample_group = 3)

  gene_tpm_tbl <- add_gene_tpm_box_group(gene_tpm_tbl)

  gene_tpm_boxplot_tbl <- get_gene_tpm_boxplot_tbl(gene_tpm_tbl)

  res_plot <- get_gene_tpm_boxplot(
    gene_tpm_boxplot_tbl, y_axis_scale = yAxisScale)

  print(res_plot)
}

#* Get a single-gene all-diseases all-GTEx-tissues TPM summary table
#*
#* @tag "Bulk tissue gene expression"
#* @param ensemblId:str one gene ENSG ID.
#* @serializer json
#* @get /tpm/gene-all-cancer-gtex/json
function(ensemblId) {
  gene_tpm_tbl <- get_gene_tpm_tbl(
    ensg_id = ensemblId, gtex_sample_group = "include",
    min_n_per_sample_group = 3)

  gene_tpm_tbl <- add_gene_tpm_box_group(
    gene_tpm_tbl, gtex_box_group = "collapse")

  gene_tpm_boxplot_tbl <- get_gene_tpm_boxplot_tbl(gene_tpm_tbl)

  gene_tpm_boxplot_summary_tbl <- get_gene_tpm_boxplot_summary_tbl(
    gene_tpm_boxplot_tbl)

  return(gene_tpm_boxplot_summary_tbl)
}

#* Get a single-gene all-diseases all-GTEx-tissues TPM boxplot
#*
#* @tag "Bulk tissue gene expression"
#* @param ensemblId:str one gene ENSG ID.
#* @param yAxisScale:str linear or log10
#* @serializer png list(res = 300, width = 3900, height = 2700)
#* @get /tpm/gene-all-cancer-gtex/plot
function(ensemblId, yAxisScale) {
  gene_tpm_tbl <- get_gene_tpm_tbl(
    ensg_id = ensemblId, gtex_sample_group = "include",
    min_n_per_sample_group = 3)

  gene_tpm_tbl <- add_gene_tpm_box_group(
    gene_tpm_tbl, gtex_box_group = "collapse")

  gene_tpm_boxplot_tbl <- get_gene_tpm_boxplot_tbl(gene_tpm_tbl)

  res_plot <- get_gene_tpm_boxplot(
    gene_tpm_boxplot_tbl, y_axis_scale = yAxisScale)

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
