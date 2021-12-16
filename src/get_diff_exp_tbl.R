# get_diff_exp_tbl.R defines a function get_diff_exp_tbl to return a
# differential expression tibble of one gene ENSG ID or one EFO ID.
#
# Call sequence:
#
# - docker run calls Rscript --vanilla main.R
# - ../main.R calls source("src/get_diff_exp_tbl.R")
#
# Defined variables:
#
# - get_diff_exp_tbl



# Get a differential expression tibble of one gene ENSG ID or one EFO ID
#
# Args:
# - ensg_id: NULL or a single character value of gene ENSG ID. Default is NULL.
# - efo_id: NULL or a single character value of EFO ID. Default is NULL.
#
# Returns a tibble with the following columns:
#
get_diff_exp_tbl <- function(ensg_id = NULL, efo_id = NULL) {
  if (is.null(ensg_id)) {
    stopifnot(is.character(efo_id))
    stopifnot(identical(length(efo_id), 1L))
    stopifnot(!is.na(efo_id))

  } else if (is.null(efo_id)) {
    stopifnot(is.character(ensg_id))
    stopifnot(identical(length(ensg_id), 1L))
    stopifnot(!is.na(ensg_id))

  } else {
    stop("Error: ensg_id and efo_id cannot both be not NULL.")
  }

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

  if (!is.null(ensg_id)) {
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

  } else if (!is.null(efo_id)) {
    q_rs <- DBI::dbSendQuery(
      conn,
      glue::glue_sql("
        SELECT *
        FROM {`q_schema`}.{`q_table`}
        WHERE \"EFO\" = ?
      ", .con = conn)
    )

    DBI::dbBind(q_rs, list(efo_id))
    q_rs_df <- DBI::dbFetch(q_rs)

  } else {
    stop("Internal error: check get_diff_exp_tbl function.")
  }

  DBI::dbClearResult(q_rs)
  DBI::dbDisconnect(conn)

  diff_exp_tbl <- tibble::as_tibble(q_rs_df)

  return(diff_exp_tbl)
}
