# db_env_vars.R is called by main.R and build_tools/build_db.R to create an R
# list db_env_vars that contains R odbc/DBI database connection and database
# object environment variable values for OpenPedCan-api database.
#
# This file can be run with any workding directory.
#
# Call sequence:
#
# - API HTTP server
#   - docker run calls Rscript --vanilla main.R
#   - main.R sources this file.
# - build_db process
#   - docker db/build_tools/build_db.Dockerfile image runs
#     db/build_tools/build_db_docker_cmd.sh.
#   - db/build_tools/build_db_docker_cmd.sh runs db/build_tools/build_db.R,
#   - db/build_tools/build_db.R sources this file.



# db_env_vars is a named list of single character values, in the
# format of list(opt_name = "env_var_value", ...).
db_env_vars <- lapply(
  # list(opt_name = "env_var_name", ...) specifies connection options and
  # environment variable names.
  #
  # Note: DB_USERNAME and DB_PASSWORD are set in shell to choose the correct
  # database user.
  list(
    Driver = "DB_DRIVER",
    Server = "DB_HOST",
    Port = "DB_PORT",
    Uid = "DB_USERNAME",
    Pwd = "DB_PASSWORD",

    Database = "DB_NAME",

    BULK_EXP_SCHEMA = "BULK_EXP_SCHEMA",
    BULK_EXP_TPM_HISTOLOGY_TBL = "BULK_EXP_TPM_HISTOLOGY_TBL",
    BULK_EXP_DIFF_EXP_TBL = "BULK_EXP_DIFF_EXP_TBL",
    
    CNV_SCHEMA = "CNV_SCHEMA",
    CNV_EVIDENCE_SUMMARY_TBL = "CNV_EVIDENCE_SUMMARY_TBL"
  ),
  # Get environmnt variable values.
  function(env_var_name) {
    env_var_val <- Sys.getenv(
      env_var_name, unset = NA_character_, names = FALSE)

    # Assert env_var_val is character of length 1
    stopifnot(is.character(env_var_val))
    stopifnot(identical(length(env_var_val), 1L))

    # Handle unset environemnt variables
    if (is.na(env_var_val)) {
      if (identical(env_var_name, "DB_DRIVER")) {
        env_var_val <- "PostgreSQL Unicode"
      } else if (identical(env_var_name, "BULK_EXP_SCHEMA")) {
        env_var_val <- "bulk_expression"

      } else if (identical(env_var_name, "BULK_EXP_TPM_HISTOLOGY_TBL")) {
        env_var_val <- "bulk_expression_tpm_histology"

      } else if (identical(env_var_name, "BULK_EXP_DIFF_EXP_TBL")) {
        env_var_val <- "bulk_expression_diff_exp"

      } else {
        stop(paste(
          "Error: Environment variable", env_var_name, "cannot be unset."))
      }
    }

    # Handle value type conversion
    if (identical(env_var_name, "DB_PORT")) {
      # Assert DB_PORT is an integer
      stopifnot(identical(
        as.numeric(env_var_val), as.numeric(as.integer(env_var_val))
      ))
      # DBI examples use numeric literal
      env_var_val <- as.numeric(env_var_val)
    }

    return(env_var_val)
  }
)
