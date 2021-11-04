# main.R is OpenPedCan-api entrypoint.
#
# This file should be run with the directory that contains this file as working
# directory.
#
# Call sequence:
#
# - docker run calls Rscript --vanilla main.R

# DEBUG is a single boolean value. Even though if (DEBUG) is evaluated at
# runtime, having DEBUG variable has more benefits than removing all extra
# assertions.
#
# - TRUE: extra assertions will in API endpoints be executed, so code will fail
#   at an earlier point.
# - FALSE (default): extra assertions will not be executed.
DEBUG <- Sys.getenv("DEBUG")
if (identical(DEBUG, "")) {
  # DEBUG env var is not set. Set to FALSE by default.
  DEBUG <- FALSE
} else if (identical(DEBUG, "0")) {
  DEBUG <- FALSE
} else if (identical(DEBUG, "1")) {
  DEBUG <- TRUE
} else {
  stop(paste("Unknown DEBUG environtment variable", DEBUG))
}

cat("---------------------------------\n",
    as.character(Sys.time()), "\n",
    "DEBUG = ", DEBUG,
    "\n---------------------------------\n")

# Notes on R environments:
#
# - Rscript runs this file in R_GlobalEnv
# - source by default runs the script in R_GlobalEnv
# - plumber::pr runs in a new environment, say X, but variables in R_GlobalEnv
#   can be accessed by plumber API functions. X inherits R_GlobalEnv (?).
# - plumber endpoint runs in a new environment, say Y, when an HTTP request
#   arrives, but variables in R_GlobalEnv and X can be accessed by plumber
#   endpoints. Y inherits X (?).

# All R scripts are sourced in .GlobalEnv, by design. Although R scripts can
# overwrite predefined variables in the .GlobalEnv, it is much more complicated
# to use R environment/(sys.)frame functionalities to manage environments/frames
# of the R scripts.
#
# To add a new script:
#
# - Only keep one variable in the end, if possible, by rm intermediate variables
#   that are not going to be used by other scripts. This way, global variables
#   can be tracked straightforwardly.
# - Use ls() to check what variables are defined by previous scripts, make sure
#   previously defined variables are not overwritten by the new script.

source("db/r_interfaces/db_env_vars.R")
source("db/r_interfaces/connect_db.R")
source("src/get_gene_tpm_tbl.R")
source("src/ggplot2_boxplot_theme.R")
source("src/get_gene_tpm_boxplot_tbl.R")
source("src/get_gene_tpm_boxplot.R")
source("src/get_gene_tpm_boxplot_summary_tbl.R")

# Adapted from https://www.rplumber.io/articles/quickstart.html
plumber::pr_run(plumber::pr("src/plumber.R"), port = 80, host = "0.0.0.0")
