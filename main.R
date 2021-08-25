# main.R is OpenPedCan-api entrypoint.
#
# This file should be run with the directory that contains this file as working
# directory.
#
# Call sequence:
#
# - docker run calls Rscript --vanilla main.R

# Notes on R environments:
#
# - Rscript runs this file in R_GlobalEnv
# - source by default runs the script in R_GlobalEnv
# - plumber::pr runs in a new environment, say X, but variables in R_GlobalEnv
#   can be accessed by plumber API functions. X inherits R_GlobalEnv (?).
# - plumber endpoint runs in a new environment, say Y, when an HTTP request
#   arrives, but variables in R_GlobalEnv and X can be accessed by plumber
#   endpoints. Y inherits X (?).

source("src/tpm_data_lists.R", chdir = TRUE)
source("src/get_single_cancer_tpm_boxplot_data.R")

# Adapted from https://www.rplumber.io/articles/quickstart.html
plumber::pr_run(plumber::pr("src/plumber.R"), port=80, host="0.0.0.0")
