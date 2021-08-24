# main.R is OpenPedCan-api entrypoint.
#
# This file should be run with the directory that contains this file as working
# directory.
#
# Call sequence:
#
# - docker run calls Rscript --vanilla main.R

# Adapted from https://www.rplumber.io/articles/quickstart.html
plumber::pr_run(plumber::pr("src/plumber.R"), port=80, host="0.0.0.0")
