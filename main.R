# Adapted from https://www.rplumber.io/articles/quickstart.html
plumber::pr_run(plumber::pr("src/plumber.R"), port=80, host="0.0.0.0")
