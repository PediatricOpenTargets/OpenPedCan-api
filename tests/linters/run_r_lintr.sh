#!/bin/bash
set -e
set -u
set -o pipefail

# This script should always run as if it were being called from
# the directory it lives in.
#
# Adapted from https://stackoverflow.com/a/3355423/4638182
cd "$(dirname "$0")" || exit

# Change working dir to git root dir
cd ../../

lintrs_val="lintr::with_defaults(object_name_linter = NULL, assignment_linter = NULL, line_length_linter = NULL, spaces_left_parentheses_linter = NULL, commented_code_linter = NULL, object_length_linter = NULL, cyclocomp_linter = lintr::cyclocomp_linter(complexity_limit = 35L))"

Rscript --vanilla -e "lintr::lint(filename = 'main.R', linters = ${lintrs_val})"
Rscript --vanilla -e "lintr::lint_dir(path = 'db', linters = ${lintrs_val})"
Rscript --vanilla -e "lintr::lint_dir(path = 'src', linters = ${lintrs_val})"
Rscript --vanilla -e "lintr::lint_dir(path = 'tests', linters = ${lintrs_val})"
