#!/bin/bash
set -e
set -u
set -o pipefail

# Input argument can only be path to one image.  

# Use imagemagick to check Absolute Error count of the number of different
# pixels. Ref: https://imagemagick.org/script/command-line-options.php
#
# -fuzz '1%': Colors within 1% distance are considered equal.
# -metric AE: absolute error count, number of different pixels (-fuzz affected).

compare -verbose -fuzz '0.5%' -metric AE "${1}" <(git show "HEAD:${1}") "null:"
