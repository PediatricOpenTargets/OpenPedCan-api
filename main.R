# Adapted from https://www.rplumber.io/articles/quickstart.html

# Get %>% without loading the whole library
`%>%` <- magrittr::`%>%`

plumber::pr("plumber.R") %>%
  plumber::pr_run(port=80, host="0.0.0.0")
