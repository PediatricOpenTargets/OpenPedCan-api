# plumber.R defines plumber API
#
# This file should be run with the directory that contains this file as working
# directory.
#
# Call sequence:
#
# - docker run calls Rscript --vanilla main.R
# - ../main.R calls plumber::pr("src/plumber.R")


# Plumber API definitions ------------------------------------------------------


# logger adapted from
# https://www.rplumber.io/articles/routing-and-input.html#forward-to-another-handler

#* Log some information about the incoming request
#* @filter logger
function(req, res) {
  cat(as.character(Sys.time()), "-\n",
     req$REQUEST_METHOD, req$PATH_INFO, "-\n",
     "body ", req$body, "-\n",
     "QUERY_STRING ", req$QUERY_STRING, "-\n",
     req$HTTP_USER_AGENT, "@", req$REMOTE_ADDR,
     "\nreq:\n")
  print(req)
  cat("res:\n")
  print(res)
  cat("--------------------------\n")
  plumber::forward()
}

#* @apiTitle Get a single-gene single-disease all-GTEx-tissue-subgroups TPM
#  table
#*
#* @param ensemblId:str a single character value of gene ENSG ID.
#* @param efoId:str a single character value of EFO ID.
#* @serializer json
#* @get /tpm/gene-disease-gtex/json
function(ensemblId, efoId) {
  res_tbl <- list(ensemblId = ensemblId, efoId = efoId)
  return(res_tbl)
}

#* @apiTitle Get a single-gene single-disease all-GTEx-tissue-subgroups TPM
#  boxplot
#*
#* @param ensemblId:str a single character value of gene ENSG ID.
#* @param efoId:str a single character value of EFO ID.
#* @serializer png
#* @get /tpm/gene-disease-gtex/plot
function(ensemblId, efoId) {
  res_plot <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) +
    ggplot2::geom_point() +
    ggplot2::ggtitle(paste(ensemblId, efoId))
  print(res_plot)
}

#* @apiTitle Get a single-gene all-diseases TPM table
#*
#* @param ensemblId:str a single character value of gene ENSG ID.
#* @serializer json
#* @get /tpm/gene-all-cancer/json
function(ensemblId) {
  res_tbl <- list(ensemblId = ensemblId)
  return(res_tbl)
}

#* Echo back the input
#* @param msg The message to echo
#* @get /echo
function(msg="") {
  list(msg = paste0("The message is: '", msg, "'"))
}

#* Plot a histogram
#* @serializer png
#* @get /plot
function() {
  rand <- rnorm(100)
  hist(rand)
}

#* Return the sum of two numbers
#* @param a The first number to add
#* @param b The second number to add
#* @post /sum
function(a, b) {
  as.numeric(a) + as.numeric(b)
}
