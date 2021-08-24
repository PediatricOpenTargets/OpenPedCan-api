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
function(req){
  cat(as.character(Sys.time()), "-\n",
     req$REQUEST_METHOD, req$PATH_INFO, "-\n",
     "body ", req$body, "-\n",
     "QUERY_STRING ", req$QUERY_STRING, "-\n",
     req$HTTP_USER_AGENT, "@", req$REMOTE_ADDR,
     "\n--------------------------\n")
  plumber::forward()
}

#* Get single-cancer GTEx plot and table
#* @serializer json
#* @get /single-cancer-gtex
function() {
  res <- list(
    tpm_data_lists$gtex$histology_df[1:3, ])
  return(res)
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
