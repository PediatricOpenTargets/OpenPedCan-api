# plumber.R defines plumber API
#
# This file should be run with the directory that contains this file as working
# directory.
#
# Call sequence:
#
# - docker run calls Rscript --vanilla main.R
# - ../main.R calls plumber::pr("src/plumber.R")



# API documentation annotations ------------------------------------------------

#* @apiTitle OpenPedCan public API

#* @apiDescription OpenPedCan (Open Pediatric Cancers) project public API
#*   transfers
#*   <a href="https://github.com/PediatricOpenTargets/OpenPedCan-analysis">OpenPedCan-analysis</a>
#*   results and plots via HTTP. This API is under alpha phase development at
#*   <a href="https://github.com/PediatricOpenTargets/OpenPedCan-api">OpenPedCan-api</a>.
#*   The OpenPedCan project is a sub-project of
#*   <a href="https://github.com/PediatricOpenTargets">Pediatric Open Targets project</a>.


#* @apiVersion v0.2.0-alpha

#* @apiContact list(name = "API Support", url =
#*   "https://github.com/PediatricOpenTargets/OpenPedCan-api/issues")



# Plumber API definitions ------------------------------------------------------

#* Logger adapted from
#* https://www.rplumber.io/articles/routing-and-input.html#forward-to-another-handler
#*
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

#* Cross-Origin Resource Sharing (CORS) workaround.
#*
#* Copied from
#* https://www.rplumber.io/articles/security.html#cross-origin-resource-sharing-cors
#*
#* Enable Cross-Origin Resource Sharing (CORS) by default for all endpoints. To
#* disable for certain endpoints, add preempt annotation to exclude this filter.
#*
#* @filter cors
cors <- function(res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  plumber::forward()
}

#* Get a single-gene single-disease all-GTEx-tissue-subgroups TPM summary table
#*
#* @tag "Bulk tissue gene expression"
#* @param ensemblId:str a single character value of gene ENSG ID.
#* @param efoId:str a single character value of EFO ID.
#* @serializer json
#* @get /tpm/gene-disease-gtex/json
function(ensemblId, efoId) {
  gene_tpm_tbl <- get_gene_tpm_tbl(
    ensg_id = ensemblId, efo_id = efoId, min_n_per_sample_group = 3)

  gene_tpm_tbl <- add_gene_tpm_box_group(gene_tpm_tbl)

  gene_tpm_boxplot_tbl <- get_gene_tpm_boxplot_tbl(gene_tpm_tbl)

  gene_tpm_boxplot_summary_tbl <- get_gene_tpm_boxplot_summary_tbl(
    gene_tpm_boxplot_tbl)

  return(gene_tpm_boxplot_summary_tbl)
}

#* Get a single-gene single-disease all-GTEx-tissue-subgroups TPM boxplot
#*
#* @tag "Bulk tissue gene expression"
#* @param ensemblId:str a single character value of gene ENSG ID.
#* @param efoId:str a single character value of EFO ID.
#* @serializer png list(res = 300, width = 3900, height = 2700)
#* @get /tpm/gene-disease-gtex/plot
function(ensemblId, efoId) {
  gene_tpm_tbl <- get_gene_tpm_tbl(
    ensg_id = ensemblId, efo_id = efoId, min_n_per_sample_group = 3)

  gene_tpm_tbl <- add_gene_tpm_box_group(gene_tpm_tbl)

  gene_tpm_boxplot_tbl <- get_gene_tpm_boxplot_tbl(gene_tpm_tbl)

  res_plot <- get_gene_tpm_boxplot(gene_tpm_boxplot_tbl)

  print(res_plot)
}

#* Get a single-gene all-diseases TPM summary table
#*
#* @tag "Bulk tissue gene expression"
#* @param ensemblId:str a single character value of gene ENSG ID.
#* @serializer json
#* @get /tpm/gene-all-cancer/json
function(ensemblId) {
  gene_tpm_tbl <- get_gene_tpm_tbl(
    ensg_id = ensemblId, min_n_per_sample_group = 3)

  gene_tpm_tbl <- add_gene_tpm_box_group(gene_tpm_tbl)

  gene_tpm_boxplot_tbl <- get_gene_tpm_boxplot_tbl(gene_tpm_tbl)

  gene_tpm_boxplot_summary_tbl <- get_gene_tpm_boxplot_summary_tbl(
    gene_tpm_boxplot_tbl)

  return(gene_tpm_boxplot_summary_tbl)
}

#* Get a single-gene all-diseases TPM boxplot
#*
#* @tag "Bulk tissue gene expression"
#* @param ensemblId:str a single character value of gene ENSG ID.
#* @serializer png list(res = 300, width = 3900, height = 2700)
#* @get /tpm/gene-all-cancer/plot
function(ensemblId) {
  gene_tpm_tbl <- get_gene_tpm_tbl(
    ensg_id = ensemblId, min_n_per_sample_group = 3)

  gene_tpm_tbl <- add_gene_tpm_box_group(gene_tpm_tbl)

  gene_tpm_boxplot_tbl <- get_gene_tpm_boxplot_tbl(gene_tpm_tbl)

  res_plot <- get_gene_tpm_boxplot(gene_tpm_boxplot_tbl)

  print(res_plot)
}



# Testing endpoints ------------------------------------------------------------
# Placeholder for simple testing
# Source https://github.com/rstudio/plumber/


#* Echo back the input
#*
#* @tag "API testing"
#* @param msg The message to echo
#* @get /echo
function(msg="") {
  list(msg = paste0("The message is: '", msg, "'"))
}


#* Plot a histogram
#*
#* @tag "API testing"
#* @serializer png
#* @get /plot
function() {
  rand <- rnorm(100)
  hist(rand)
}


#* Return the sum of two numbers
#*
#* @tag "API testing"
#* @param a The first number to add
#* @param b The second number to add
#* @post /sum
function(a, b) {
  as.numeric(a) + as.numeric(b)
}
