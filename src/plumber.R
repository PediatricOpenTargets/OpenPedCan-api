# plumber.R defines plumber API
#
# This file should be run with the directory that contains this file as working
# directory.
#
# Call sequence:
#
# - docker run calls Rscript --vanilla main.R
# - ../main.R calls plumber::pr("src/plumber.R")

# API documentation annotations

#* @apiTitle OpenPedCan public API

#* @apiDescription OpenPedCan (Open Pediatric Cancers) project public API
#*   transfers
#*   <a href="https://github.com/PediatricOpenTargets/OpenPedCan-analysis">OpenPedCan-analysis</a>
#*   results and plots via HTTP. This API is under alpha phase development at
#*   <a href="https://github.com/PediatricOpenTargets/OpenPedCan-api">OpenPedCan-api</a>.
#*   The OpenPedCan project is a sub-project of
#*   <a href="https://github.com/PediatricOpenTargets">Pediatric Open Targets project</a>.


#* @apiVersion 0.1

#* @apiContact list(name = "API Support", url =
#*   "https://github.com/PediatricOpenTargets/OpenPedCan-api/issues")



# Plumber API definitions ------------------------------------------------------

#* Logger adapted from https://www.rplumber.io/articles/routing-and-input.html#forward-to-another-handler
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

#* Get a single-gene single-disease all-GTEx-tissue-subgroups TPM summary table
#*
#* @tag "Bulk tissue gene expression"
#* @param ensemblId:str a single character value of gene ENSG ID.
#* @param efoId:str a single character value of EFO ID.
#* @serializer json
#* @get /tpm/gene-disease-gtex/json
function(ensemblId, efoId) {
  gene_tpm_tbl <- get_gene_tpm_tbl(
    tpm_data_lists = tpm_data_lists, ensg_id = ensemblId, efo_id = efoId)

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
    tpm_data_lists = tpm_data_lists, ensg_id = ensemblId, efo_id = efoId)

  gene_tpm_tbl <- add_gene_tpm_box_group(gene_tpm_tbl)

  gene_tpm_boxplot_tbl <- get_gene_tpm_boxplot_tbl(gene_tpm_tbl)

  res_plot <- get_gene_tpm_boxplot(gene_tpm_boxplot_tbl)

  print(res_plot)
}

#* [Not implemented] Get a single-gene all-diseases TPM summary table
#*
#* @tag "Bulk tissue gene expression"
#* @param ensemblId:str a single character value of gene ENSG ID.
#* @serializer json
#* @get /tpm/gene-all-cancer/json
function(ensemblId) {
  res_tbl <- list(ensemblId = ensemblId)
  return(res_tbl)
}

#* [Not implemented] Get a single-gene all-diseases TPM boxplot
#*
#* @tag "Bulk tissue gene expression"
#* @param ensemblId:str a single character value of gene ENSG ID.
#* @serializer png list(res = 300, width = 3900, height = 2700)
#* @get /tpm/gene-all-cancer/plot
function(ensemblId) {
  res_plot <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) +
    ggplot2::geom_point() +
    ggplot2::ggtitle(ensemblId)

  print(res_plot)
}
