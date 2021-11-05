# get_tpm_endpoint_tbl.R defines a function get_tpm_endpoint_tbl to return a
# tibble for /tpm/ endpoints.
#
# Call sequence:
#
# - docker run calls Rscript --vanilla main.R
# - ../main.R calls source("src/get_tpm_endpoint_tbl.R")
#
# Defined variables:
#
# - get_tpm_endpoint_tbl



# Get a tibble for a /tpm/ endpoint
#
# Args:
# - ensg_id: a single character value of gene ENSG ID. Required.
# - efo_id: NULL or a single character value of EFO ID. Required.
# - include_tumor_desc: a single character value of the following choices.
#   Required.
#   - "primaryOnly"
#   - "relapseOnly"
#   - "primaryAndRelapseInSameBox"
#   - "primaryAndRelapseInDifferentBoxes"
# - gtex_sample_group: a single character value passed to get_gene_tpm_tbl
# - gtex_histology_group: a single character value passed to
#   get_gene_tpm_boxplot_tbl.
# - min_n_per_box: a single numeric value passed to get_gene_tpm_boxplot_tbl.
#
# Returns a tibble for generating an endpoint response.
get_tpm_endpoint_tbl <- function(ensg_id, efo_id, include_tumor_desc,
                                 gtex_sample_group,
                                 gtex_histology_group = "tissue_subgroup",
                                 min_n_per_box = 1L) {
  stopifnot(is.character(include_tumor_desc))
  stopifnot(identical(length(include_tumor_desc), 1L))

  if (include_tumor_desc == "primaryOnly") {
    relapse_sample_group <- "exclude"

    spec_desc_group <- "primary_only_group"
  } else {
    relapse_sample_group <- "require"

    if (include_tumor_desc == "relapseOnly") {
      spec_desc_group <- "relapse_only_group"

    } else if (include_tumor_desc == "primaryAndRelapseInSameBox") {
      spec_desc_group <- "primary_and_relapse_same_group"

    } else if (include_tumor_desc == "primaryAndRelapseInDifferentBoxes") {
      spec_desc_group <- "primary_and_relapse_different_groups"

    } else {
      stop(paste(
        "Internal error: Not implemented include_tumor_desc",
        include_tumor_desc))
    }
  }

  gene_tpm_tbl <- get_gene_tpm_tbl(
    ensg_id = ensg_id, gtex_sample_group = gtex_sample_group,
    relapse_sample_group = relapse_sample_group, efo_id = efo_id)

  gene_tpm_boxplot_tbl <- get_gene_tpm_boxplot_tbl(
    gene_tpm_tbl, spec_desc_group = spec_desc_group,
    gtex_histology_group = gtex_histology_group,
    min_n_per_box = min_n_per_box)

  return(gene_tpm_boxplot_tbl)
}
