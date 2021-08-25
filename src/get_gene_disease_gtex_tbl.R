# get_gene_disease_gtex_tbl.R defines a function
# get_gene_disease_gtex_tbl to return a tibble for plotting a
# single-cancer all-gtex-subgroup TPM boxplot.
#
# Call sequence:
#
# - docker run calls Rscript --vanilla main.R
# - ../main.R calls source("src/get_gene_disease_gtex_tbl.R")
#
# Defined variables:
#
# - get_gene_disease_gtex_tbl


# Get a single-gene single-disease all-gtex-subgroups TPM tibble.
#
# Args:
# - tpm_data_lists: tpm_data_lists defined in src/tpm_data_lists.R.
# - ensg_id: a single character value of gene ENSG ID.
# - gene_symbol: a single character value of gene symbol.
# - efo_id: a single character value of EFO ID.
#
# Returns a tibble with the following columns:
# - Kids_First_Biospecimen_ID: a single Kids_First_Biospecimen_ID
# - EFO: a single EFO ID
# - MONDO: a single MONDO ID
# - Disease: a single Disease/cancer_group
# - GTEx_tissue_subgroup_UBERON: a single GTEx tissue subgroup UBERON ID
# - GTEx_tissue_subgroup: a single GTEx tissue subgroup
# - TPM: a single TPM value
# - Gene_Ensembl_ID: a single ENSG ID
# - Gene_symbol: a single gene symbol
# - RMTL: a single RMTL value
#
# Note on requiring both ensg_id and gene_symbol to align PedOT with
# OpenPedCan-analysis: One ENSG ID can map to multiple gene symbols in the
# OpenPedCan-analysis release TPM data frame, e.g. ENSG00000273032 maps to DGCR5
# and DGCR9. Similarly, one gene symbol can map to multiple ENSG IDs in the
# OpenPedCan-analysis release TPM data frame, e.g. DGCR5 maps to
# ENSG00000273032, ENSG00000237517 and ENSG00000283406. One tuple of gene ENSG
# ID and symbol is unique. If gene_symbol cannot be provided when querying API,
# change the interface to take only ensg_id, and handle duplicates with one of
# the following potential options:
# - Identify the gene symbol that match PedOT.
# - Select a gene symbol, but the selected one may not match PedOT.
# - Completely drop gene_symbol, as it is also shown on PedOT.
get_gene_disease_gtex_tbl <- function(tpm_data_lists, ensg_id, gene_symbol,
                                      efo_id) {
  stopifnot(is.character(ensg_id))
  stopifnot(is.character(gene_symbol))
  stopifnot(is.character(efo_id))
  stopifnot(identical(length(ensg_id), as.integer(1)))
  stopifnot(identical(length(gene_symbol), as.integer(1)))
  stopifnot(identical(length(efo_id), as.integer(1)))

  long_tpm_tbl_list <- lapply(tpm_data_lists, function(xl) {

    # Each row is Gene_symbol, Kids_First_Biospecimen_ID (prev colname), tpm
    long_tpm_tbl <- tidyr::pivot_longer(
      xl$tpm_df, !Gene_symbol, names_to = "Kids_First_Biospecimen_ID",
      values_to = "tpm")
    if (DEBUG) {
      stopifnot(identical(sum(is.na(long_tpm_tbl)), as.integer(0)))
      stopifnot(identical(
        sort(unique(long_tpm_tbl$Gene_symbol)),
        sort(unique(xl$tpm_df$Gene_symbol))))
      stopifnot(identical(
        sort(unique(c("Gene_symbol", long_tpm_tbl$Kids_First_Biospecimen_ID))),
        sort(unique(colnames(xl$tpm_df)))))
    }
  })

}
