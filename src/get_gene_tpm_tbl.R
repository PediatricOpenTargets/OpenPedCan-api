# get_gene_tpm_tbl.R defines a function get_gene_tpm_tbl to return a TPM tibble
# of a single-gene, one or more diseasees, and zero or more GTEx tissue
# subgroups
#
# Call sequence:
#
# - docker run calls Rscript --vanilla main.R
# - ../main.R calls source("src/get_gene_tpm_tbl.R")
#
# Defined variables:
#
# - get_gene_tpm_tbl


# Get a TPM tibble of a single-gene, one or more diseasees, and zero or more
# GTEx tissue(s).
#
# Args:
# - tpm_data_lists: tpm_data_lists defined in src/tpm_data_lists.R.
# - ensg_id: a single character value of gene ENSG ID.
# - efo_id: a single character value of EFO ID.
# - gene_symbol: an optional single character value of gene symbol.
#
# Returns a tibble with the following columns:
# - Kids_First_Biospecimen_ID: a single Kids_First_Biospecimen_ID
# - cohort: a single cohort. If the input efo_id has more than one histology_df
#   cohorts, e.g. GMKF and TARGET, include an additional cohort "all_cohorts" in
#   the result table.
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
# Note on havng both ensg_id and gene_symbol to align PedOT with
# OpenPedCan-analysis: One ENSG ID can map to multiple gene symbols in the
# OpenPedCan-analysis release TPM data frame, e.g. ENSG00000273032 maps to DGCR5
# and DGCR9. Similarly, one gene symbol can map to multiple ENSG IDs in the
# OpenPedCan-analysis release TPM data frame, e.g. DGCR5 maps to
# ENSG00000273032, ENSG00000237517 and ENSG00000283406. One tuple of gene ENSG
# ID and symbol is unique. If gene_symbol cannot be provided when querying API,
# change the interface to take only ensg_id, and handle duplicates with one of
# the following potential options:
# - (Implemented) Select the first of sorted gene symbols, but the selected one
#   may not match PedOT.
# - Identify the gene symbol that match PedOT.
# - Completely drop gene_symbol, as it is also shown on PedOT.
get_gene_tpm_tbl <- function(tpm_data_lists, ensg_id, efo_id,
                             gene_symbol = NULL) {
  stopifnot(is.character(ensg_id))
  stopifnot(is.character(efo_id))
  stopifnot(identical(length(ensg_id), 1L))
  stopifnot(identical(length(efo_id), 1L))
  stopifnot(!is.na(ensg_id))
  stopifnot(!is.na(efo_id))

  all_cohorts_str_id <- "all_cohorts"

  long_tpm_tbl_list <- purrr::imap(tpm_data_lists, function(xl, xname) {
    # tpm_df cols: Gene_Ensembl_ID, Gene_symbol, RMTL, Sample1, Sample2, ...
    ensg_tpm_df <- dplyr::filter(xl$tpm_df, Gene_Ensembl_ID == ensg_id)

    if (identical(nrow(ensg_tpm_df), 0L)) {
      # ensg id is not in tpm df
      stop(paste(ensg_id, "is not available."))
    }

    if (!identical(nrow(ensg_tpm_df), 1L)) {
      # ensg id is mapped to multiple symbols
      if (!is.null(gene_symbol)) {
        stopifnot(is.character(gene_symbol))
        stopifnot(identical(length(gene_symbol), 1L))
        stopifnot(!is.na(gene_symbol))

        ensg_tpm_df <- dplyr::filter(ensg_tpm_df, Gene_symbol == gene_symbol)

        if (identical(nrow(ensg_tpm_df), 0L)) {
          stop(paste(ensg_id, gene_symbol, "is not available."))
        }
      }
    }

    if (!identical(nrow(ensg_tpm_df), 1L)) {
      # This branch could be reached by
      # - is.null(gene_symbol): one ensg mapped to multiple symbols
      # - !is.null(gene_symbol): (ensg, symbol) tuple is duplicated in tpm_df
      #
      # Either way, the selection method works.
      ensg_tpm_df <- dplyr::slice(dplyr::arrange(ensg_tpm_df, Gene_symbol), 1L)
    }

    if (DEBUG) {
      stopifnot(identical(nrow(ensg_tpm_df), 1L))
    }

    if (!identical(xname, "gtex")) {
      efo_selected_sids <- dplyr::filter(
        xl$histology_df, EFO == efo_id)$Kids_First_Biospecimen_ID

      min_n_samples <- 1
      if (length(efo_selected_sids) < min_n_samples) {
        stop(paste(
          efo_id, "has", length(efo_selected_sids), "samples, which",
          "is less than the minimum", min_n_samples, "requirements."))
      }

      efo_selected_columns <- c("Gene_Ensembl_ID", "Gene_symbol", "RMTL",
                                efo_selected_sids)
      ensg_tpm_df <- ensg_tpm_df[, efo_selected_columns]
    }

    # Each row is Gene_symbol, Kids_First_Biospecimen_ID (prev colname), tpm
    ensg_long_tpm_tbl <- tidyr::pivot_longer(
      ensg_tpm_df, !dplyr::all_of(c("Gene_Ensembl_ID", "Gene_symbol", "RMTL")),
      names_to = "Kids_First_Biospecimen_ID", values_to = "TPM")

    if (DEBUG) {
      stopifnot(identical(sum(is.na(ensg_long_tpm_tbl)), 0L))
      stopifnot(identical(
        sort(unique(ensg_long_tpm_tbl$Gene_symbol)),
        sort(unique(xl$tpm_df$Gene_symbol))))
      stopifnot(identical(
        sort(unique(
          c("Gene_Ensembl_ID", "Gene_symbol", "RMTL",
            ensg_long_tpm_tbl$Kids_First_Biospecimen_ID))),
        sort(unique(colnames(xl$tpm_df)))))
    }

    ensg_long_tpm_tbl <- dplyr::left_join(
      ensg_long_tpm_tbl, xl$histology_df, by = "Kids_First_Biospecimen_ID")
    ensg_long_tpm_tbl <- dplyr::select(
      ensg_long_tpm_tbl, "Kids_First_Biospecimen_ID", "cohort", "EFO", "MONDO",
      "Disease", "GTEx_tissue_subgroup_UBERON", "GTEx_tissue_subgroup", "TPM",
      "Gene_Ensembl_ID", "Gene_symbol", "RMTL")

    if (DEBUG) {
      stopifnot(identical(
        nrow(ensg_long_tpm_tbl),
        length(unique(ensg_long_tpm_tbl$Kids_First_Biospecimen_ID))))

      stopifnot(identical(sum(!is.na(ensg_long_tpm_tbl$cohort)), 0L))
      stopifnot(!all_cohorts_str_id %in% ensg_long_tpm_tbl$cohort)
      if (!identical(xname, "gtex")) {
        stopifnot(identical(
          sum(is.na(ensg_long_tpm_tbl$GTEx_tissue_subgroup)), 0L))
        stopifnot(identical(sum(!is.na(ensg_long_tpm_tbl$EFO)), 0L))
        stopifnot(identical(sum(!is.na(ensg_long_tpm_tbl$Disease)), 0L))
      } else {
        stopifnot(identical(unique(ensg_long_tpm_tbl$EFO), efo_id))
        stopifnot(identical(
          sum(!is.na(ensg_long_tpm_tbl$GTEx_tissue_subgroup)), 0L))
        stopifnot(identical(sum(is.na(ensg_long_tpm_tbl$EFO)), 0L))
        stopifnot(identical(sum(is.na(ensg_long_tpm_tbl$Disease)), 0L))
      }
    }

    return(ensg_long_tpm_tbl)
  })

  # Bind pt_all_cohorts, pt_each_cohort, and gtex long_tpm_tbls
  if (DEBUG) {
    stopifnot(identical(
      colnames(long_tpm_tbl_list$gtex),
      colnames(long_tpm_tbl_list$pt_each_cohort)))

    stopifnot(identical(
      colnames(long_tpm_tbl_list$gtex),
      colnames(long_tpm_tbl_list$pt_all_cohorts)))
  }

  res_long_tpm_tbl <- dplyr::bind_rows(
    long_tpm_tbl_list$gtex, long_tpm_tbl_list$pt_each_cohort)
  # Handle all-cohorts/combined-cohorts/all_cohorts.
  #
  # For any Disease that has two or more cohorts, append all_cohorts rows.
  # 
  # Primary tumor all-cohorts independent (disease, n unique cohort > 1) table.
  pt_aci_disease_n_uniq_cohorts_g1_tbl <- dplyr::filter(
    dplyr::summarise(
      dplyr::group_by(long_tpm_tbl_list$pt_all_cohorts, Disease),
      n_uniq_cohorts = length(unique(cohort))),
    n_uniq_cohorts > 1)

  pt_aci_disease_n_uniq_cohorts_g1_long_tpm_tbl <- purrr::map_dfr(
    pt_aci_disease_n_uniq_cohorts_g1_tbl$Disease,
    function(x) {
      x_long_tbl <- dplyr::filter(
        long_tpm_tbl_list$pt_all_cohorts, Disease == x)
      if (DEBUG) {
        stopifnot(nrow(x_long_tbl) > 0)
      }
      x_long_tbl$cohort <- all_cohorts_str_id
      return(x_long_tbl)
    }
  )
  if (DEBUG) {
    stopifnot(identical(
      colnames(pt_aci_disease_n_uniq_cohorts_g1_long_tpm_tbl),
      colnames(res_long_tpm_tbl)))
  }

  res_long_tpm_tbl <- dplyr::bind_rows(
    pt_aci_disease_n_uniq_cohorts_g1_long_tpm_tbl, res_long_tpm_tbl)

  return(res_long_tpm_tbl)
}
