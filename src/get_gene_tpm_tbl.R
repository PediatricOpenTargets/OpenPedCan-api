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


# Get a TPM tibble of a single-gene, one or more disease group(s), and zero or
# more GTEx tissue subgroup(s).
#
# Args:
# - tpm_data_lists: tpm_data_lists loaded from the tpm_data_lists.rds output of
#   db/tpm_data_lists.R. Required.
# - ensg_id: a single character value of gene ENSG ID. Required.
# - efo_id: NULL or a single character value of EFO ID. Default is NULL, which
#   is to include all diseases and zero GTEx tissue, aka gene-all-cancer. If
#   efo_id is not NULL, include all samples that have the EFO ID and all GTEx
#   tissues, aka gene-disease-gtex.
# - gene_symbol: NULL or a single character value of gene symbol. Default is
#   NULL, which is to select the first sorted gene symbol when one ENSG ID maps
#   to multiple gene symbols. If gene_symbol is not NULL, the (efo_id,
#   gene_symbol) tuple is selected when one ENSG ID maps to multiple gene
#   symbols.
# - min_n_per_sample_group: a single numeric value of the minimum number of
#   samples per Disease or GTEx_tissue_subgroup. Default is 1.
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
get_gene_tpm_tbl <- function(tpm_data_lists, ensg_id, efo_id = NULL,
                             gene_symbol = NULL, min_n_per_sample_group = 1L) {
  # Adapted from https://stackoverflow.com/a/38539734/4638182
  stopifnot(inherits(tpm_data_lists, "list"))

  stopifnot(is.character(ensg_id))
  stopifnot(identical(length(ensg_id), 1L))
  stopifnot(!is.na(ensg_id))

  if (!is.null(efo_id)) {
    stopifnot(is.character(efo_id))
    stopifnot(identical(length(efo_id), 1L))
    stopifnot(!is.na(efo_id))
  }

  if (!is.null(gene_symbol)) {
    stopifnot(is.character(gene_symbol))
    stopifnot(identical(length(gene_symbol), 1L))
    stopifnot(!is.na(gene_symbol))
  }

  stopifnot(is.numeric(min_n_per_sample_group))
  stopifnot(identical(length(min_n_per_sample_group), 1L))
  stopifnot(!is.na(min_n_per_sample_group))

  all_cohorts_str_id <- "all_cohorts"

  long_tpm_tbl_list <- purrr::imap(tpm_data_lists, function(xl, xname) {
    # .data and .env are from rlang package, but they do not need to be imported
    # to work. "The .data pronoun is automatically created for you by
    # data-masking functions using the tidy eval framework." This should also
    # apply to .env.
    #
    # - .data "retrieves data-variables from the data frame".
    # - .env "retrieves env-variables from the environment".
    # - Ref: https://rlang.r-lib.org/reference/tidyeval-data.html
    #
    # tpm_df cols: Gene_Ensembl_ID, Gene_symbol, RMTL, Sample1, Sample2, ...
    ensg_tpm_df <- dplyr::filter(
      xl$tpm_df, .data$Gene_Ensembl_ID == .env$ensg_id)

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

        ensg_tpm_df <- dplyr::filter(
          ensg_tpm_df, .data$Gene_symbol == .env$gene_symbol)

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
      ensg_tpm_df <- dplyr::slice(
        dplyr::arrange(ensg_tpm_df, .data$Gene_symbol),
        1L)
    }

    if (DEBUG) {
      stopifnot(identical(nrow(ensg_tpm_df), 1L))
    }

    # The annotation columns of ensg_tpm_df, all other columns must be samples
    # TPM values.
    ensg_tpm_df_ann_cols <- c("Gene_symbol", "RMTL", "Gene_Ensembl_ID")

    # Subset samples
    if (identical(xname, "gtex")) {
      if (is.null(efo_id)) {
        # all-diseases table and plot
        #
        # - keep zero gtex subgroups
        selected_sids <- character()
      } else {
        # disease vs gtex table and plot
        #
        # - keep all gtex subgroups that have >= min_n_per_sample_group samples
        selected_sids <- dplyr::filter(
          dplyr::add_count(
            xl$histology_df, GTEx_tissue_subgroup,
            name = "gtex_subgroup_n"),
          gtex_subgroup_n >= min_n_per_sample_group)$Kids_First_Biospecimen_ID
      }
    } else {
      disease_n_histology_df <- dplyr::filter(
        dplyr::add_count(xl$histology_df, Disease, name = "disease_n"),
        disease_n >= min_n_per_sample_group)

      if (is.null(efo_id)) {
        # all-diseases table and plot
        #
        # - keep all cancer groups that have >= min_n_per_sample_group samples
        selected_sids <- disease_n_histology_df$Kids_First_Biospecimen_ID
      } else {
        # disease vs gtex table and plot
        #
        # - keep all cancer groups that are mapped to input efo_id and have >=
        #   min_n_per_sample_group samples
        selected_sids <- dplyr::filter(
          disease_n_histology_df, EFO == efo_id)$Kids_First_Biospecimen_ID
      }
    }

    ensg_tpm_df <- ensg_tpm_df[, c(ensg_tpm_df_ann_cols, selected_sids)]

    if (DEBUG) {
      # Assert all sample columns are numeric.
      purrr::walk(
        dplyr::select(ensg_tpm_df, !dplyr::all_of(ensg_tpm_df_ann_cols)),
        function(xcol) {
          stopifnot(is.numeric(xcol))
        }
      )
    }

    # initial ensg_long_tpm_tbl column names
    init_ensg_long_tpm_tbl_cols <- c(ensg_tpm_df_ann_cols,
                                     "Kids_First_Biospecimen_ID", "TPM")

    if (identical(sort(colnames(ensg_tpm_df)), sort(ensg_tpm_df_ann_cols))) {
      # no sample selected
      #
      # For gtex, use empty tibble.
      #
      # For disease, raise error.
      if (identical(xname, "gtex")) {
        # Create empty tibble.
        ensg_long_tpm_tbl <- purrr::map_dfc(
          init_ensg_long_tpm_tbl_cols,
          function(xcol) {
            empty_tbl <- tibble::tibble()
            if (identical(xcol, "TPM")) {
              empty_tbl[, xcol] <- numeric()
            } else {
              empty_tbl[, xcol] <- character()
            }
            return(empty_tbl)
          }
        )
      } else {
        if (is.null(efo_id)) {
          stop(paste("No Disease has >=", min_n_per_sample_group, "samples."))
        } else {
          stop(paste(
            efo_id, "has no Disease with >=", min_n_per_sample_group,
            "samples."))
        }
      }
    } else {
      # >= 1 sample selected

      # Each row of ensg_long_tpm_tbl is Gene_Ensembl_ID, Gene_symbol,
      # Kids_First_Biospecimen_ID (prev colname), TPM.
      ensg_long_tpm_tbl <- tidyr::pivot_longer(
        ensg_tpm_df, !dplyr::all_of(ensg_tpm_df_ann_cols),
        names_to = "Kids_First_Biospecimen_ID", values_to = "TPM")
    }

    # Ensure constent column names of ensg_long_tpm_tbl.
    ensg_long_tpm_tbl <- dplyr::select(
      ensg_long_tpm_tbl, dplyr::all_of(init_ensg_long_tpm_tbl_cols))

    if (DEBUG) {
      stopifnot(identical(
        colnames(ensg_long_tpm_tbl), init_ensg_long_tpm_tbl_cols))
      stopifnot(identical(
        sum(is.na(dplyr::select(ensg_long_tpm_tbl, -RMTL))), 0L))

      if (nrow(ensg_long_tpm_tbl) > 0) {
        stopifnot(identical(
          sort(unique(ensg_long_tpm_tbl$Gene_symbol)),
          sort(unique(ensg_tpm_df$Gene_symbol))))
      }

      stopifnot(identical(
        sort(unique(
          c("Gene_Ensembl_ID", "Gene_symbol", "RMTL",
            ensg_long_tpm_tbl$Kids_First_Biospecimen_ID))),
        sort(unique(colnames(ensg_tpm_df)))
      ))
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

      stopifnot(identical(sum(is.na(ensg_long_tpm_tbl$cohort)), 0L))
      stopifnot(!all_cohorts_str_id %in% ensg_long_tpm_tbl$cohort)
      if (identical(xname, "gtex")) {
        if (is.null(efo_id)) {
          # gene-all-cancer
          stopifnot(identical(nrow(ensg_long_tpm_tbl), 0L))
        }

        stopifnot(identical(
          sum(is.na(ensg_long_tpm_tbl$GTEx_tissue_subgroup)), 0L))

        stopifnot(identical(sum(!is.na(ensg_long_tpm_tbl$EFO)), 0L))

        stopifnot(identical(sum(!is.na(ensg_long_tpm_tbl$Disease)), 0L))
      } else {
        if (!is.null(efo_id)) {
          # gene-disease-gtex
          stopifnot(identical(unique(ensg_long_tpm_tbl$EFO), efo_id))
        }

        stopifnot(identical(sum(is.na(ensg_long_tpm_tbl$EFO)), 0L))

        stopifnot(identical(
          sum(!is.na(ensg_long_tpm_tbl$GTEx_tissue_subgroup)), 0L))

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
    long_tpm_tbl_list$pt_each_cohort, long_tpm_tbl_list$gtex)
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

  if (nrow(pt_aci_disease_n_uniq_cohorts_g1_tbl) > 0) {
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
  }

  return(res_long_tpm_tbl)
}
