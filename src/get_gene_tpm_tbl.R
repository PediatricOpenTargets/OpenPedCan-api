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

# Design notes:
#
# - gtex_sample_group and relapse_sample_group parameters are designed to be
#   required to prevent complex default value behaviors.
# - gtex_sample_group and relapse_sample_group parameters are designed to take
#   character values to allow additional choices, e.g. subset certain tissue
#   subgroups or tumor descriptors, at a later point. Therefore, the name of the
#   parameter is gtex_sample_group rather than include_gtex_sample_group, which
#   would take a boolean value.

# Get a TPM tibble of a single-gene, one or more disease group(s), and zero or
# more GTEx tissue subgroup(s).
#
# Args:
# - ensg_id: a single character value of gene ENSG ID. Required.
# - gtex_sample_group: a single character value with the following choices.
#   Required.
#   - "exclude": Exclude GTEx samples. Does NOT raise error if there is no GTEx
#     sample.
#   - "require": Require all GTEx samples. Raise error if there is no GTEx
#     sample.
# - relapse_sample_group: a single character value with the following choices.
#   Required.
#   - "exclude": Exclude relapse tumors. Does NOT raise error if there is no
#     relapse tumor.
#   - "require": Require all relapse tumors. Raise error if there is no relapse
#     tumor.
# - efo_id: NULL or a single character value of EFO ID. Default is NULL, which
#   is to include all diseases and zero GTEx tissue, aka gene-all-cancer. If
#   efo_id is not NULL, include all samples that have the EFO ID and all GTEx
#   tissues, aka gene-disease-gtex.
# - gene_symbol: NULL or a single character value of gene symbol. Default is
#   NULL, which is to select the first sorted gene symbol when one ENSG ID maps
#   to multiple gene symbols. If gene_symbol is not NULL, the (efo_id,
#   gene_symbol) tuple is selected when one ENSG ID maps to multiple gene
#   symbols.
#
# Returns a tibble with the following columns:
# - Kids_First_Biospecimen_ID: a single Kids_First_Biospecimen_ID
# - cohort: a single cohort. If the input efo_id has more than one histology_df
#   cohorts, e.g. GMKF and TARGET, include an additional cohort "All Cohorts" in
#   the result table.
# - EFO: a single EFO ID
# - MONDO: a single MONDO ID
# - Disease: a single Disease/cancer_group
# - GTEx_tissue_subgroup_UBERON: a single GTEx tissue subgroup UBERON ID
# - GTEx_tissue_subgroup: a single GTEx tissue subgroup
# - specimen_descriptor: a single specimen descriptor.
# - TPM: a single TPM value
# - Gene_Ensembl_ID: a single ENSG ID
# - Gene_symbol: a single gene symbol
# - PMTL: a single PMTL value
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
get_gene_tpm_tbl <- function(ensg_id, gtex_sample_group, relapse_sample_group,
                             efo_id = NULL, gene_symbol = NULL) {
  stopifnot(is.character(ensg_id))
  stopifnot(identical(length(ensg_id), 1L))
  stopifnot(!is.na(ensg_id))

  stopifnot(is.character(gtex_sample_group))
  stopifnot(identical(length(gtex_sample_group), 1L))
  stopifnot(gtex_sample_group %in% c("require", "exclude"))

  stopifnot(is.character(relapse_sample_group))
  stopifnot(identical(length(relapse_sample_group), 1L))
  stopifnot(relapse_sample_group %in% c("require", "exclude"))

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

  # In OpenPedCan-analysis project, "All Cohorts" is used as the cohort of
  # combined cohorts. The value "All Cohorts" may be changed at a later point.
  # If "All Cohorts" is changed to some other value. search the whole code base
  # and make relevant chagnes.
  all_cohorts_str_id <- "All Cohorts"  # nolint: object_usage_linter.

  # Query database.
  #
  # connect_db and db_env_vars are coming from main.R.
  conn <- connect_db(db_env_vars)  # nolint: object_usage_linter.

  # Case insensitive db schema and table names. DBI/glue quotes names. Table
  # columns are case sensitive.
  q_schema <- tolower(
    db_env_vars$BULK_EXP_SCHEMA)  # nolint: object_usage_linter.
  q_table <- tolower(
    db_env_vars$BULK_EXP_TPM_HISTOLOGY_TBL)  # nolint: object_usage_linter.

  # Use parameterized queries to protect queries from SQL injection attacks.
  #
  # https://db.rstudio.com/best-practices/run-queries-safely/#parameterized-queries
  q_rs <- DBI::dbSendQuery(
    conn,
    glue::glue_sql("
      SELECT *
      FROM {`q_schema`}.{`q_table`}
      WHERE \"Gene_Ensembl_ID\" = ?
    ", .con = conn)
  )
  # Bind ? in the query statment with values.
  DBI::dbBind(q_rs, list(ensg_id))
  # "dbFetch() always returns a data.frame with as many rows as records were
  # fetched and as many columns as fields in the result set, even if the result
  # is a single value or has one or zero rows."
  #
  # Ref: https://dbi.r-dbi.org/reference/dbfetch
  q_rs_df <- DBI::dbFetch(q_rs)
  DBI::dbClearResult(q_rs)
  DBI::dbDisconnect(conn)

  long_tpm_tbl <- tibble::as_tibble(q_rs_df)

  # Raise error if table is empty, i.e. no data available.
  stopifnot(nrow(long_tpm_tbl) > 0)

  # Raise error if table is not expected.
  stopifnot(identical(
    colnames(long_tpm_tbl),
    c("Kids_First_Biospecimen_ID", "cohort", "EFO", "MONDO",
      "Disease", "GTEx_tissue_subgroup_UBERON", "GTEx_tissue_subgroup",
      "specimen_descriptor", "TPM", "Gene_Ensembl_ID", "Gene_symbol", "PMTL")
  ))
  # Assert column types are expected.
  placeholder_res <- purrr::imap_lgl(long_tpm_tbl, function(xcol, xcolname) {
    if (xcolname == "TPM") {
      stopifnot(is.numeric(xcol))
    } else {
      stopifnot(is.character(xcol))
    }
    return(TRUE)
  })
  # Assert input ensg_id is the only ENSG ID.
  stopifnot(identical(unique(long_tpm_tbl$Gene_Ensembl_ID), ensg_id))
  # Assert no NA in required columns.
  stopifnot(identical(
    sum(is.na(
      dplyr::select(
        long_tpm_tbl, Kids_First_Biospecimen_ID, cohort, specimen_descriptor,
        TPM, Gene_Ensembl_ID, Gene_symbol))),
    0L
  ))
  # Assert no duplicated (sample, gene) tuple.
  stopifnot(identical(
    nrow(dplyr::distinct(dplyr::select(long_tpm_tbl, -TPM))),
    nrow(long_tpm_tbl)
  ))

  # .data and .env are from rlang package, but they do not need to be imported
  # to work. "The .data pronoun is automatically created for you by data-masking
  # functions using the tidy eval framework." This should also apply to .env.
  #
  # - .data "retrieves data-variables from the data frame".
  # - .env "retrieves env-variables from the environment".
  # - Ref: https://rlang.r-lib.org/reference/tidyeval-data.html
  #
  # tpm_df cols: Gene_Ensembl_ID, Gene_symbol, PMTL, Sample1, Sample2, ...

  # Handle one ENSG ID mapping to more than one symbols
  #
  # Predicates at this point:
  #
  # - long_tpm_tbl has > 0 rows.
  # - ensg_id is the only ENSG ID.
  # - long_tpm_tbl$Gene_symbol has no NA.
  ltt_uniq_gene_symbols <- unique(long_tpm_tbl$Gene_symbol)

  if (DEBUG) {
    stopifnot(is.character(ltt_uniq_gene_symbols))
    stopifnot(length(ltt_uniq_gene_symbols) > 0)
  }

  if (length(ltt_uniq_gene_symbols) > 1) {
    # ensg id is mapped to multiple symbols
    if (!is.null(gene_symbol)) {
      stopifnot(is.character(gene_symbol))
      stopifnot(identical(length(gene_symbol), 1L))
      stopifnot(!is.na(gene_symbol))

      long_tpm_tbl <- dplyr::filter(
        long_tpm_tbl, .data$Gene_symbol == .env$gene_symbol)

      if (nrow(long_tpm_tbl) == 0) {
        stop(paste(ensg_id, gene_symbol, "is not available."))
      }
    } else {
      first_sorted_ltt_uniq_gene_symbol <- dplyr::first(
        sort(ltt_uniq_gene_symbols))

      long_tpm_tbl <- dplyr::filter(
        long_tpm_tbl,
        .data$Gene_symbol == .env$first_sorted_ltt_uniq_gene_symbol)
    }
  }

  if (DEBUG) {
    stopifnot(identical(length(unique(long_tpm_tbl$Gene_symbol)), 1L))
  }

  # Subset samples.

  # The following predicates are asserted in the database building procedure:
  #
  # - If Disease is NA, GTEx_tissue_subgroup is not NA.
  # - If GTEx_tissue_subgroup is NA, Disease is not NA.
  # - If Disease is not NA, EFO is not NA.
  if (DEBUG) {
    # xor evaluates to TRUE if two values are different
    stopifnot(all(xor(
      is.na(long_tpm_tbl$Disease),
      is.na(long_tpm_tbl$GTEx_tissue_subgroup)
    )))
    stopifnot(identical(
      is.na(long_tpm_tbl$Disease),
      is.na(long_tpm_tbl$EFO)
    ))
  }

  # Subset Diseases (aka cancer groups)
  #
  # Separate gtex and disease tables to simplify different procedures for
  # handling Diseases and GTEx tissues.
  disease_long_tpm_tbl <- dplyr::filter(
    long_tpm_tbl, !is.na(.data$Disease))  # nolint: object_usage_linter.

  # specimen_descriptor is asserted above to have no NA
  if (relapse_sample_group == "exclude") {
    disease_long_tpm_tbl <- dplyr::filter(
      disease_long_tpm_tbl, specimen_descriptor != "Relapse Tumor")
  } else if (relapse_sample_group == "require") {
    # Raise error if no relapse sample, i.e., data not available. Raising error
    # is favored over analyzing without required samples, by design.
    stopifnot(any(disease_long_tpm_tbl$specimen_descriptor == "Relapse Tumor"))
  } else {
    stop(paste0(
      "Not implemented relapse_sample_group value ", relapse_sample_group))
  }

  # Raise error if no Disease, i.e. data not available.
  stopifnot(nrow(disease_long_tpm_tbl) > 0)

  if (!is.null(efo_id)) {
    # Keep only Disease (aka cancer group) that is mapped to input efo_id.
    disease_long_tpm_tbl <- dplyr::filter(
      disease_long_tpm_tbl, .data$EFO == .env$efo_id)

    # Raise error if no Disease passes the filter, i.e., data not available.
    stopifnot(nrow(disease_long_tpm_tbl) > 0)
  }

  if (gtex_sample_group == "require") {
    gtex_long_tpm_tbl <- dplyr::filter(
      long_tpm_tbl, !is.na(.data$GTEx_tissue_subgroup))

    # Raise error if no GTEx_tissue_subgroup, i.e., data not available.
    stopifnot(nrow(gtex_long_tpm_tbl) > 0)

    if (DEBUG) {
      stopifnot(identical(
        colnames(disease_long_tpm_tbl),
        colnames(gtex_long_tpm_tbl)
      ))
    }

    long_tpm_tbl <- dplyr::bind_rows(disease_long_tpm_tbl, gtex_long_tpm_tbl)
  } else if (gtex_sample_group == "exclude") {
    # Exclude gtex samples.
    long_tpm_tbl <- disease_long_tpm_tbl
  } else {
    stop(paste0(
      "Not implemented gtex_sample_group value ", gtex_sample_group))
  }

  # Let return table have the same colnames and order.
  long_tpm_tbl <- dplyr::select(
    long_tpm_tbl,
    dplyr::all_of(
      c("Kids_First_Biospecimen_ID", "cohort", "EFO", "MONDO",
        "Disease", "GTEx_tissue_subgroup_UBERON", "GTEx_tissue_subgroup",
        "specimen_descriptor", "TPM", "Gene_Ensembl_ID", "Gene_symbol",
        "PMTL")
    )
  )

  if (DEBUG) {
    stopifnot(nrow(long_tpm_tbl) > 0)
    stopifnot(identical(sum(is.na(long_tpm_tbl$cohort)), 0L))
    stopifnot(identical(sum(is.na(long_tpm_tbl$specimen_descriptor)), 0L))

    all_cohorts_long_tpm_tbl <- dplyr::filter(
      long_tpm_tbl, .data$cohort == .env$all_cohorts_str_id)

    each_cohort_long_tpm_tbl <- dplyr::filter(
      long_tpm_tbl, .data$cohort != .env$all_cohorts_str_id)

    # all_cohorts_long_tpm_tbl and each_cohort_long_tpm_tbl cannot have
    # duplicated Kids_First_Biospecimen_ID. All Kids_First_Biospecimen_IDs must
    # be unique.
    stopifnot(identical(
      nrow(all_cohorts_long_tpm_tbl),
      length(unique(all_cohorts_long_tpm_tbl$Kids_First_Biospecimen_ID))))

    stopifnot(identical(
      nrow(each_cohort_long_tpm_tbl),
      length(unique(each_cohort_long_tpm_tbl$Kids_First_Biospecimen_ID))))

    if (gtex_sample_group == "exclude") {
      # gene-all-cancer
      stopifnot(all(is.na(long_tpm_tbl$GTEx_tissue_subgroup)))
      stopifnot(all(!is.na(long_tpm_tbl$EFO)))
      stopifnot(all(!is.na(long_tpm_tbl$Disease)))
    } else if (gtex_sample_group == "require") {
      # gene-disease-gtex
      if (!is.null(efo_id)) {
        stopifnot(identical(
          efo_id,
          purrr::discard(unique(long_tpm_tbl$EFO), is.na)
        ))
      }
      # gene-all-cancer-gtex
      stopifnot(identical(is.na(long_tpm_tbl$EFO), is.na(long_tpm_tbl$Disease)))
      stopifnot(sum(!is.na(long_tpm_tbl$EFO)) > 0)
      stopifnot(sum(!is.na(long_tpm_tbl$GTEx_tissue_subgroup)) > 0)
      # xor evaluates to TRUE if two values are different
      stopifnot(all(xor(
        is.na(long_tpm_tbl$Disease),
        is.na(long_tpm_tbl$GTEx_tissue_subgroup)
      )))
    } else {
      stop(paste0(
        "Not implemented gtex_sample_group value ", gtex_sample_group))
    }
  }

  return(long_tpm_tbl)
}
