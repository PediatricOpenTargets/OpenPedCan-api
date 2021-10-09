# This file is called by build_db.Dockerfile to load the rds files build a
# database. Note that this file requires env vars defined in
# build_db.Dockerfile.
#
# This file should be run with the directory that contains this file as working
# directory.
#
# Call sequence:
#
# - docker build db/build_tools/build_db.Dockerfile runs tpm_data_lists.R
# - docker build db/build_tools/build_db.Dockerfile runs build_db.R
#
# Outputs:
#
# - ${BUILD_OUTPUT_DIR_PATH}/build_outputs/${BULK_EXP_SCHEMA}_${BULK_EXP_TPM_HISTOLOGY_TBL}.csv
# - Create empty database table
#   ${DB_NAME}.${BULK_EXP_SCHEMA}.${BULK_EXP_TPM_HISTOLOGY_TBL} that have the
#   same columns as the csv file.



# Specify paths and define global variables-------------------------------------

# Get %>% without loading the whole library
`%>%` <- magrittr::`%>%`

# Helper function to get env vars
get_env_var <- function(env_var_name) {
  env_var_val <- Sys.getenv(
    env_var_name, unset = NA_character_, names = FALSE)

  # Assert env_var_val is character of length 1
  stopifnot(is.character(env_var_val))
  stopifnot(identical(length(env_var_val), 1L))

  if (is.na(env_var_val)) {
    stop(paste(
      "Error: Environment variable", env_var_name, "cannot be unset."))
  }

  return(env_var_val)
}

db_r_interface_dir <- file.path(
  get_env_var("DB_HOME_DIR_PATH"), "db/r_interfaces")
stopifnot(dir.exists(db_r_interface_dir))

source(file.path(db_r_interface_dir, "db_env_vars.R"))
source(file.path(db_r_interface_dir, "connect_db.R"))

db_build_output_dir <- get_env_var("BUILD_OUTPUT_DIR_PATH")
stopifnot(dir.exists(db_build_output_dir))

output_fn_prefix <- paste0(
  db_env_vars$BULK_EXP_SCHEMA, "_", db_env_vars$BULK_EXP_TPM_HISTOLOGY_TBL)

# Use .csv rather than .csv.gz to speed up database COPY command.
csv_out_path <- file.path(
  db_build_output_dir, paste0(output_fn_prefix, ".csv"))

# Get DOWN_SAMPLE_DB_GENES env var:
# - 0 or unset: do not down sample genes for database
# - 1: down sample genes for database, for efficient testing.
# - Other values: error.
DOWN_SAMPLE_DB_GENES <- Sys.getenv(
  "DOWN_SAMPLE_DB_GENES", unset = NA_character_, names = FALSE)

stopifnot(is.character(DOWN_SAMPLE_DB_GENES))
stopifnot(identical(length(DOWN_SAMPLE_DB_GENES), 1L))

if (is.na(DOWN_SAMPLE_DB_GENES)) {
  # DOWN_SAMPLE_DB_GENES env var is not set. Set to FALSE by default.
  DOWN_SAMPLE_DB_GENES <- FALSE
} else if (identical(DOWN_SAMPLE_DB_GENES, "0")) {
  DOWN_SAMPLE_DB_GENES <- FALSE
} else if (identical(DOWN_SAMPLE_DB_GENES, "1")) {
  DOWN_SAMPLE_DB_GENES <- TRUE
} else {
  stop(paste(
    "Unknown DOWN_SAMPLE_DB_GENES environtment variable",
    DOWN_SAMPLE_DB_GENES))
}



# Read data --------------------------------------------------------------------
cat("Read data...\n")

tpm_data_lists <- readRDS(file.path(db_build_output_dir, "tpm_data_lists.rds"))



# Function definitions ---------------------------------------------------------

# postgres compatible write csv function.
#
# Args:
#  - x: A data frame or tibble to write to disk.
#  - file: File or connection to write to.
#  - append: If FALSE, will overwrite existing file. If TRUE, will append to
#    existing file. In both cases, if the file does not exist a new file is
#    created.
#  - col_names: If FALSE, column names will not be included at the top of the
#    file. If TRUE, column names will be included.
#
# Returns the input x invisibly.
#
# Notes on readr::write_csv format and postgres COPY format compatibility
#
# - header
#   - COPY HEADER TRUE: "Specifies that the file contains a header line with the
#     names of each column in the file. [...] on input, the first line is
#     ignored."
#   - readr::write_csv writes header only if output csv file does not exist.
# - missing values
#   - COPY NULL: "The default is [...] an unquoted empty string in CSV format."
#   - readr::write_csv na = "" writes nothing to the field; "Missing values will
#     never be quoted; strings with the same value as na will always be quoted."
# - quote
#   - COPY QUOTE: "Specifies the quoting character to be used when a data value
#     is quoted. The default is double-quote."
#   - readr::write_csv also uses double quotes. "quote = 'needed' - Only quote
#     fields which need them."
# - escape
#   - COPY ESCAPE: "Specifies the character that should appear before a data
#     character that matches the QUOTE value. The default is the same as the
#     QUOTE value (so that the quoting character is doubled if it appears in the
#     data)."
#   - readr::write_csv: escape = "double" - quotes are escaped by doubling
#     them."
# - rownames
#   - readr::write_csv: "do not include row names as a column in the written
#     file."
#
# TODO: Validate special values.
# - COPY: "Because backslash is not a special character in the CSV format, \.,
#   the end-of-data marker, could also appear as a data value." readr::write_csv
#   does not quote "\\.", so the following procedure assumes that there is no
#   such value in the tibbles.
# - COPY: "If the value contains the delimiter character, the QUOTE character,
#   the NULL string, a carriage return, or line feed character, then the whole
#   value is prefixed and suffixed by the QUOTE character, and any occurrence
#   within the value of a QUOTE character or the ESCAPE character is preceded by
#   the escape character."
# - COPY: "The CSV format has no standard way to distinguish a NULL value from
#   an empty string. PostgreSQL's COPY handles this by quoting. A NULL is output
#   as the NULL parameter string and is not quoted, while a non-NULL value
#   matching the NULL parameter string is quoted. For example, with the default
#   settings, a NULL is written as an unquoted empty string, while an empty
#   string data value is written with double quotes (""). Reading values follows
#   similar rules. You can use FORCE_NOT_NULL to prevent NULL input comparisons
#   for specific columns."
pgc_write_csv <- function(x, file, append, col_names) {
  readr::write_csv(
    x, file, na = "", append = append, col_names = col_names,
    quote = "needed", escape = "double", progress = FALSE)
}

# Write tibble::tibble or dataframe to database.
#
# Args:
# - df: a tibble::tibble or dataframe with natural row names from 1 to nrow(df).
# - conn: a DBIConnection to a database.
# - schema_name: a single non-blank non-NA character value of the schema name of
#   the table to be written.
# - table_name: a single non-blank non-NA character value of the name of the
#   table to be written.
#
# Returns TRUE invisibly if success.
db_write_table <- function(df, conn, schema_name, table_name, overwrite = FALSE,
                           append = FALSE, field.types = NULL,
                           temporary = FALSE) {
  stopifnot(is.data.frame(df) || tibble::is_tibble(df))

  assert_is_valid_name <- function(x) {
    stopifnot(is.character(x))
    stopifnot(identical(length(x), 1L))
    stopifnot(!is.na(x))
    stopifnot(!identical(nchar(x), 0L))
  }

  assert_is_valid_name(schema_name)
  assert_is_valid_name(table_name)

  df_to_write <- as.data.frame(df)

  # Assert df and df_to_write are equal
  stopifnot(identical(colnames(df), colnames(df_to_write)))
  stopifnot(identical(rownames(df), rownames(df_to_write)))
  stopifnot(identical(lapply(df, class), lapply(df_to_write, class)))
  stopifnot(all.equal(df, df_to_write, check.attributes = FALSE))

  # Assert rownames are natural from 1 to nrow(df_to_write)
  if (identical(nrow(df_to_write), 0L)) {
    stopifnot(identical(rownames(df_to_write), character(0)))
  } else {
    stopifnot(identical(
      rownames(df_to_write),
      as.character(seq(1L, nrow(df_to_write), by = 1L))
    ))
  }

  tbl_id <- DBI::dbQuoteIdentifier(
    conn, DBI::Id(schema = schema_name, table = table_name))

  DBI::dbWriteTable(
    conn, tbl_id, df_to_write, row.names = FALSE, overwrite = overwrite,
    append = append, field.types = field.types, temporary = temporary)
}

# Get a list of 1-based index chunk vectors.
#
# Args:
# - n_elements: a single non-negative non-NA integer value of the total number
#   of elements that need to be indexed.
# - n_chunks: a single non-negative non-NA integer value of the total number of
#   chunks. n_chunks must be less than or equal to n_elements.
#
# Returns a list of 1-based index chunk vectors.
#
# TODO: unit tests.
get_ind_chunk_list <- function(n_elements, n_chunks) {
  assert_is_valid_int <- function(x) {
    stopifnot(is.numeric(x))
    stopifnot(identical(length(x), 1L))
    stopifnot(!is.na(x))
    stopifnot(x >= 0)
    # Assert n_elements has an integer value numerically
    x_int <- as.integer(x)
    stopifnot(!is.na(x_int))
    stopifnot(identical(as.numeric(x_int), as.numeric(x)))
  }

  assert_is_valid_int(n_elements)
  assert_is_valid_int(n_chunks)

  n_elements_int <- as.integer(n_elements)
  n_chunks_int <- as.integer(n_chunks)
  stopifnot(n_chunks_int <= n_elements_int)

  # If n_chunks_int == 0 or n_elements_int == 0, return an empty list.
  if (identical(n_chunks_int, 0L) || identical(n_elements_int, 0L)) {
    return(list())
  }

  # At this point, n_chunks_int and n_elements_int can only be >= 1.
  element_ind_range_vec <- seq(1L, n_elements_int, by = 1L)

  stopifnot(is.integer(element_ind_range_vec))
  stopifnot(all(!is.na(element_ind_range_vec)))
  stopifnot(identical(length(element_ind_range_vec), n_elements_int))
  stopifnot(identical(length(unique(element_ind_range_vec)), n_elements_int))
  stopifnot(identical(min(element_ind_range_vec), 1L))
  stopifnot(identical(max(element_ind_range_vec), n_elements_int))

  # If n_chunks_int == 1, return a list of length 1 that contains all indices.
  if (identical(n_chunks_int, 1L)) {
    ind_chunk_list <- list(element_ind_range_vec)
  } else {
    # cut requries n_chunks_int >= 2.
    chunk_ids <- cut(element_ind_range_vec, n_chunks_int, labels = FALSE)
    stopifnot(is.integer(chunk_ids))
    stopifnot(all(!is.na(chunk_ids)))
    stopifnot(identical(length(chunk_ids), length(element_ind_range_vec)))
    stopifnot(identical(length(unique(chunk_ids)), n_chunks_int))

    ind_chunk_list <- split(element_ind_range_vec, chunk_ids)
    names(ind_chunk_list) <- NULL
  }

  stopifnot(identical(length(ind_chunk_list), n_chunks_int))
  ind_chunk_vec_lengths <- purrr::map_int(ind_chunk_list, length)
  stopifnot(all(ind_chunk_vec_lengths > 0))

  stopifnot(identical(
    sort(element_ind_range_vec, na.last = TRUE),
    sort(purrr::flatten_int(ind_chunk_list), na.last = TRUE)
  ))

  return(ind_chunk_list)
}



# Generate long format TPM tables and write to csv -----------------------------
cat("Generate long format TPM tables and write to csv...\n")

# db connection to create empty table.
conn <- connect_db(db_env_vars)

# postgres "tables can have at most 1600 columns", but TPM datafarme has >
# 20,000 columns/samples.
#
# Therefore, convert TPM dataframes and histology dataframes into a single long
# format table, with the following steps.
#
# - chunk-wise
#   - TPM wide to long
#   - Add histology data. Replace all-cohorts cohort values with
#     all_cohorts_str_id.
#   - check each chunk
#   - write to db
#
# db queries of a single ENSG ID may have multipe gene symbols. Handle them
# there.

# Distinguish all-cohorts from each-cohort
all_cohorts_str_id <- "All Cohorts"

# The annotation columns of tpm_df, all other columns must be samples TPM
# values.
tpm_df_ann_cols <- c("Gene_symbol", "PMTL", "Gene_Ensembl_ID")

# Assert tpm_data_lists entries are valid for the following procedures. If this
# assertion fails, check all code.
stopifnot(identical(
  sort(names(tpm_data_lists), na.last = TRUE),
  sort(c("gtex", "pt_all_cohorts", "pt_each_cohort"))
))

# Assert tpm_data_lists contents are valid for the following procedures.
place_holder_res <- purrr::imap_lgl(tpm_data_lists, function(xl, xname) {
  stopifnot(tibble::is_tibble(xl$tpm_df))
  stopifnot(tibble::is_tibble(xl$histology_df))

  stopifnot(identical(sum(is.na(dplyr::select(xl$tpm_df, -PMTL))), 0L))
  stopifnot(identical(
    sum(is.na(xl$histology_df[, c("Kids_First_Biospecimen_ID", "cohort")])), 0L
  ))

  stopifnot(identical(
    nrow(xl$tpm_df), nrow(dplyr::distinct(xl$tpm_df[, tpm_df_ann_cols]))
  ))

  stopifnot(identical(
    ncol(xl$tpm_df), length(unique(colnames(xl$tpm_df)))
  ))

  stopifnot(!all_cohorts_str_id %in% xl$histology_df$cohort)

  if (identical(xname, "gtex")) {
    stopifnot(identical(
      sum(is.na(xl$histology_df$GTEx_tissue_subgroup)), 0L))

    stopifnot(identical(sum(!is.na(xl$histology_df$EFO)), 0L))
    stopifnot(identical(sum(!is.na(xl$histology_df$Disease)), 0L))
  } else {
    stopifnot(identical(sum(is.na(xl$histology_df$EFO)), 0L))
    stopifnot(identical(sum(is.na(xl$histology_df$Disease)), 0L))

    stopifnot(identical(
      sum(!is.na(xl$histology_df$GTEx_tissue_subgroup)), 0L))
  }

  tpm_df_colnames <- colnames(xl$tpm_df)
  stopifnot(all(tpm_df_ann_cols %in% tpm_df_colnames))

  # Assert tpm_df Kids_First_Biospecimen_ID columns match exactly to
  # histology_df$Kids_First_Biospecimen_ID
  sids <- tpm_df_colnames[!tpm_df_colnames %in% tpm_df_ann_cols]
  stopifnot(identical(length(sids), length(unique(sids))))
  stopifnot(all(!is.na(sids)))
  stopifnot(identical(
    sort(sids, na.last = TRUE),
    sort(xl$histology_df$Kids_First_Biospecimen_ID, na.last = TRUE)
  ))

  # Assert all histology column names are identical to other list entries.
  histology_df_colnames <- colnames(xl$histology_df)
  stopifnot(is.character(histology_df_colnames))
  stopifnot(identical(
    length(histology_df_colnames), length(unique(histology_df_colnames))
  ))
  stopifnot(length(histology_df_colnames) > 0)
  stopifnot(all(!is.na(histology_df_colnames)))

  stopifnot(identical(
    colnames(tpm_data_lists$gtex$histology_df), histology_df_colnames
  ))

  return(TRUE)
})

# Handle all-cohorts/combined-cohorts/all_cohorts.
#
# Keep only all-cohorts diseases that have > 1 cohorts.
#
# Primary tumor all-cohorts independent (disease, n unique cohort > 1) table.
# Use shorthand prefix padg1 to reduce variable name redundancy.
#
# It is asserted above that tpm_data_lists$pt_all_cohorts$histology_df$Disease
# has no NA.
padg1_tbl <- dplyr::group_by(tpm_data_lists$pt_all_cohorts$histology_df,
                             .data$Disease) %>%
  dplyr::summarise(n_uniq_cohorts = length(unique(.data$cohort))) %>%
  dplyr::filter(.data$n_uniq_cohorts > 1)

padg1_histology_df <- dplyr::filter(
  tpm_data_lists$pt_all_cohorts$histology_df,
  .data$Disease %in% .env$padg1_tbl$Disease)

# If padg1_histology_df is empty, the following expression does not add any row
# or raise any error.
#
# Empty padg1_histology_df and no-biospecimen
# tpm_data_lists$pt_all_cohorts$tpm_df are handled in chunk-wise operations.
padg1_histology_df$cohort <- all_cohorts_str_id

padg1_tpm_df <- dplyr::select(
  tpm_data_lists$pt_all_cohorts$tpm_df,
  tidyselect::all_of(
    c(tpm_df_ann_cols,
      padg1_histology_df$Kids_First_Biospecimen_ID)))

stopifnot(identical(
  sum(is.na(dplyr::select(padg1_tpm_df, -PMTL))), 0L
))

stopifnot(identical(
  sum(is.na(dplyr::select(
    padg1_histology_df,
    Kids_First_Biospecimen_ID, cohort))),
  0L
))

stopifnot(identical(
  ncol(padg1_tpm_df),
  length(unique(colnames(padg1_tpm_df)))
))

# Assert padg1_tpm_df Kids_First_Biospecimen_ID columns match exactly to
# padg1_histology_df$Kids_First_Biospecimen_ID
stopifnot(identical(
  sort(colnames(padg1_tpm_df), na.last = TRUE),
  sort(c(tpm_df_ann_cols, padg1_histology_df$Kids_First_Biospecimen_ID),
       na.last = TRUE)
))

tpm_data_lists$pt_all_cohorts <- list(
  tpm_df = padg1_tpm_df,
  histology_df = padg1_histology_df
)

if (DOWN_SAMPLE_DB_GENES) {
  # Arbitrarily selected genes for quick testing.
  arbt_db_ensg_ids <- c("ENSG00000213420", "ENSG00000157764", "ENSG00000273032",
                        "ENSG00000141510", "ENSG00000171094")

  cat("Downsample ", length(arbt_db_ensg_ids),
      " ENSG IDs for quick testing...\n", sep = "")

  tpm_data_lists <- purrr::map(tpm_data_lists, function(xl) {
    ds_tpm_df <- dplyr::filter(
      xl$tpm_df, .data$Gene_Ensembl_ID %in% .env$arbt_db_ensg_ids)

    x_dsl <- list(
      tpm_df = ds_tpm_df,
      histology_df = xl$histology_df
    )
  })
}

invisible(gc(reset = TRUE))

stopifnot(!file.exists(csv_out_path))

# chunk-wise write to database.
place_holder_res <- purrr::imap_lgl(tpm_data_lists, function(xl, xname) {
  cat("Biospecimen source: ", xname, "\n", sep = "")

  # If xl$padg1_histology_df has 0 row, xl$tpm_df must only have
  # tpm_df_ann_cols, so there is no need to write to CSV file or database.
  stopifnot(is.integer(nrow(xl$histology_df)))

  if (identical(nrow(xl$histology_df), 0L)) {
    stopifnot(identical(
      sort(colnames(xl$tpm_df), na.last = TRUE),
      sort(tpm_df_ann_cols, na.last = TRUE)
    ))
    return(TRUE)
  }

  stopifnot(is.integer(nrow(xl$tpm_df)))
  stopifnot(nrow(xl$tpm_df) > 0)

  # At this point xl$padg1_histology_df must have >= 1 row(s), and TPM table has
  # the same Kids_First_Biospecimen_ID columns.
  #
  # Write TPM values to database by chunk. It takes > 30 GB RAM to convert wide
  # to long at once.
  #
  # Set n_row_chunks by estimating the memory usage of non-chunked operations.
  n_row_chunks <- 20
  tpm_row_ind_chunk_list <- get_ind_chunk_list(
    nrow(xl$tpm_df), min(n_row_chunks, nrow(xl$tpm_df)))

  cat("  Writing ",
      length(tpm_row_ind_chunk_list), " total chunks to csv.\n", sep = "")

  # If xl$tpm_df has 0 row, tpm_row_ind_chunk_list is an empty list. The
  # function will not be mapped, and chunk_tpm_ann_dfs will be an empty 0 x 0
  # tibble.
  chunk_tpm_ann_dfs <- purrr::imap_dfr(
    tpm_row_ind_chunk_list,
    function(x_row_inds, x_chunk_ind) {
      cat("  chunk #", x_chunk_ind, "\n", sep = "")

      x_tpm_df <- xl$tpm_df[x_row_inds, ]

      stopifnot(tibble::is_tibble(x_tpm_df))
      stopifnot(identical(sum(is.na(dplyr::select(x_tpm_df, -PMTL))), 0L))
      stopifnot(identical(nrow(x_tpm_df), length(x_row_inds)))
      stopifnot(nrow(x_tpm_df) > 0)
      stopifnot(ncol(x_tpm_df) > length(tpm_df_ann_cols))

      # Return annotation columns for checking all rows are output, and no
      # duplicate is output.
      x_tpm_ann_df <- x_tpm_df[, tpm_df_ann_cols]
      # Assert all annotations are distinct.
      stopifnot(identical(
        nrow(dplyr::distinct(x_tpm_ann_df)), nrow(x_tpm_ann_df)
      ))

      x_long_tpm_tbl <- tidyr::pivot_longer(
        x_tpm_df, !dplyr::all_of(tpm_df_ann_cols),
        names_to = "Kids_First_Biospecimen_ID", values_to = "TPM")

      # initial x_long_tpm_tbl column names
      init_x_long_tpm_tbl_cols <- c(tpm_df_ann_cols,
                                    "Kids_First_Biospecimen_ID", "TPM")

      # Ensure constant column names of x_long_tpm_tbl.
      x_long_tpm_tbl <- dplyr::select(
        x_long_tpm_tbl, dplyr::all_of(init_x_long_tpm_tbl_cols))

      stopifnot(identical(
        colnames(x_long_tpm_tbl), init_x_long_tpm_tbl_cols))
      stopifnot(identical(
        sum(is.na(dplyr::select(x_long_tpm_tbl, -PMTL))), 0L))

      # dplyr::across selects all columns by default.
      stopifnot(identical(
        dplyr::arrange(
          dplyr::distinct(x_long_tpm_tbl[, tpm_df_ann_cols]),
          dplyr::across()),
        dplyr::arrange(x_tpm_ann_df, dplyr::across())
      ))

      stopifnot(identical(
        sort(
          unique(c("Gene_Ensembl_ID", "Gene_symbol", "PMTL",
                   x_long_tpm_tbl$Kids_First_Biospecimen_ID)),
          na.last = TRUE),
        sort(unique(colnames(x_tpm_df)), na.last = TRUE)
      ))

      stopifnot(identical(
        sort(unique(x_long_tpm_tbl$Kids_First_Biospecimen_ID), na.last = TRUE),
        sort(unique(xl$histology_df$Kids_First_Biospecimen_ID), na.last = TRUE)
      ))

      stopifnot(identical(
        length(unique(xl$histology_df$Kids_First_Biospecimen_ID)),
        nrow(xl$histology_df)
      ))

      x_long_tpm_tbl <- dplyr::left_join(
        x_long_tpm_tbl, xl$histology_df, by = "Kids_First_Biospecimen_ID")
      x_long_tpm_tbl <- dplyr::select(
        x_long_tpm_tbl, "Kids_First_Biospecimen_ID", "cohort", "EFO", "MONDO",
        "Disease", "GTEx_tissue_subgroup_UBERON", "GTEx_tissue_subgroup", "TPM",
        "Gene_Ensembl_ID", "Gene_symbol", "PMTL")

      stopifnot(identical(
        nrow(xl$histology_df),
        length(unique(x_long_tpm_tbl$Kids_First_Biospecimen_ID))
      ))

      stopifnot(identical(sum(is.na(x_long_tpm_tbl$cohort)), 0L))
      if (identical(xname, "pt_all_cohorts")) {
        stopifnot(identical(unique(x_long_tpm_tbl$cohort), all_cohorts_str_id))
      } else {
        stopifnot(!all_cohorts_str_id %in% x_long_tpm_tbl$cohort)
      }

      if (identical(xname, "gtex")) {
        stopifnot(identical(
          sum(is.na(x_long_tpm_tbl$GTEx_tissue_subgroup)), 0L))

        stopifnot(identical(sum(!is.na(x_long_tpm_tbl$EFO)), 0L))

        stopifnot(identical(sum(!is.na(x_long_tpm_tbl$Disease)), 0L))
      } else {
        stopifnot(identical(sum(is.na(x_long_tpm_tbl$EFO)), 0L))

        stopifnot(identical(
          sum(!is.na(x_long_tpm_tbl$GTEx_tissue_subgroup)), 0L))

        stopifnot(identical(sum(is.na(x_long_tpm_tbl$Disease)), 0L))
      }

      if (file.exists(csv_out_path)) {
        # Append.
        pgc_write_csv(
          x_long_tpm_tbl, csv_out_path, append = TRUE, col_names = FALSE)
      } else {
        # Write colnames and empty db table only, if output file does not exist.
        pgc_write_csv(
          x_long_tpm_tbl, csv_out_path, append = FALSE, col_names = TRUE)

        # Create table. Table should not exist.
        cat("  Create empty database table.\n")
        # DBI table ID is case sensitive.
        db_write_table(
          dplyr::slice(x_long_tpm_tbl, 0), conn,
          tolower(db_env_vars$BULK_EXP_SCHEMA),
          tolower(db_env_vars$BULK_EXP_TPM_HISTOLOGY_TBL))
      }

      return(x_tpm_ann_df)
    }
  )

  m_chunk_tpm_ann_df <- dplyr::bind_rows(chunk_tpm_ann_dfs)
  stopifnot(identical(
    nrow(xl$tpm_df),
    nrow(dplyr::distinct(xl$tpm_df[, tpm_df_ann_cols]))
  ))

  stopifnot(identical(
    dplyr::arrange(
      dplyr::distinct(xl$tpm_df[, tpm_df_ann_cols]), dplyr::across()),
    dplyr::arrange(m_chunk_tpm_ann_df, dplyr::across())
  ))

  return(TRUE)
})

DBI::dbDisconnect(conn)

cat("Done running build_db.R.\n")
