#
# cnv_evidence_db.R is called by build_db.Dockerfile to output a variable,
# tpm_data_lists, into cnv_evidence.rds that is used by other functions or
# procedures.
#
# This file should be run with the directory that contains this file as working
# directory.
#
# Call sequence:
#
# - docker build db/build_tools/build_db.Dockerfile runs cnv_evidence_db.R


# Get %>% without loading the whole library
`%>%` <- magrittr::`%>%`


# Define input and output directory --------------------------------------------

# Helper function to get env vars (copied from db/build_toolds/tpm_data_lists.R)
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

# Input dirs
# opc_analysis_dir <- file.path(
#   get_env_var("DB_HOME_DIR_PATH"), "OpenPedCan-analysis")
opc_analysis_dir <- '../../../OpenPedCan-analysis/'
stopifnot(dir.exists(opc_analysis_dir))

data_dir <- file.path(opc_analysis_dir, "data/")
stopifnot(dir.exists(data_dir))

# Output dir
# NOTE: Should the CNV databases go in a subfolder in build_outputs??
# output_dir <- get_env_var("BUILD_OUTPUT_DIR_PATH")
output_dir <- '../../build_outputs/'
stopifnot(dir.exists(output_dir))


# Read and process data --------------------------------------------------------

# independent specimens lists; Some samples are duplicated and this is a list of 
# unique samples that all OpenPedCan analyses use. Lists are either for primary
# tumors or tumors that have relapsed after treatment and are either for all
# OpenPedCan samples or on a per-cohort basis
independent_specs <- tibble::tibble(file_path = c(paste0(data_dir, 'independent-specimens.wgswxspanel.primary.eachcohort.tsv'),
                                                  paste0(data_dir, 'independent-specimens.wgswxspanel.primary.tsv'),
                                                  paste0(data_dir, 'independent-specimens.wgswxspanel.relapse.tsv'),
                                                  paste0(data_dir, 'independent-specimens.wgswxspanel.relapse.eachcohort.tsv'))) %>%
  dplyr::mutate(specimen_descriptor = paste0(stringr::str_to_title(stringr::str_extract(file_path, 'primary|relapse')),
                                             ' Tumor'),
                cohort_level = ifelse(stringr::str_detect(file_path, 'eachcohort'), 
                                      'each_cohort', 'all_cohorts'),
                data = purrr::map(file_path, ~ readr::read_tsv(.))) %>%
  tidyr::unnest(c(data)) %>%
  dplyr::select(-file_path)
# print(head(independent_specs))

# Independent specimen list checks
# Check for important columns in data
stopifnot(c("Kids_First_Participant_ID", "Kids_First_Biospecimen_ID",
            "specimen_descriptor") %in%
            colnames(independent_specs))
# check for NAs/missing information
stopifnot(independent_specs %>%
            dplyr::filter(dplyr::across(.cols = everything(), .fns = dplyr::any_vars(is.na(.)))) %>% 
            nrow(.) == 0)


# histology table has sample metadata, including sequencing type, tumor status,
# readr::read_tsv(paste0(data_dir, 'histologies.tsv')) -> histologies

# EFO and MONDO ids are two ontology systems that assign unique numeric codes to
# biological variables, in our case to the cancer group assigned to all samples.
# Needed to connect our cancer names to the Molecular Target Platform's backend
readr::read_tsv(paste0(data_dir, 'efo-mondo-map.tsv')) -> efos
# check that the correct columns exist
stopifnot(c("cancer_group", "efo_code", "mondo_code") %in%
            colnames(efos))

# Table mapping HUGO gene names to ensembl IDs and also labelling whether the
# gene is on a list of known relevant pediatric molecular targets (PMTL)
readr::read_tsv(paste0(data_dir, 
                         'ensg-hugo-pmtl-mapping.tsv')) -> ensg_pmtl_hugo
# check that the correct columns exist
stopifnot(c("ensg_id", "gene_symbol", "pmtl", "version") %in%
            colnames(ensg_pmtl_hugo))
# check that all pmtl annotations have the matching version
stopifnot(identical(
  is.na(ensg_pmtl_hugo$pmtl),
  is.na(ensg_pmtl_hugo$version)))


# Read and filter CNV calls ----------------------------------------------------
# Table of copy number variants; status is a categorical description of the 
# copy_number relative to the ploidy of the tumor, deep deletion = 0 copies, 
# loss = fewer copies than ploidy, neutral = same as ploidy, gain = up to 2 
# times ploidy, amplification = more than 2 times ploidy
data.table::fread(paste0(data_dir,
                         'consensus_wgs_plus_cnvkit_wxs.tsv.gz')) %>%
  dtplyr::lazy_dt() %>%
  # filter for masterlist of biospecimen ids
  dplyr::filter(biospecimen_id %in% independent_specs$Kids_First_Biospecimen_ID) %>%
  # This group_by() %>% slice_head() is to resolve the problem of some samples
  # having multiple CNV calls for the same gene. The problem is being addressed
  # in OpenPedCan-analysis, so this is temporarily here for development purposes
  # and code will be updated once the unique calls are implemented in
  # OpenPedCan-analysis
  dplyr::group_by(biospecimen_id, ensembl) %>%
  dplyr::slice_head(n = 1) %>%
  dplyr:: ungroup() %>%
  dplyr::mutate(status = ifelse(!is.na(status), status,
                         case_when(copy_number == 0 ~ 'deep deletion',
                                   copy_number < ploidy & copy_number != 0 ~ 'loss',
                                   copy_number == ploidy ~ 'neutral',
                                   copy_number <= (2*ploidy) ~ 'gain',
                                   copy_number > (2*ploidy) ~ 'amplification'))) %>%
  tibble::as_tibble() -> cnv
# check that the correct columns exist
stopifnot(c("biospecimen_id", "status", "ensembl", "gene_symbol") %in%
            colnames(cnv))
# check that every biospecimen has a unique CNV call (status)
stopifnot(
  cnv %>% 
    dtplyr::lazy_dt() %>%
    dplyr::distinct(biospecimen_id, status, ensembl, gene_symbol) %>% 
    dplyr::count(biospecimen_id, ensembl, gene_symbol) %>%
    dplyr::filter(n != 1) %>%
    tibble::as_tibble() %>%
    nrow(.) == 0
)


# Format data for CNV evidence database ----------------------------------------
cnv_evidence_db <- cnv %>%
  dplyr::inner_join(independent_specs,
                    by = c('biospecimen_id' = 'Kids_First_Biospecimen_ID')) %>%
  dplyr::count(ensembl, gene_symbol, cancer_group, status, cohort_level, 
               specimen_descriptor, name = 'sample_count') %>%
  dplyr::left_join(efos, by = 'cancer_group') %>%
  dplyr::left_join(ensg_pmtl_hugo, by = c('ensembl' = 'ensg_id', 'gene_symbol'))

# check for NAs/missing information
stopifnot(cnv_evidence_db %>%
            dplyr::filter(dplyr::across(.cols = c('ensembl', 'gene_symbol',
                                                  'cancer_group', 'status', 
                                                  'cohort_level', 
                                                  'specimen_descriptor', 
                                                  'sample_count',
                                                  'efo_code', 'mondo_code'), 
                                        .fns = dplyr::any_vars(is.na(.)))) %>% 
            nrow(.) == 0)

# cat("---------------------------------\n",
#     as.character(Sys.time()), "\n",
#     "All-cohorts independent n tumor samples:\n",
#     format_spec_desc_counts(
#       tpm_data_lists$prm_rlp_all_cohorts$histology_df$specimen_descriptor),
#     "\n",
#     "Each-cohort independent n tumor samples:\n",
#     format_spec_desc_counts(
#       tpm_data_lists$prm_rlp_each_cohort$histology_df$specimen_descriptor),
#     "\n",
#     "GTEx all n samples: ", nrow(tpm_data_lists$gtex$histology_df), "\n",
#     "Number of genes: ", nrow(tpm_data_lists$prm_rlp_all_cohorts$tpm_df),
#     "\n---------------------------------\n", sep = "")


# Output -----------------------------------------------------------------------
readr::write_tsv(cnv_evidence_db, paste0(output_dir, "cnv_evidence_db.tsv"))



