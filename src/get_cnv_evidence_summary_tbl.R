# get_cnv_evidence_summary_tbl.R defines a function
# get_cnv_evidence_summary_tbl to return a summary table that is accompanies and
# is available for download with the plot created in get_cnv_eviden_plot.R
#
# Call sequence:
#
# - docker run calls Rscript --vanilla main.R
# - ../main.R calls source("src/get_cnv_evidence_summary_tbll.R")
#
# Defined variables:
#
# - get_cnv_evidence_summary_tbl



# Get a summary tibble of the TPM of a single gene and one disease, which will
# have multiple cohorts if applicable
#
# Args:
# - get_cnv_evidence_summary_tbl: a tibble of a single gene and one disease, 
#   with multiple cohorts if applicable
#
# Returns a tibble with all NAs replaced by blank string "".
get_cnv_evidence_summary_tbl <- function(cnv_evidence_tbl) {
  
  cnv_evidence_tbl %>%
    group_by(ensembl, gene_symbol, pmtl, cancer_group, cancer_status, cohort) %>%
    mutate(group_count = sum(sample_count)) %>%
    ungroup() %>%
    mutate(status = factor(status, levels = c('amplification', 'gain', 'neutral',
                                              'loss', 'deep deletion')),
           cohort = factor(cohort, levels = c('All Cohorts', 'GMKF',
                                              'PBTA', 'TARGET')),
           cancer_status = factor(cancer_status, levels = c('primary', 'relapse')),
           percentage = round(((sample_count / group_count) * 100)),
           label = ifelse(sample_count > 0 & percentage == 0, '<1%',
                          paste0(percentage, '%'))) %>%
    select(Disease = cancer_group, Gene_Ensembl_ID = ensembl,
           Gene_symbol = gene_symbol, PMTL = pmtl, Dataset = cohort,
           Cancer_Stage = cancer_status, Copy_Number_Variant_Type = status,
           Total_Alterations = sample_count, Frequency_Alterations = label) %>%
    arrange(Dataset, Cancer_Stage, 
            Copy_Number_Variant_Type) -> cnv_evidence_summary_tbl
  
  return(cnv_evidence_summary_tbl)
}