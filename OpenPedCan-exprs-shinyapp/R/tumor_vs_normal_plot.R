# plot/table for tumor vs normal plots
suppressPackageStartupMessages({
  library(tidyr)
  library(dplyr)
  library(ggplot2)
})

# Get `magrittr` pipe
`%>%` <- dplyr::`%>%`

tumor_vs_normal_plot <- function(expr_mat_gene, hist_file, 
                                 efo_mondo_map, ensg_hugo_rmtl_mapping,
                                 cohorts, cancer_group_name,
                                 analysis_type = c("cohort_cancer_group_level", "cancer_group_level"), 
                                 log = FALSE,
                                 plots_dir = 'www', results_dir = 'www', 
                                 plot_width = 13, plot_height = 9, 
                                 meta_file = 'www/metadata.tsv'){
  
  # standardize groups:
  # create a single group variable for both cancer_group and gtex_subgroup 
  hist_file <- hist_file %>%
    mutate(group = ifelse(sample_type == "Normal", gtex_subgroup, cancer_group)) %>%
    filter(cohort %in% cohorts) %>%
    filter(cancer_group %in% cancer_group_name | cohort %in% "GTEx") %>%
    select(Kids_First_Biospecimen_ID, cohort, sample_type, gtex_subgroup, cancer_group, group)
  
  # expression matrix to long format
  expr_mat_gene <- expr_mat_gene %>%
    tidyr::gather("Kids_First_Biospecimen_ID", "tpm", -c("gene"))
  
  # combine with histology file
  expr_mat_gene <- expr_mat_gene %>%
    inner_join(hist_file, by = "Kids_First_Biospecimen_ID")
  
  # format x-axis labels and filter to n >= 5
  if(analysis_type == "cohort_cancer_group_level"){
    expr_mat_gene <- expr_mat_gene %>%
      group_by(cohort, group) %>%
      mutate(n_samples = n()) %>%
      filter(n_samples >= 5) %>%
      mutate(x_labels = paste0(group, " (N = ", n_samples, ")"))
  } else {
    expr_mat_gene <- expr_mat_gene %>%
      group_by(group) %>%
      mutate(n_samples = n()) %>%
      filter(n_samples >= 5) %>%
      mutate(x_labels = paste0(group, " (N = ", n_samples, ")"))
  }
  
  # get cancer group
  cohort_cancer_group <- expr_mat_gene %>%
    filter(sample_type == "Tumor") %>%
    .$x_labels %>%
    unique()
  
  # reorder by alphabet 
  fcts <- expr_mat_gene %>% 
    select(sample_type, group, x_labels) %>%
    unique() %>%
    arrange(desc(sample_type), group) %>%
    .$x_labels 
  expr_mat_gene$x_labels <- factor(expr_mat_gene$x_labels, levels = fcts)
  
  # create unique title and filenames
  gene_name <- unique(expr_mat_gene$gene)
  tumor_cohort <- expr_mat_gene %>%
    filter(sample_type == "Tumor") %>%
    .$cohort %>% 
    unique() %>%
    paste0(collapse = ", ")
  
  # define cohort_name
  if(analysis_type == "cohort_cancer_group_level"){
    cohort_name <- tumor_cohort
  } else {
    cohort_name <- "all_cohorts"
  }
  
  cancer_group_name <- expr_mat_gene %>%
    filter(sample_type == "Tumor") %>%
    .$cancer_group %>%
    unique()
  
  # replace semi-colon, forward slash and spaces with hyphen in filenames
  cancer_group_name_fname <- gsub('/| |;', '-', cancer_group_name) 
  
  # create title and filename prefix
  if(analysis_type == "cohort_cancer_group_level"){
    title <- paste(gene_name, 
                   paste(tumor_cohort, cancer_group_name, "vs. GTEx", sep = " "), sep = "\n")
    fname <- paste(gene_name, tumor_cohort, cancer_group_name_fname, "vs_GTEx", analysis_type, sep = "_")
  } else {
    title <- paste(gene_name,
                   paste(cancer_group_name, "vs. GTEx", sep = " "), sep = "\n")
    fname <- paste(gene_name, cancer_group_name_fname, "vs_GTEx", analysis_type, sep = "_")
  }
  plot_fname <- paste0(fname, '.png')
  table_fname <- paste0(fname, '.tsv')
  
  # data-frame for metadata output 
  meta_df <- data.frame(Gene_symbol = gene_name, 
                        plot_type = "tumor_normal_gtex", 
                        Dataset = cohort_name,
                        Disease = gsub(" [(].*|[,].*", "", cohort_cancer_group),
                        analysis_type = analysis_type, 
                        plot_fname = plot_fname,
                        table_fname = table_fname)
  if(!file.exists(meta_file)){
    write.table(x = meta_df, file = meta_file, sep = "\t", row.names = F, quote = F)
  } else {
    write.table(x = meta_df, file = meta_file, sep = "\t", row.names = F, col.names = F, quote = F, append = TRUE)
  }
  
  # boxplot
  cols <- c("Normal" = "grey80", "Tumor" = "red3")
  if(log == TRUE){
    expr_mat_gene <- expr_mat_gene %>%
      mutate(tpm = log2(tpm + 1))
    y_lab <- "log2 (TPM)"
  } else {
    y_lab <- "TPM"
  }
  output_plot <- ggplot(expr_mat_gene, aes(x = x_labels, y = tpm, fill = sample_type)) +
    stat_boxplot(geom ='errorbar', width = 0.2, lwd = 0.3) +
    geom_boxplot(lwd = 0.3, fatten = 0.7, outlier.shape = 1, width = 0.5, outlier.size = 1) +
    ylab(y_lab) + xlab("") +
    theme_Publication2(base_size = 12) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    ggtitle(title) +
    scale_fill_manual(values = cols) + theme(legend.position='none')
  ggsave(plot = output_plot, filename = file.path(plots_dir, plot_fname), device = "png", width = plot_width, height = plot_height)
  
  # output table of gene, median and sd
  output_table <- expr_mat_gene %>%
    group_by(gene, x_labels) %>%
    summarise(mean = mean(tpm),
              median = median(tpm),
              sd = sqrt(var(tpm))) %>%
    mutate(mean = round(mean, digits = 2),
           median = round(median, digits = 2),
           sd = round(sd, digits = 2))
  
  # format columns and annotate using RMTL, EFO and MONDO
  output_table <- output_table %>%
    dplyr::rename(Gene_symbol = gene) %>%
    mutate(Dataset = cohort_name, 
           Disease = gsub(" [(].*|[,].*", "", cohort_cancer_group)) %>%
    inner_join(ensg_hugo_rmtl_mapping, by = c("Gene_symbol" = "gene_symbol")) %>%
    inner_join(efo_mondo_map, by = c("Disease" = "cancer_group")) %>%
    dplyr::rename(Gene_Ensembl_ID = ensg_id,
                  RMTL = rmtl,
                  EFO = efo_code,
                  MONDO = mondo_code) %>%
    dplyr::select(Gene_symbol, Gene_Ensembl_ID, Dataset, Disease, 
                  x_labels, mean, median, sd, RMTL, EFO, MONDO)
  write.table(x = output_table, file = file.path(results_dir, table_fname), sep = "\t", row.names = F, quote = F)
  
  # from here on, the code is plotly specific
  # now remove error bars because plotly adds it by default
  output_plot$layers[[1]] <- NULL
  
  # add labels to outliers
  expr_mat_gene <- expr_mat_gene %>% 
    group_by(x_labels) %>% 
    mutate(OutlierFlag = ifelse((tpm < quantile(tpm, 1/3, na.rm = T) - 1.5*IQR(tpm, na.rm = T)) | 
                                  (tpm > quantile(tpm, 2/3, na.rm = T) + 1.5*IQR(tpm, na.rm = T)), 'Outlier', 'NotOutlier'))%>%
    group_by()
  
  # add geom point layer
  output_plot <- output_plot + 
    geom_point(data = expr_mat_gene %>% 
                 filter(OutlierFlag == "Outlier"), aes(group = x_labels, label = Kids_First_Biospecimen_ID), stroke = 0.3)
  
  # convert to plotly
  output_plot <- ggplotly(output_plot, tooltip = c("label"))
  for (i in 1:length(output_plot$x$data)){
    if (output_plot$x$data[[i]]$type=="box"){
      output_plot$x$data[[i]]$marker$opacity = 0  
      output_plot$x$data[[i]]$hoverinfo = "none"
    }
  }
  
  # return plot and table
  return(list(output_plot = output_plot,
              output_table = output_table))
}
