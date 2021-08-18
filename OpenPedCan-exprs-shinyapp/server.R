# source functions
source('R/pubTheme.R')
source('R/viewDataTable.R')
source('R/pan_cancer_plot.R')
source('R/tumor_vs_normal_plot.R')

# Get `magrittr` pipe
`%>%` <- dplyr::`%>%`

## global data read each session
# expression matrix and histology file
expr_mat <- readRDS('data/gene-expression-rsem-tpm-collapsed.rds')
genes <- unique(rownames(expr_mat))
hist_file <- read.delim('data/histologies.tsv', stringsAsFactors = F)

# mapping files for ENSEMBL identifier, RMTL, EFO and MONDO codes
ensg_hugo_rmtl_mapping <- read.delim('data/ensg-hugo-rmtl-mapping.tsv') %>%
  unique()
efo_mondo_map <- read.delim('data/efo-mondo-map.tsv') %>%
  unique()

shinyServer(function(input, output, session){
  
  # update inputs for "pan-cancer" comparison
  observe({
    # update gene names
    updatePickerInput(session = session, inputId = "select_gene_1", choices = genes)
  })
  
  # update inputs for "tumor vs normal" comparison
  observe({
    # update gene names
    updatePickerInput(session = session, inputId = "select_gene_2", choices = genes)
    
    # update analysis type
    # if more than one cohort is chosen, the analysis type is updated to cancer group level
    if(length(input$select_tumor_cohort_1) == 1){
      updatePickerInput(session = session, inputId = "analysis_type_1", choices = c("Cohort-Cancer-Group level" = "cohort_cancer_group_level"))
    } else if(length(input$select_tumor_cohort_1) > 1){
      updatePickerInput(session = session, inputId = "analysis_type_1", choices = c("Cancer-Group level" = "cancer_group_level"))
    }
    
    # update histologies n >= 5
    cancer_groups <- hist_file %>%
      filter(cohort %in% c(input$select_normal_cohort_1, input$select_tumor_cohort_1),
             !is.na(cancer_group)) %>%
      group_by(cohort, cancer_group) %>%
      summarise(n_samples = n()) %>%
      filter(n_samples >= 5) %>%
      .$cancer_group %>%
      unique()
    updatePickerInput(session = session, inputId = "select_histology_1", choices = cancer_groups)
  })
  
  # call pan-cancer function
  observeEvent(input$create_boxplot_2, {
    isolate({
      gene_name <- input$select_gene_2
      cohorts <- input$select_tumor_cohort_2
      
      # subset histology file to samples in selected cohort
      hist_file_subset <- hist_file %>%
        filter(cohort %in% cohorts)
      
      # subset gene matrix to selected samples and selected gene
      expr_mat_subset <- expr_mat %>%
        select(hist_file_subset$Kids_First_Biospecimen_ID) %>%
        rownames_to_column('gene') %>%
        filter(gene %in% gene_name)
      
      # call function
      res2 <<- pan_cancer_plot(expr_mat_gene = expr_mat_subset,
                               hist_file = hist_file_subset, 
                               efo_mondo_map, ensg_hugo_rmtl_mapping,
                               analysis_type = input$analysis_type_2,
                               log = input$log_val_2)
    })
  })
  
  # render pan-cancer boxplot
  output$boxplot_2 <- renderPlotly({
    if(input$create_boxplot_2 == 0){
      return()
    }
    isolate({
      res2$output_plot
    })
  })
  
  # render pan-cancer table
  output$table_2 <- DT::renderDataTable({
    if(input$create_boxplot_2 == 0){
      return()
    }
    isolate({
      viewDataTable(res2$output_table, pageLength = 10)
    })
  })
  
  # pan-cancer plots: disable action button if no cohort is selected 
  observe({
    if(is.null(input$select_tumor_cohort_2)) {
      shinyjs::disable("create_boxplot_2")
    } else {
      shinyjs::enable("create_boxplot_2")
    }
  })
  
  
  # call tumor vs normal function
  observeEvent(input$create_boxplot_1, {
    isolate({
      gene_name <- input$select_gene_1
      cohorts <- c(input$select_normal_cohort_1, input$select_tumor_cohort_1)
      
      # subset histology file to samples in selected cohort
      hist_file_subset <- hist_file %>%
        filter(cohort %in% cohorts)
      
      # subset gene matrix to selected samples and selected gene
      expr_mat_subset <- expr_mat %>%
        select(hist_file_subset$Kids_First_Biospecimen_ID) %>%
        rownames_to_column('gene') %>%
        filter(gene %in% gene_name)
      
      # call function
      res1 <<- tumor_vs_normal_plot(expr_mat_gene = expr_mat_subset,
                                    hist_file = hist_file_subset, 
                                    efo_mondo_map, ensg_hugo_rmtl_mapping,
                                    analysis_type = input$analysis_type_1,
                                    cohorts = cohorts,
                                    cancer_group_name = input$select_histology_1,
                                    log = input$log_val_1)
    })
  })
  
  # render tumor vs normal boxplot
  output$boxplot_1 <- renderPlotly({
    if(input$create_boxplot_1 == 0){
      return()
    }
    isolate({
      res1$output_plot
    })
  })
  
  # render tumor vs normal table
  output$table_1 <- DT::renderDataTable({
    if(input$create_boxplot_1 == 0){
      return()
    }
    isolate({
      viewDataTable(res1$output_table, pageLength = 10)
    })
  })
  
  # tumor vs normal plots: disable action button if no cohort is selected 
  observe({
    if(is.null(input$select_tumor_cohort_1)) {
      shinyjs::disable("create_boxplot_1")
    } else {
      shinyjs::enable("create_boxplot_1")
    }
  })
  
})

