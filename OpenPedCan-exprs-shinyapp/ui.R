# load libraries
suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(shinyjs)
  library(plotly)
  library(DT)
  library(shinyWidgets)
  library(tidyverse)
  library(dplyr)
})


dashboardPage(
  
  # dashboardHeader begins
  dashboardHeader(title = 'OpenTargets Expression Plots', titleWidth = 300, dropdownMenuOutput("messageMenu")), # dashboardHeader ends
  
  # dashboardSidebar begins
  dashboardSidebar(width = 300,
                   
                   tags$head(
                     tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
                   
                   # enable vertical scrolling
                   div(style="overflow-y: scroll"),
                   
                   # sidebarMenu begin
                   sidebarMenu(
                     menuItem("Tumor vs Normal", icon = icon("dashboard"), tabName = "tumor_vs_normal"),
                     menuItem("Tumors only", icon = icon("dashboard"), tabName = "tumors_only")
                   ) # sidebarMenu
  ), # dashboardSidebar
  dashboardBody(
    
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    
    div(style="overflow-x: scroll"),
    
    # tabItems begins
    tabItems(
      tabItem(tabName = "tumor_vs_normal",
              fluidRow(
                box(background = "navy", width = 12,
                    column(2, pickerInput(inputId = "select_gene_1", label = "Gene", choices = NULL, multiple = F, options = list(`live-search` = T))),
                    column(2, pickerInput(inputId = "select_normal_cohort_1", label = "Normal Cohort", choices = c("GTEx"), selected = T)), 
                    column(2, pickerInput(inputId = "select_tumor_cohort_1", label = "Tumor Cohort", choices = c("GMKF", "PBTA", "TARGET"), multiple = T, options = list(`actions-box` = T, 
                                                                                                                                                               `none-selected-text` = "Please make a selection!"))), 
                    column(2, pickerInput(inputId = "analysis_type_1", label = "Analysis Type", choices = NULL)),
                    column(2, pickerInput(inputId = "select_histology_1", label = "Histology", choices = NULL, multiple = F, options = list(`live-search` = T))),
                    column(2, br(), switchInput(inputId = "log_val_1", label = "log2", onStatus = "success", offStatus = "danger", size = 'normal')),
                    br(),
                    shinyjs::useShinyjs(),
                    column(3, actionButton(inputId = "create_boxplot_1", label = "Create boxplot", icon("paper-plane"), style = "font-size: 14px; margin-top: 4px; padding:8px;color: #fff; background-color: #337ab7; border-color: #2e6da4")))
              ),
              tabsetPanel(type = "tabs",
                          tabPanel(title = "Plot", div(style="overflow-x: scroll; overflow-y: scroll", plotlyOutput("boxplot_1", width = "100%", height = 800))),
                          tabPanel(title = "Table", div(style="overflow-x: scroll; overflow-y: scroll", DT::dataTableOutput("table_1")))
              )
      ),
      tabItem(tabName = "tumors_only",
              fluidRow(
                box(background = "navy", width = 12,
                    column(2, pickerInput(inputId = "select_gene_2", label = "Gene", choices = NULL, multiple = F, options = list(`live-search` = T))),
                    column(2, pickerInput(inputId = "select_tumor_cohort_2", label = "Tumor Cohort", choices = c("GMKF", "PBTA", "TARGET"), multiple = T, options = list(`actions-box` = T, 
                                                                                                                                                               `none-selected-text` = "Please make a selection!"))), 
                    column(2, pickerInput(inputId = "analysis_type_2", label = "Analysis Type", choices = c("Cancer-Group level" = "cancer_group_level",
                                                                                                            "Cohort-Cancer-Group level" = "cohort_cancer_group_level"), multiple = F)),
                    column(2, br(), switchInput(inputId = "log_val_2", label = "log2", onStatus = "success", offStatus = "danger", size = 'normal')),
                    br(),
                    shinyjs::useShinyjs(),
                    column(2, actionButton(inputId = "create_boxplot_2", label = "Create boxplot", icon("paper-plane"), style = "font-size: 14px; margin-top: 4px; padding:8px;color: #fff; background-color: #337ab7; border-color: #2e6da4")))
              ),
              tabsetPanel(type = "tabs",
                          tabPanel(title = "Plot", div(style="overflow-x: scroll; overflow-y: scroll", plotlyOutput("boxplot_2", width = "80%", height = 800))),
                          tabPanel(title = "Table", div(style="overflow-x: scroll; overflow-y: scroll", DT::dataTableOutput("table_2")))
              )
      )
    )
  )
)
