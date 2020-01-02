#' ui
#' 
#' @param request needed for bookmarking
#'
#' @import shiny
#' @import shinydashboard
#' @import shinydashboardPlus
#' @import shinyWidgets
#' @import plotly
#' @import rhandsontable
#' @import DT

app_ui <- function(request) {
  model_list <- dir('models')
  model_list <- unlist(strsplit(model_list,'.R'))
  #library(V8)
  tagList(
    #shinyjs::useShinyjs(),
    golem_add_external_resources(),
    dashboardPagePlus(
      dashboardHeaderPlus(title = "TS",
                          fixed=FALSE,
                          enable_rightsidebar=TRUE,#General information button at top-right. 
                          #currently only shows:
                          # a) "active" dataset
                          # b) "selected subject ID"
                          rightSidebarIcon = "ellipsis-h"
      ),
      #------------------------------ Side Bar Function -------------------------------------
      dashboardSidebar(
        sidebarMenuOutput("menu")
      ),
      uiOutput('tp_tab_script'),
      
      rightsidebar=rightSidebar(
        box(width=2,
            actionButton("browser", "debug")
        ),
        rightSidebarTabContent(
          id=1,
          title="",
          icon='hourglass',
          active=TRUE,
          conditionalPanel(
            condition="input.tabs == 'tpestimation'",
            title = "Model parameter configuration",
            width=3,
            selectInput(inputId='tp_model1',
                        label='Choose comparison model 1',
                        choices=model_list,
                        selected='ar'
            ),
            selectInput(inputId='tp_model2',
                        label='Choose comparison model 2',
                        choices=model_list,
                        selected='var'
            ),
            conditionalPanel(
              condition="input.tp_model1 == 'pcvar'"
              #uiOutput('ncomp_pca')
            )
          )
        ),
        rightSidebarTabContent(
          id=2,
          title="",
          icon='cogs',
          active=FALSE,
          conditionalPanel(
            condition="input.tabs == 'tpestimation'",
            title = "Algorithm parameter configuration",
            width=3,
            selectInput(inputId='tp_error_metric',
                        label='Choose error metric',
                        choices=c('Mean Squared Error (MSE)'='mse',
                                  'Root Mean Squared Error (RMSE)'='rmse',
                                  'Mean Absolute Error (MAE)'='mae',
                                  'Mean Absolute Percentage Error (MAPE)'='mape',
                                  'Normalized Mean Squared Error (LMSE)'='nmse',
                                  'Relative Standard Deviation (rSTD)'='rstd'),
                        selected='mse'),
            numericInput(inputId='select_k_fold',label='Choose number of folds',min=2,max=20,value=5),
            numericInput(inputId='select_max_iter',label="Choose maximum number of iterations",min=10,max=1000,value=100),
            uiOutput('select_stepsize_init_element'),
            numericInput(inputId='select_stepsize_scaler',label='Choose stepsize scaler',min=.0001,max=1,value=.8),
            uiOutput('num_searchtp_sim')
          )
        ),
        conditionalPanel(
          condition="input.tabs == 'tpestimation'",
          actionButton("submitTPS", "Submit")
        )
        
      ),
      #------------------------------ Dashboard Body -------------------------------------
      dashboardBody(
        #------------------------------ Tags Style -------------------------------------    
        tags$style(type="text/css",
                   ".shiny-output-error { visibility: hidden; }",
                   ".shiny-output-error:before { visibility: hidden; }"
        ),
        #------------------------------ Tabs Start -------------------------------------     
        tabItems(
          #------------------------------ Tabs 1 Data Start-------------------------------------
          tabItem(tabName = "data1",
                  #------------------------------ Tabs 1 Data - fluid page start -------------------------------------
                  fluidPage(
                    #------------------------------ Tabs 1 Data - fluid row 1 -------------------------------------
                    fluidRow(
                      infoBoxOutput("in_memory_df"),
                      infoBoxOutput("load_datasets")
                    ),
                    #------------------------------ Tabs 1 Data - fluid row 2 -------------------------------------
                    fluidRow(
                      box(
                        width = 4, 
                        height = 100,
                        selectInput('data_source', 'Select Data Source', 
                                    list(
                                      "In-memory" = "data_frame",
                                      "Import CSV file" = "import"
                                    ),
                                    selected = "data_frame"
                        )
                      ),
                      box(width =  4, 
                          height = 100,
                          conditionalPanel(condition = "input.data_source.includes('data_frame')",
                                           uiOutput("df_list")
                          ),
                          conditionalPanel(condition = "input.data_source == 'import'",
                                           dropdownButton(
                                             fileInput('file1', 'Choose CSV File',
                                                       accept=c('text/csv', 
                                                                'text/comma-separated-values,text/plain', 
                                                                '.csv')),
                                             awesomeCheckbox(inputId = "csv_header", 
                                                             label = "Header", 
                                                             value = TRUE),
                                             radioButtons('sep', 'Separator',
                                                          c(Comma=',',
                                                            Semicolon=';',
                                                            Tab='\t'),
                                                          ','),
                                             radioButtons('quote', 'Quote',
                                                          c(None='',
                                                            'Double Quote'='"',
                                                            'Single Quote'="'"),
                                                          '"'),
                                             circle = TRUE, status = "danger", 
                                             icon = icon("file-text", lib = "font-awesome"), width = "300px",
                                             tooltip = tooltipOptions(title = "Click to set csv file parameters!")
                                           )
                          )
                      ),
                      box(width =  4, height = 100,
                          conditionalPanel(condition = "(output.load_flag == '2' && 
                                       input.data_source == 'inst_pack') ||  
                                       (output.load_flag == '2' && input.data_source == 'import' ) || 
                                       (output.load_flag == '1' && input.data_source == 'data_frame' ) || 
                                       (output.load_flag == '1' && input.data_source == 'inst_pack') || 
                                       (output.load_flag == '1' && input.data_source == 'time_series')",
                                           actionButton("load", "Load")
                          ),
                          conditionalPanel(condition =  "output.loaded_table_flag == '1'",
                                           actionButton("remove", "Remove")
                          )
                      )
                    ),
                    fluidRow(
                      boxPlus(width = 7, title = "Preview Table",
                              collapsible=TRUE,
                          div(style = 'overflow-x: scroll',
                              DT::dataTableOutput('view_table'))
                      ),
                      box(width = 5, title = "Loaded Datasets",
                          collapsible=TRUE,
                          div(style = 'overflow-x: scroll',
                              DT::dataTableOutput('list_loaded_df'))
                      )
                    )
                  )
                  #------------------------------ Tabs 1 Data - fluid page end -------------------------------------
          ),
          #------------------------------ Tabs Data2 Start-------------------------------------
          tabItem(tabName = "data2",
                  fluidPage(
                    fluidRow(
                      conditionalPanel(condition =  "output.loaded_table_flag == '1'",
                                       infoBoxOutput("data_name"),
                                       infoBoxOutput("num_var"),
                                       infoBoxOutput("num_obs")
                      )
                    ),
                    fluidRow(
                      conditionalPanel(condition =  "output.loaded_table_flag == '1'",
                                       box(width = 2, title = "Edit dataset",
                                           uiOutput("loaded_ds_list"),
                                           conditionalPanel(condition =  "output.loaded_table_flag == '1' && 
                                                        output.class_df_flag == false ",
                                                            selectInput('data_option', 'Select Option', 
                                                                        list(
                                                                          "Variable Attributes" = "var_attr",
                                                                          "Reshape Options" = "data_reshape"
                                                                        ),
                                                                        selected = "data_reshape")
                                           ),
                                           conditionalPanel(condition =  "output.loaded_table_flag == '1' && output.class_df_flag == false && input.data_option == 'var_attr'",
                                                            radioButtons("class_selection", label = "Variables Modification", 
                                                                         choices = list(Numeric = "numeric", Factor = "factor", 
                                                                                        Character = "character",
                                                                                        Date = "date"),
                                                                         selected = "numeric"),
                                                            conditionalPanel(condition =  "input.class_selection == 'date' && output.loaded_table_flag == '1' && output.class_df_flag == false && input.data_option == 'var_attr'",
                                                                             selectInput('date_format', "Select the Date Format",
                                                                                         list(
                                                                                           YMD = "ymd",
                                                                                           YDM = "ydm",
                                                                                           MYD = "myd",
                                                                                           MDY = "mdy",
                                                                                           DMY = "dmy",
                                                                                           DYM = "dym"
                                                                                         )),
                                                                             #titlePanel(h5("Date Preview")),
                                                                             tags$h5("Date Preview"),
                                                                             verbatimTextOutput("date_prev")
                                                            ),
                                                            actionButton("var_modify", "Modify")
                                           ),
                                           conditionalPanel(condition = "output.loaded_table_flag == '1' && output.class_df_flag == false && input.data_option == 'data_reshape'",
                                                            actionButton("remove_var", "Remove variable"))
                                           
                                       ),
                                       box(width=2,uiOutput('id_variable'),
                                           conditionalPanel(condition = "output.loaded_table_flag == '1' && input.select_dataset_id_var != 'None'", 
                                                            uiOutput('dataset_id_value'))
                                       )),
                      conditionalPanel(condition =  "output.loaded_table_flag == '1' && output.class_df_flag == false ",
                                       box(width = 4, title = "List of Variables",
                                           DT::dataTableOutput("data_tab2_var"),
                                           conditionalPanel(condition = "output.loaded_table_flag == '1'",
                                                            uiOutput('dataset_select_index_variable')
                                           )
                                           
                                       ),
                                       box(width = 4, title = "Variable Summary",
                                           plotlyOutput("data_tab2_summary_plot",height = 200),
                                           tableOutput("data_tab2_var_summary")
                                           #tableOutput("data_tab2_var_summary_descr")
                                       )
                      )
                      
                    ),
                    
                    fluidRow(
                      conditionalPanel(condition = "output.loaded_table_flag == '1'",
                                       radioButtons("data_tab2_na_omit", h6("Show data including missing values"),
                                                    choices = list("Yes", "No"), selected = "No",inline=TRUE),
                                       div(style = 'overflow-x: scroll',
                                           DT::dataTableOutput("data_tab2_table"))
                      )
                    )
                  )
                  
          ),
          #------------------------------ Tabs Data End-------------------------------------
          #------------------------------ Tabs Visualization Start-------------------------------------
          
          tabItem(tabName = "vis",
                  conditionalPanel(condition =  "output.loaded_table_flag == '1'",
                                   fluidPage(
                                     box(title = "plot",
                                         plotlyOutput("main_plot")
                                     )
                                   )
                  )
          ),
          #------------------------------ Tabs Visualization End-------------------------------------
          #------------------------------ Tabs Simulation -----
          tabItem(tabName = "sim",
                  fluidPage(
                    boxPlus(status = "success",
                            title='Simulation parameter setup',
                            solidHeader = TRUE,
                            closable=FALSE,
                            collapsible=TRUE,
                            enable_dropdown=TRUE,
                            uiOutput('index_variable'),
                            selectInput(
                              "selection1",
                              "Choose your data-generating model.",
                              model_list
                            ),
                            numericInput('lagNum','Choose the number of lags',min=1,max=100,value=1),
                            uiOutput('simulation_parameter_origin'),
                            numericInput(
                              "nError",
                              "Measurement error:",
                              0,
                              min = 0,
                              max = 1,
                              step = 0.01
                            ),
                              uiOutput('num_var_sim'),
                            
                            uiOutput('num_tp_sim'),
                            ####Parameters from model
                            actionButton("submit1", "Submit")
                    ),
                    p(id='sim_anchor','')
                    )
                  )
        
          ,
        
          tabItem(tabName= "tpestimation",
                  fluidPage(
                    fluidRow(
                      uiOutput("tp")
                      # uiOutput("tp_last_1"),
                      # uiOutput("tp_last_2")
                    ),
                    uiOutput('tp_plots')
                  )
          ),
          tabItem(tabName = "modelcomparison",
                  uiOutput('mc_config_ui'),
                  uiOutput('mc_outcome_ui')
                  # fluidPage(
                  #   box(
                  #     width=2,
                  #     title = "Best model",
                  #     HTML(
                  #       '<p> Compare models, based on APE and parameter accuracy. <p>'
                  #     ),
                  #     status = "success",
                  #     actionButton("submitModelComparison", "Submit")
                  #     
                  #   ),
                  #   uiOutput("best"),
                  #   uiOutput("cvbest"),
                  #   box(width = 12,
                  #       title = "Plot of variables with x-axis timepoints and y-axis mse",
                  #       plotlyOutput("mseplot")),
                  #   box(
                  #     title = "Applied model",
                  #     status = "success",
                  #     width = 2,
                  #     selectInput(
                  #       "selection2",
                  #       "Choose a model to apply to data",
                  #       model_list,
                  #       selected = "ar"
                  #     ),
                  #     actionButton("submit2", "Submit")
                  #   ),
                  #   box(title = "Parameter accuracy",
                  #       withMathJax(),
                  #       uiOutput("accuracy")
                  #   ),
                  #   infoBoxOutput("mse"),
                  #   infoBoxOutput("paramacc")
                  # )),
          ),
          tabItem(tabName = "networkanalysis",
                  fluidPage(
                    boxPlus(
                      title='Parameters',
                      collapsible=TRUE,
                        uiOutput('select_network_vars_element'),
                        uiOutput('select_network_graph_type'),
                        uiOutput('select_network_treshold'),
                        uiOutput('select_network_tuning')
                        ),
                      plotOutput("networkplot")
                  )
          ),
          tabItem(tabName="faq",
                  fluidPage(
                    box(
                      HTML('placeholder')
                    )
                  )
                  )
      )
        )
      )
    )
}

#' @import shiny
golem_add_external_resources <- function() {
  # library(shinydashboard)
  # library(shinydashboardPlus)
  addResourcePath('www', system.file('app/www', package = 'tsim'))
  # Load packages --------------------------------------------------------------------
  #set.seed('1')
  #initiation of packages thanks to https://gist.github.com/benmarwick/5054846
  # list.of.packages = c(
  #   "shinythemes",
  #   "qgraph",
  #   "shinydashboardPlus",
  #   "Matrix",
  #   "shinyWidgets",
  #   "plotly",
  #   #"caret",
  #   "dplyr",
  #   "data.table",
  #   "lubridate",
  #   "reshape2",
  #   "DT",
  #   "knitr",
  #   "kableExtra",
  #   "datasets",
  #   "ggplot2",
  #   "MASS",
  #   "shiny",
  #   "psych",
  #   "stats",
  #   "shinydashboard",
  #   "reshape2",
  #   "vars",
  #   "xtable",
  #   "Hmisc",
  #   "reshape",
  #   "rowr",
  #   "grid",
  #   "gridExtra",
  #   "rlist",
  #   "pracma",
  #   "latex2exp",
  #   "dplyr",
  #   "rhandsontable",
  #   "lubridate",
  #   #"RSQLite",
  #   "here",
  #   "devtools",
  #   #"sqldf",
  #   #"DBI",
  #   #"dbplyr",
  #   "pool",
  #   #"RMySQL",
  #   "tidyverse",
  #   "tibble",
  #   "tsibble",
  #   "rintrojs",
  #   "shiny"
  # )
  # 
  # new.packages <-
  #   list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
  # if (length(new.packages) > 0) {
  #   install.packages(new.packages)
  # }
  # lapply(list.of.packages, library, character.only = T)
  
  
  tags$head(golem::activate_js(),
            golem::favicon(),
            tags$title("tsim")
            # Add here all the external resources
            # If you have a custom.css in the inst/app/www
            # Or for example, you can add shinyalert::useShinyalert() here
            #tags$link(rel="stylesheet", type="text/css", href="www/custom.css"))
  )
}
