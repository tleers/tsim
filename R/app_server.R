#' @import shiny
#' @import ggplot2
#' @import xtable
#' @import ggridges
#' @import shinydashboardPlus
#' @import DT
#' @import qgraph
#' @import viridis

app_server <- function(input, output, session) {
  observeEvent(input$browser,{
    browser()
  })
  models <- dir('models')
  models <- paste0('models/', models)
  for(i in 1:length(models)){
    source(models[i],local=TRUE)
  }
  source('R/utils_server.R',local=TRUE)
  print(paste0('Loaded model: ',models))
  model_list <- dir('models')
  model_list <- unlist(strsplit(model_list,'.R'))
  
  #------------------------------ Initialize datasets in memory -----------------------
  data(Bringmann2016)
  data(sim_var)
  
  df_list <- c(names(which(sapply(.GlobalEnv, is.data.frame))),
               names(which(sapply(.GlobalEnv, is.matrix)))
  )
  #------------------------------ First User Introduction -----------------------
  
  #Use this for the shinyintrojs - currently not implemented
  # observeEvent(input$help,{
  #   introjs(session,
  #   )
  # })
  
  #------------------------------ Global Dataset Parameters -----------------------
  # input_df set initial parameters
  input_df <- reactiveValues(counter = 0,
                             data_name = NULL,
                             ts_obj = NULL,
                             mts_obj = NULL,
                             df_list = NULL,
                             df_class = NULL,
                             names_list = NULL,
                             df = NULL,
                             class = NULL,
                             var_summary = NULL)
  
  # active_df currently loaded dataset on which all manipulations are performed
  active_df <- reactiveValues(df = NULL,
                              class = NULL,
                              var_summary = NULL,
                              names_list = NULL)
  
  #------------------------------ SideBar Menu -----------------------
  output$data_select_top <- renderUI({
    selectInput(
      'select_df', label = "Select active dataset", choices = input_df$names_list
    )
  })
  
  output$menu <- renderMenu({
    sidebarMenu(
      id="tabs",
      menuItem("Dataset", 
               tabName = "data", 
               icon = icon("table"), 
               startExpanded = TRUE,
               #conditionalPanel(condition="is.null(input.input_df)==FALSE",
               #),
               menuSubItem("Load", tabName = "data1"),
               menuSubItem("Pre-processing",tabName = "data2")
      ),
      menuItem("Data Simulation", icon = icon("database"),tabName = "sim"
               
               #ADDING BADGES CAUSES THE TAB MENU TO RELOAD AND THE TAB TO RESET TO INITIAL VALUE
               # badgeLabel = ifelse(is.null(input$select_dataset_id_var),"X dataset","dataset"),
               # badgeColor = ifelse(is.null(input$select_dataset_id_var),"red","green")
      ),
      menuItem("Analysis", icon = icon("microscope"), tabName = "analysis",
               menuSubItem("Model Comparison", tabName = "modelcomparison"),
               menuSubItem("Timepoint Estimation", tabName="tpestimation")
      ),
      menuItem("Network Analysis",icon=icon("project-diagram"),tabName="networkanalysis"),
      conditionalPanel(condition = "output.loaded_table_flag == '1'", 
                       uiOutput('data_select_top'),
                       uiOutput('id_variable')
      ),
      conditionalPanel(condition = "output.loaded_table_flag == '1' && input.select_dataset_id_var != 'None'", 
                       uiOutput('dataset_id_value')
      )
    )
  })
  
  # observeEvent({input$tabs},{
  #   if (input$tabs == "tpestimation") {
  #     shinyjs::addClass(selector="body",class="control-sidebar-open")
  #   } else {
  #     shinyjs::removeClass(selector="body",class="control-sidebar-open")
  #     }
  # })
  
  tp_tab_script<-renderUI({
      if (input$tabs == "tpestimation") {
        tags$script(HTML(
          "var element = document.getElementById('body');
          element.classList.add('control-sidebar-open');"
        ))
      } else {
        tags$script(HTML(
          "var element = document.getElementById('body');
      element.classList.add('control-sidebar-open');"
        ))
        }
  })
  
  #DOWNLOAD
  output$downloadInnoDataset <- downloadHandler(
    filename = function() {
      paste("inno-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(mod_params()$inno, file)
    }
  )
  
  output$downloadPhiDataset <- downloadHandler(
    filename = function() {
      paste("phi-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(mod_params()$phi, file)
    }
  )

  #TABLES
  
  #currently active - what is actually in the table
  current_phi_input <- reactive({
    if(!is.null(input_df$df) && input$select_simulation_parameter_origin != 'Manual'){
      dphi <- hot_to_r(input$phi)
    } else if (input$select_simulation_parameter_origin == 'Manual'){
      dphi <- hot_to_r(input$phi)
    } 
  })
  
  current_inno_input <- reactive({
    if(!is.null(input_df$df) && input$select_simulation_parameter_origin != 'Manual'){
      dinno <- hot_to_r(input$inno)
    } else if (input$select_simulation_parameter_origin == 'Manual'){
      dinno <- hot_to_r(input$inno)
    }
  })
  
  #display
  output$phi <- renderRHandsontable({
    if(!is.null(updated_phi())){
      rhandsontable(updated_phi())
    }
  })
  
  output$inno <- renderRHandsontable({
    if(!is.null(updated_inno())){
      rhandsontable(updated_inno())
    }
  })
  
  computePhi <- function(model,sim_params, ...){
    class(model)<-tolower(model)
    UseMethod('computePhi',model)
  }
  
  computePhi.ar <- function(model, sim_params, ...) {
    N<-sim_params$nvar
    Phi <- matrix(0, N, N)
    diag(Phi) <- .3 # The diagonal elements
    Phi[diag(1, N) == 0] <- 0 # The off-diagonal elements
    return(Phi)
  }
  
  computePhi.var <- function(model, sim_params, ...) {
    N<-sim_params$nvar
    Phi <- matrix(0, N, N)
    diag(Phi) <- .3 # The diagonal elements
    Phi[diag(1, N) == 0] <- .2 # The off-diagonal elements
    return(Phi)
  }
  
  simParams <- function(model){
    class(model) <- tolower(model)
    UseMethod("simParams",model)
  }
  

  
  simParams.ar <- function(model){
    return(list(nvar=input$nVar))
  }
  
  simParams.var <- function(model){
    return(list(nvar=input$nVar))
  }

  
  #what is the value based off of dataset: initial values for tables
  updated_phi <- reactive({
    if(!is.null(input_df$df) && input$select_simulation_parameter_origin != 'Manual'){
      phi_output <- mod_params()$phi
      colnames(phi_output) <- colnames(filedata_updated())[1:ncol(phi_output)]
      rownames(phi_output) <- colnames(filedata_updated())[1:nrow(phi_output)]
    } else if(input$select_simulation_parameter_origin == 'Manual'){
        phi_output<-computePhi(input$selection1,simParams(input$selection1))
        colnames(phi_output) <- c(paste("V",1:ncol(phi_output),sep=""))
        rownames(phi_output) <- c(paste("V",1:nrow(phi_output),sep=""))
    } else {
      phi_output <- NULL
    }
    phi_output
  })
  
  
  computeSigma <- function(model, sim_params, ...){
    class(model)<-tolower(model)
    UseMethod("computeSigma",model)
  }
  
  computeSigma.ar <- function(model, sim_params, ...) {
    N<-sim_params$nvar
    Sigma <- matrix(0, N, N)
    diag(Sigma) <- .5
    Sigma[diag(1, N) == 0] <- .3
    return(Sigma)
  }
  
  computeSigma.var <- function(model, sim_params, ...) {
    N<-sim_params$nvar
    Sigma <- matrix(0, N, N)
    diag(Sigma) <- .5
    Sigma[diag(1, N) == 0] <- .3
    return(Sigma)
  }
  

  
  updated_inno <- reactive({
    if(!is.null(input_df$df) && (input$select_simulation_parameter_origin != 'Manual')){
      inno_output <- mod_params()$inno
      colnames(inno_output) <- colnames(filedata_updated())[1:ncol(inno_output)]
      rownames(inno_output) <- colnames(filedata_updated())[1:nrow(inno_output)]
      
    } else if(input$select_simulation_parameter_origin == 'Manual'){
      inno_output<-computeSigma(input$selection1,simParams(input$selection1))
      colnames(inno_output) <- c(paste("V",1:ncol(inno_output),sep=""))
      rownames(inno_output) <- c(paste("V",1:nrow(inno_output),sep=""))
    } else {
      inno_output <- NULL
    }
    inno_output
  })
  
  ##### SOURCE NOTE: SIGNIFICANT PART OF CODE USED FOR DATASET LOADING IN DT1 IS BASED ON https://github.com/RamiKrispin/Shiny-App
  #------------------------------ Data Tab 1 - DT1 ---------------------------------------------###############################
  
  #------------------------------ DT1 summary boxes -------------------------------------
  
  output$in_memory_df <- renderValueBox({
    valueBox(
      length(prev_table$data_frame_list), "In-memory data", icon = icon("superscript"),
      color = "light-blue"
    )
  })
  
  output$load_datasets <- renderValueBox({
    valueBox(
      ifelse(is.null(input_df$df_list), 0, length(input_df$df_list)), "Loaded datasets", icon = icon("list"),
      color = "maroon"
    )
  })
  
  #------------------------------ DT1 Selecting the Data Input ------------------------------------- 
  #prev_table is short for "preview table"
  
  prev_table <- reactiveValues(inputs_list = NULL, # Get the list of available dataset to load
                               data_frame_list = df_list, # List of available dataframes in memory
                               file_name = NULL, # If loading csv file, the name of the file
                               file_path = NULL, # If loading csv file, the path of the file
                               class = NULL, # Identify the class of the selected dataset 
                               df_name = NULL  # The name of the selected dataset
  )  
  
  
  observeEvent(input$data_source,{
    #------------------------------ DT1 Loading from data frame or package ------------------------------------- 
    prev_table$inputs_list <- switch(
      input$data_source,
      "data_frame" = {
        # Case I - load in memory data frames
        # If there is no any data frame available in memory
        if(length(prev_table$data_frame_list) == 0){
          showModal(
            modalDialog(
              title = "Warning - No Data Frame",
              HTML(paste("There is no in-memory data frame available.",
                         sep = "<br/>")
              ), 
              size = "s"
            ))
          
          df_return_list <- NA
          # Set the condition for the load button
          output$load_flag <- reactive('0')
          outputOptions(output, "load_flag", suspendWhenHidden = FALSE)
        } else { # Otherwise return the list of available data frames in memory
          df_return_list <- prev_table$data_frame_list
          # Set the condition for the load button
          output$load_flag <- reactive('1')
          outputOptions(output, "load_flag", suspendWhenHidden = FALSE)
        }
        df_return_list
      }
    )
  }) 
  
  #------------------------------ DT1 Setting the csv file path------------------------------------- 
  observeEvent(input$file1,{
    output$load_flag <- reactive('0')
    inFile <- input$file1
    if(!is.null(inFile$datapath)){
      prev_table$file_name <- inFile$name
      prev_table$file_path <- inFile$datapath
      output$load_flag <- reactive('2')
      outputOptions(output, "load_flag", suspendWhenHidden = FALSE)
    } else{
      output$load_flag <- reactive('0')
      outputOptions(output, "load_flag", suspendWhenHidden = FALSE)
    } 
    
  })
  
  #------------------------------ DT1 Loading from data frame or package ------------------------------------- 
  # Feed the list of data frames and 
  #avialable datasets to the menue selection
  output$df_list <- renderUI({
    if(input$data_source == "data_frame" ) {
      selectInput("df_to_load", "Select Data Frame",
                  choices = prev_table$inputs_list )
    }  else if(input$data_source == "inst_pack" ){
      selectInput("df_to_load", "Select Dataset",
                  choices = prev_table$inputs_list )
    }
  })
  
  # Load the data according to the user selection  
  df_tbl_view <- reactive({
    prev_table$class <- NULL
    if(input$data_source == "data_frame" & length(prev_table$data_frame_list) != 0){
      df_view <- NULL
      prev_table$df_name <- input$df_to_load
      df_view <- suppressWarnings(get(input$df_to_load))
      if(length(class(df_view)) > 1 & "data.frame" %in% class(df_view)){
        prev_table$class <- "data.frame"
        df_view <- as.data.frame(df_view)
      } else if(length(class(df_view)) > 1){
        prev_table$class <- class(df_view)[1]
        df_view <- as.data.frame(df_view)
      } else{
        prev_table$class <- class(df_view)
        df_view <- as.data.frame(df_view)
      }
    } else if(input$data_source == "import" & !is.null(prev_table$file_path)){
      df_view <- NULL
      prev_table$class <- NULL
      prev_table$df_name <- substr(prev_table$file_name,1,regexpr(".", prev_table$file_name, fixed = T)-1)
      df_view <- read.csv(prev_table$file_path, stringsAsFactors = FALSE,
               header = input$csv_header,
               sep = input$sep,
               quote = input$quote)
      prev_table$class <- class(df_view)
      
    } else {
      df_view <- NULL
    }
    return(df_view)
  })
  
  # View of the data
  output$view_table <- DT::renderDataTable(
    df_tbl_view(), 
    server = TRUE, 
    rownames = FALSE,
    options = list(pageLength = 10,
                   lengthMenu = c(10, 25, 50))
  )
  
  #------------------------------ DT1 Loading a selected dataset  ------------------------------------- 
  observeEvent(input$load, {
    name <- prev_table$df_name
    type <- NULL
    type <- ifelse(prev_table$class == "data.frame", "Data Frame",
                   ifelse(prev_table$class == "ts", "Time Series",
                          ifelse(prev_table$class == "mts", "Multiple Time Series",
                                 ifelse(prev_table$class == "matrix", "Matrix", 
                                        prev_table$class ))))
    
    if(!name %in% input_df$data_name){
      input_df$data_name <- c(input_df$data_name, name)
      if(is.null(input_df$loaded_table)){
        input_df$loaded_table <- data.frame(name = name,
                                            var = ncol(df_tbl_view()),
                                            row = nrow(df_tbl_view()),
                                            class = type,
                                            stringsAsFactors = FALSE)
        
      } else {
        temp <- data.frame(name = name,
                           var = ncol(df_tbl_view()),
                           row = nrow(df_tbl_view()),
                           class = type,
                           stringsAsFactors = FALSE)
        input_df$loaded_table <- rbind(input_df$loaded_table,temp)
        
        temp <- NULL    
      }
      if(is.null(input_df$df_list)){
        
        input_df$df_list <- list(df_tbl_view())
        input_df$df_class <- list(type)
        
      } else {
        input_df$df_list[[length(input_df$df_list) + 1]] <- df_tbl_view()
        input_df$df_class[[length(input_df$df_list)]] <- type
      }
      names(input_df$df_list)[length(input_df$df_list)] <- name
      input_df$names_list <- names(input_df$df_list)
    } else{
      input_df$df_list[[which(names(input_df$df_list) == name)]] <- df_tbl_view()
      input_df$df_class[[which(names(input_df$df_list) == name)]] <- type
    }
  })
  #------------------------------ DT1 Setting the condition for the "Remove" button  ------------------------------------- 
  observeEvent(input_df$loaded_table,{
    if(is.null(input_df$loaded_table)){
      output$loaded_table_flag <- reactive("0")
      outputOptions(output, "loaded_table_flag", suspendWhenHidden = FALSE)
    } else {
      output$loaded_table_flag <- reactive("1")
      outputOptions(output, "loaded_table_flag", suspendWhenHidden = FALSE)
    }
    
  })
  #------------------------------ DT1 Activate the "Remove" button ------------------------------------- 
  observeEvent(input$remove,{
    
    if(length(input_df$df_list)>1){
      input_df$df_list[[input$list_loaded_df_rows_selected]] <- NULL
      input_df$df_class[[input$list_loaded_df_rows_selected]] <- NULL
      input_df$loaded_table <- input_df$loaded_table[-input$list_loaded_df_rows_selected,]
      input_df$data_name <- names(input_df$df_list)
      input_df$names_list  <- input_df$data_name
    } else {
      input_df$df_list <- NULL
      input_df$loaded_table <- NULL
      input_df$data_name <- NULL
      input_df$names_list <- NULL
      input_df$df_class <- NULL
      input_df$names_list <- "NA"
      output$loaded_table_flag <- reactive("0")
      outputOptions(output, "loaded_table_flag", suspendWhenHidden = FALSE)
    }
  })
  
  #------------------------------ DT1 Loaded dataset table ------------------------------------- 
  output$list_loaded_df <- DT::renderDataTable(
    data.frame(input_df$loaded_table), 
    colnames = c("Dataset Name", "Num. of Variables", "Num. of Obs", "Data Type"),
    selection = list(selected = 1, mode = 'single'), 
    options = list(pageLength = 10,
                   lengthMenu = c(10, 25, 50))
  )
  
  ##### SOURCE NOTE: SIGNIFICANT PART OF CODE USED FOR DATA VISUALIZATION IN DT2 IS BASED ON https://github.com/RamiKrispin/Shiny-App
  #------------------------------ DATA TAB 2 -------------------------------------   
  #------------------------------ DT2 summary boxes -------------------------------------
  output$data_name <- renderValueBox({
    valueBox(
      input$select_df, input_df$class, icon = icon("folder-open"),
      color = "green"
    )
  })
  
  #ignore errors here
  output$num_var <- renderValueBox({
    valueBox(
      ncol(filedata_updated()),
      "Variables", 
      icon = icon("superscript"),
      color = "light-blue"
    )
  })
  
  output$num_obs <- renderValueBox({
    valueBox(
      nrow(filedata_updated()),
      "Observations",
      icon = icon("list"),
      color = "maroon"
    )
  })
  
  observeEvent({
    input_df$names_list
  },{
    output$id_variable <- renderUI({
      selectInput("select_dataset_id_var", "Select the ID variable",
                  choices = c('None',names(input_df$df))
      )
    })
  })
  
  observeEvent({
    input_df$names_list
  },{
    output$dataset_select_index_variable <- renderUI({
      selectInput("dataset_select_index_variable", "Select the exogeneous variables",
                  multiple=TRUE,
                  choices = c('None',names(input_df$df))
      )
    })
  })
  
  observeEvent({
    input_df$names_list
  },{
    output$dataset_id_value <- renderUI({
      selectInput("current_dataset_id_value", "Select the desired ID value",
                  choices = c('None',unique(input_df$df[,id_var_number()])),
                  selectize = TRUE
      )
    })
  })
  
  
  output$num_var_sim <- renderUI({
    numericInput(
      "nVar",
      "Number of variables:",
      if(!is.null(input$select_dataset_id_var) && !is.null(input_df$df)){
        if(input$select_dataset_id_var != 'None'){
          ncol(input_df$df)-1
        } else {
          ncol(input_df$df)
        }
      } else if (!is.null(input_df$df)){
        ncol(input_df$df)
      } else {
        3
      },
      min = 2,
      max = 150
    )
  })

  output$num_tp_sim <- renderUI({
    numericInput(
      "nTime",
      "Number of time points:",
      # if(is.null(input_df$df)){
      #   15
      # } else if(input$current_dataset_id_value != 'None'){
      #   nrow(input_df$df %>% dplyr::filter_at(id_var_number(), all_vars(.==input$current_dataset_id_value)))
      # } else if (!is.null(active_df$df)){
      #   nrow(input_df$df)
      # } else {
      #   
      # },
      20,
      min = 20,
      max = 10000
    )
  })
  
  output$num_searchtp_sim <- renderUI({
    numericInput(
      "nTime_tp",
      "Starting time point:",
      20,
      min = 15,
      max = 10000
    )
  })
  
  # observeEvent(input$selection1,
  #              input$nvar,
  #              input_df$df,{
  #                tmp<-
  #                  reactive({
  #                    callModule(get(paste0('simRenderE.',input$selection1)),'test',list(input, output, session, input_df, r, mod_params()))
  #                    attach(tmp)
  #                })
  #                output$sim_params <- renderUI({
  #                  do.call(paste0('simRenderUI.',input$selection1),'test')
  #   })
  # 
  # })
  
  output$simulation_parameter_origin <- renderUI({
    if(!is.null(input_df$df)){
      data_list <- c('Manual', 'Active dataset') 
    } else {
      data_list <- c('Manual') 
    }
    tagList(
      selectInput(
        "select_simulation_parameter_origin",
        "Select parameter estimate source",
        choices=data_list,
        selected='Manual',#ifelse(!is.null(input_df$data_name),'Active dataset','Manual'),
        multiple = FALSE
      )
    )
  })
  
  observeEvent(input$select_df, {
    if(!is.null(input$select_df)){
      input_df$df <- (
        input_df$df_list[[which(names(input_df$df_list) == input$select_df)]]
      )
      input_df$class <- input_df$df_class[[which(names(input_df$df_list) == input$select_df)]]
      input_df$df <- data.frame(input_df$df)
      uiOutput('data_tab2_table')
      
      input_df$df <- (
        input_df$df_list[[which(names(input_df$df_list) == input$select_df)]]
      )
      input_df$df <- data.frame(input_df$df)
      active_df$class <- input_df$df_class[[which(names(input_df$df_list) == input$select_df)]]
      
    } else{
      input_df$df <- NULL
      input_df$class <- NULL
      
      input_df$df <- NULL
      active_df$class <- NULL
      
      output$data_tab2_table <- NULL
    }
  })
  
  
  #------------------------------ Data tab 2 - Data Prep -------------------------------------    
  #------------------------------ Data tab 2 - Creating Variables Table ------------------------------------- 
  
  #reset, else app will crash when switching from one dataset to another when the selected column is larger than the available columns
  #right now, we have a workaround by only letting the var summary table change when the row is changed. ideally this would also reset when changing datasets.
  proxy_data_tab2_var = dataTableProxy('data_tab2_var')
  observeEvent({
    input$data_option
    input$select_df
    input$current_dataset_id_value
    input$select_dataset_id_var
  }, {
    proxy_data_tab2_var %>% selectRows(NULL)
    proxy_data_tab2_var %>% selectColumns(NULL)
  },
  priority = 100)
  
  id_var <- reactive({
    if(!is.null(input$select_dataset_id_var) && input$select_dataset_id_var != 'None'){
      input$select_dataset_id_var
    } else {
      NULL
    }
  })
  
  id_var_number <- reactive({
    which( colnames(input_df$df)==input$select_dataset_id_var)
  })
  
  loaded_dataset_index_variable <- reactive({
    dlist <- names(filedata_updated())
    ids <- NULL
    if(!is.null(input$dataset_select_index_variable)){
      for(i in 1:length(input$dataset_select_index_variable)){
        ids[i] <- which(dlist == input$dataset_select_index_variable[i])
      }
    }
    ids
  })
  
  #currently loaded dataset id variable selected value
  loaded_dataset_id_value <- reactive ({
    if(!is.null(input$current_dataset_id_value) && input$current_dataset_id_value != 'None'){
      input$current_dataset_id_value
    } else {
      NULL
    }
  })
  
  numVarsData<-reactive({
    if(!is.null(input$select_dataset_id_var) && !is.null(input_df$df)){
      if(input$select_dataset_id_var != 'None'){
        ncol(input_df$df)-1
      } else {
        ncol(input_df$df)
      }
    } else if (!is.null(input_df$df)){
      ncol(input_df$df)
    }
  })
  
  observeEvent({
    input$select_simulation_parameter_origin
  },{
    if(input$select_simulation_parameter_origin!='Manual'){
      updateNumericInput(session,
                         'nVar',
                         label="Number of variables:",
                         value=numVarsData(),
                         max=numVarsData(),
                         min=numVarsData()
                         
      ) 
      } else {
        updateNumericInput(session,
                           'nVar',
                           label="Number of variables:",
                           value=numVarsData(),
                           max=150,
                           min=1
        )
      }
    
  })
  
  observeEvent({input$data_option
    input_df$df
    input$select_df
  }, {
    if((input$data_option == "var_attr" | input$data_option =="data_reshape") &
       !is.null(input_df$df) &
       !is.null(input_df$loaded_table)
    ){
      var.names <- names(input_df$df)
      var.class <- NULL
      for(i in 1:ncol(input_df$df)){
        if(length(class(input_df$df[,i])) > 1){
          if("factor" %in% class(input_df$df[,i])){
            var.class <- c(var.class, "factor")
          } else {
            var.class <- c(var.class, "NA")
          }
        } else {
          var.class <- c(var.class, class(input_df$df[,i])[1])
        }
      }
      input_df$var_summary <- data.frame(var.names, var.class, stringsAsFactors = FALSE)
      names(input_df$var_summary) <- c("Name", "Class")
      output$data_tab2_var <- DT::renderDataTable(
        input_df$var_summary,
        server = FALSE, rownames = FALSE,
        selection = list(selected = 1, mode = 'single'), 
        options = list(lengthMenu = c(5, 10, 15, 20), pageLength = 10, dom = 'p')
      )
      
    } 
    
  })
  
  output$class_df_flag <- reactive({
    ifelse(is.ts(input_df$df), TRUE, FALSE)
  })
  
  outputOptions(output, "class_df_flag", suspendWhenHidden = FALSE)    
  #------------------------------ Data tab 2 - Creating Variable Summary -------------------------------------  
  
  observeEvent({input$data_tab2_var_rows_selected},
               priority = -100,{
                 r1 <- input$data_tab2_var_rows_selected
                 if(is.numeric(input_df$df[, r1]) | is.integer(input_df$df[, r1])){
                   var.mean <- mean(input_df$df[, r1], na.rm = TRUE)
                   var.min  <- min(input_df$df[, r1], na.rm = TRUE)
                   var.max  <- max(input_df$df[, r1], na.rm = TRUE)
                   var.median <- median(input_df$df[, r1], na.rm = TRUE)
                   var.sd <- sd(input_df$df[, r1])
                   var.na <- sum(is.na(input_df$df[,r1]))
                   summary.vec <- c(var.mean, var.min, var.max, var.median, var.sd,var.na)
                   var_s <- data.frame(summary.vec)
                   names(var_s) <- names(input_df$df)[r1]
                   row.names(var_s) <- c("Mean", "Min", "Max", "Median", "Standard Deviation", "Missing values")
                   p <- plot_ly(y = ~ input_df$df[, r1], type = "box", name = names(input_df$df)[r1],
                                boxpoints = "all", jitter = 0.3,
                                pointpos = -1.8)%>%
                     layout(yaxis = list(title = "Range"))
                 } else if(is.factor(input_df$df[, r1]) | is.character(input_df$df[, r1])){
                   if(is.character(input_df$df[, r1])){
                     std_input_df <- as.factor(input_df$df[, r1])
                   } else {
                     std_input_df <- input_df$df[, r1]
                   }
                   var.n.levels <- length(levels(std_input_df))
                   var.levels <- NULL
                   for(i in 1:var.n.levels){var.levels <- c(var.levels,levels(std_input_df)[i])}
                   var_s <- c(var.n.levels)
                   var_s <- data.frame(var_s)
                   row.names(var_s) <- c("Number of Levels")
                   names(var_s) <- names(input_df$df)[r1]
                   factor.df <- group_by(input_df$df, get(names(input_df$df)[r1])) %>%
                     summarise(count = dplyr::n())
                   names(factor.df) <- c(names(std_input_df), "Count")
                   p <- plot_ly(data = factor.df, name = "Levels",
                                x =  ~ get(names(factor.df)[1]),
                                y =  ~ get(names(factor.df)[2]), 
                                type = "bar") %>%
                     layout(yaxis = list(title = "Count"),
                            xaxis = list(title = "Levels"))
                 } else if(is.Date(input_df$df[, r1])){
                   var_s <- NULL
                   var_s <- data.frame(c(as.character(min(input_df$df[, r1])), 
                                         as.character(max(input_df$df[, r1]))), row.names = c("Start/Min Date", "End/Max Date"))
                   names(var_s) <- names(input_df$df)[r1]
                   p <- NULL
                 } 
                 
                 
                 # render the data summary into table
                 output$data_tab2_var_summary <- renderTable(var_s, rownames = TRUE)
                 output$data_tab2_var_summary_descr <- renderTable(summarytools::descr(input_df$df[r1]))
                 output$data_tab2_summary_plot <- renderPlotly(p)
                 
                 
               })
  #------------------------------ Data tab 2 - Midifing Variables Attributes -------------------------------------  
  observeEvent(input$remove_var,{
    input_df$df[,input$data_tab2_var_rows_selected] <- NULL
  })
  
  observeEvent(input$var_modify,{
    if(!is.ts(input_df$df)){
      r2 <- input$data_tab2_var_rows_selected
      input_df$df[,r2] <- switch(input$class_selection,
                                 "numeric" = as.numeric(input_df$df[,r2]),
                                 "factor" = as.factor(input_df$df[,r2]),
                                 "character" = as.character(input_df$df[,r2]),
                                 "date" = {eval(parse(text = 
                                                        paste("lubridate::",
                                                              input$date_format,
                                                              "('", 
                                                              as.character(input_df$df[,input$data_tab2_var_rows_selected]),
                                                              "')",
                                                              sep = "")))
                                 }
      )
      input_df$df_list[[which(names(input_df$df_list) == input$select_df)]] <- input_df$df
    }
  })
  
  observeEvent({input$date_format
    input$data_tab2_var_rows_selected
    input$class_selection
    input$select_df
  },{
    if(!is.ts(input_df$df)){
      new.date <- input_df$df[1,input$data_tab2_var_rows_selected]
      new.date <- as.character(new.date)
      output$date_prev <-  renderPrint(eval(parse(text = 
                                                    paste("lubridate::",
                                                          input$date_format,
                                                          "('", 
                                                          new.date[1],
                                                          "')",
                                                          sep = "")))
      )
    }
  })
  
  
  
  
  observeEvent(input$tabs,{
    if((input$tabs == "data2" | input$tabs == "vis") & is.null(input_df$df_list)){
      showModal(modalDialog(
        title = "Warning - No Loaded Dataset",
        HTML(paste("There is no loaded dataset. ",
                   "Please load a dataset before initialization.", 
                   sep = "<br/>")
        ), size = "s"
      ))
    }
    
    if((input$tabs == "modelcomparison" | input$tabs == "tpestimation") & (is.null(input_df$df_list) && is.null(r$data))){
      showModal(modalDialog(
        title = "Warning - No Loaded Dataset",
        HTML(paste("There is no loaded dataset.",
                   "Please load or simulate a dataset before analysis.", 
                   sep = "<br/>")
        ), size = "s"
      ))
    }
  })
  
  # observeEvent(input$tabs,{
  #   if((input$tabs == "tpestimation" | input$tabs == "sim") & is.null(input_df$df_list)){
  #     showModal(modalDialog(
  #       title = "Warning - No Loaded Dataset",
  #       HTML(paste("There is no loaded dataset ",
  #                  "Please select input and load it", 
  #                  sep = "<br/>")
  #       ), size = "s"
  #     ))
  #   }
  # })
  #------------------------------ Data tab 2 - End ------------------------------------- 
  #------------------------------ Visualization Tab Start -------------------------------------  
  # Selecting the Dataset
  # Setting reactive values
  vis_df <- reactiveValues(df = NULL,
                           class = NULL,
                           var_factor = NULL,
                           var_numeric = NULL,
                           var_date = NULL)
  
  # Setting the data selection
  observeEvent({
    input_df$names_list
  },{
    
    output$loaded_ds_list_vis  <- renderUI({
      selectInput("select_df_vis", "Select Dataset",
                  choices = input_df$names_list
      )
    })
    
  })
  
  
  observeEvent({
    input$var_modify
    input$select_df
  }, {
    if(!is.null(input$select_df_vis)){
      vis_df$df <- (
        input_df$df_list[[which(names(input_df$df_list) == input$select_df_vis)]]
      )
      vis_df$class <- input_df$df_class[[which(names(input_df$df_list) == input$select_df_vis)]]
      
      vis_df$var_numeric <- vis_df$var_factor <- NULL
      for(i in 1:ncol(vis_df$df)){
        if(is.factor(vis_df$df[,i])){
          vis_df$var_factor <- c(vis_df$var_factor, names(vis_df$df)[i])
        } else if(is.numeric(vis_df$df[,i]) | is.integer(vis_df$df[,i])){
          vis_df$var_numeric <- c(vis_df$var_numeric,names(vis_df$df)[i])
        }
      }
      
    } else{
      
      vis_df$df <- NULL
      vis_df$class <- NULL
      vis_df$var_factor <- NULL
      vis_df$var_numeric <- NULL
      
      
      
    }
  })
  
  observeEvent({input$submit1},
               {
                 if(!is.null(r$data)){
                   output$simulated_data_plot <- renderPlotly({
                     x <- 1:nrow(r$data)
                     colnames(r$data)<-c(paste("V",1:ncol(r$data),sep=""))
                     p<-ggplot(reshape::melt(cbind(r$data),id.vars=x),
                            aes(x=X1,y=value,color=X2)) +
                       geom_line() + 
                       scale_x_continuous(name="Time points") +
                       labs(fill="Variable") + 
                       scale_y_continuous(name='Value') +
                       theme_classic() 
                     
                     ggplotly(p) %>% 
                       layout(autosize=TRUE)
                 
                   })
               }
               })
    
    
  observeEvent({input$var_modify
    input$plot_factor
    input$plot_var
    input$plot_x
    input$plot_y
    input$plot_type
    vis_df$df
    input$select_df_vis
    
  },{
    
    output$main_plot <- renderPlotly({
      
      if(!is.ts(vis_df$df)){
        p <- x <- y <- color <-   NULL
        
        if(length(vis_df$var_numeric) > 1){
          y <- vis_df$df[,input$plot_y]
        } else if(length(vis_df$var_numeric) == 1){
          y <- NA
        }
        
        if(input$plot_type == "box" | input$plot_type == "density"){
          x <- vis_df$df[, input$plot_var]
        } else {
          x <- vis_df$df[,input$plot_x]
        }
        
        if(input$plot_factor != "None" & input$plot_factor != "NA" & !is.null(input$plot_factor)){
          color <- vis_df$df[,input$plot_factor]
          type <- vis_df$df[,input$plot_factor]
        } else {
          color <-  NULL
          type <- input$plot_var
        }
        
        p <- switch(input$plot_type,
                    "scatter" = {          
                      plot_ly(x = x, y = y, color = color) %>%
                        layout(xaxis = list(title = input$plot_x),
                               yaxis = list(title = input$plot_y))
                    },
                    "line" = {
                      plot_ly(x = x, y = y, mode = "lines", color = NULL)%>%
                        layout(xaxis = list(title = input$plot_x),
                               yaxis = list(title = input$plot_y))
                    },
                    "box" = {
                      plot_ly(y = x, type = "box", color = color, 
                              name = names(vis_df$df)[which(names(vis_df$df) == input$plot_factor)],
                              boxpoints = "all", jitter = 0.3,
                              pointpos = -1.8)%>%
                        layout(yaxis = list(title = names(vis_df$df)[which(names(vis_df$df) == input$plot_x)]),
                               xaxis = list(title = "")
                        )
                    },
                    "hist" = {
                      p_hist <- NULL
                      if(input$plot_factor == "None" | input$plot_factor == "NA"){
                        p_hist <- plot_ly(x = vis_df$df[,input$plot_var], type = "histogram") 
                      } else if(input$plot_factor != "None" & 
                                input$plot_factor != "NA" & 
                                !is.null(input$plot_factor)){
                        
                        plot_list <- l <- NULL
                        for(l1 in levels(vis_df$df[,input$plot_factor])){
                          hist.sub.df <- subset(vis_df$df, vis_df$df[,input$plot_factor] == l1)
                          l <- length(plot_list)
                          plot_list[[l + 1]] <- plot_ly(hist.sub.df, 
                                                        x = hist.sub.df[,input$plot_var], 
                                                        name = l1) %>%
                            layout(xaxis = list(title = l1),
                                   title = input$plot_var)
                          
                        }
                        p_hist <- subplot(plot_list, titleX = TRUE, shareX = TRUE) %>% 
                          hide_legend()
                      }  
                      p_hist
                    },
                    "density" = {
                      plot_den <- NULL
                      if(input$plot_factor == "None" | input$plot_factor == "NA"){
                        dens <- density(x)
                        dens.df <- data.frame(x = dens$x, y = dens$y)
                        min_y <- 0 
                        max_y <- max(dens.df$y)
                        plot_den <- plot_ly(data = dens.df, x  = ~x, 
                                            y = ~y)
                      } else if(input$plot_factor != "None" & 
                                input$plot_factor != "NA" & 
                                !is.null(input$plot_factor)){ 
                        
                        plot_list_den <- l <-  NULL 
                        for(l2 in levels(vis_df$df[, input$plot_factor])){
                          df.den <- subset(vis_df$df, 
                                           vis_df$df[, input$plot_factor] == l2)
                          l <- length(plot_list_den)
                          dens <- density(df.den[,input$plot_var])
                          dens.df <- data.frame(x = dens$x, y = dens$y)
                          plot_list_den[[l + 1]] <- plot_ly(data = dens.df, 
                                                            x = ~x, 
                                                            y = ~y)%>%
                            layout(xaxis = list(title = l2),
                                   title = input$plot_var)
                        }
                        
                        plot_den <- subplot(plot_list_den, titleX = TRUE, shareX = TRUE)%>% 
                          hide_legend()
                      }
                      plot_den
                      
                      
                    },
                    "cor" = {
                      c <- NULL
                      c <- round(cor(vis_df$df[, which(colnames(vis_df$df) %in% vis_df$var_numeric)]), 3)
                      plot_ly(x = vis_df$var_numeric, y = vis_df$var_numeric, z = c, 
                              key = c, type = "heatmap", source = "heatplot")
                    }
        )
      }
    }) 
    return(p)
  })
  
  output$class_df_flag_vis <- reactive({
    ifelse(is.ts(vis_df$df), TRUE, FALSE)
  })
  outputOptions(output, "class_df_flag_vis", suspendWhenHidden = FALSE)  
  
  #------------------------------ Simulation -------------------------------------------------------------------
  
  modelDataArgs <- function(model){
    class(model)<-tolower(model)
    UseMethod('modelDataArgs',model)
  }
  
  computeDataArgs <- function(model){
    class(model)<-tolower(model)
    UseMethod('computeDataArgs',model)
  }
  
  tmod <- reactive({
    tmod<-do.call(
      modelData,
      list(
        input$selection1,
        filedata_updated(),      
        selectedLagNum(),
        loaded_dataset_index_variable(),
        simParams(input$selection1)
      )
    )
    tmod
  })
  
  mod_params <- reactive({
    if(!is.null(tmod) && data_simulation_parameter_origin() != 'Manual'){
      mod_params<-relevantModelParameters(tmod())
    } else {
      mod_params<-currentModelParameters(input$selection1)
    }
    #print(mod_params)
    mod_params
  })
  
  filedata_updated <- reactive ({
    if(!is.null(input_df$df)){
      if(!is.null(id_var()) && !is.null(loaded_dataset_id_value())){
        input_df$df %>% 
          dplyr::filter_at(id_var_number(), all_vars(. == as.integer(loaded_dataset_id_value()))) %>% 
          dplyr::select(-starts_with(input$select_dataset_id_var)) %>% na.omit()
      } else if (!is.null(id_var()) && is.null(loaded_dataset_id_value())){
        input_df$df %>% 
          dplyr::select(-starts_with(input$select_dataset_id_var)) %>% na.omit()
      } else {
        input_df$df %>% na.omit()
      }
    }
  })
  
  
  # observeEvent({
  #   input$selection1},{
  #     if(!is.null(get0(prev_sim))){
  #       prev_sim <- reactive({
  #         input$selection1
  #       })
  #     } else {
  #       prev_sim <- function(){
  #         'ar'
  #       }
  #     }
  # 
  # })
  
  prev_sim <<- reactiveVal(NULL)

  observeEvent({input$tp_model1},{
    if(!is.null(prev_mod())){
      removeUI(selector=paste0('div#',prev_mod(),'_mod_output'))
    }
    insertUI(
      selector='#mod_anchor1',
      where='afterEnd',
      ui=uiOutput(paste0(input$tp_model1,'_mod_output'))
    )
    prev_mod(input$tp_model1)
  })
  
  observeEvent({input$tp_model2},{
    if(!is.null(prev_mod2())){
      removeUI(selector=paste0('div#',prev_mod2(),'_mod_output'))
    }
    insertUI(
      selector='#mod_anchor2',
      where='afterEnd',
      ui=uiOutput(paste0(input$tp_model2,'_mod_output'))
    )
    prev_mod2(input$tp_model2)
  })
  
  observeEvent({input$selection1},{
    if(!is.null(prev_sim())){
      removeUI(selector=paste0('div#',prev_sim(),'_sim_output'))
    }
    #model_specific_sim_output <- 
    insertUI(
      selector='#sim_anchor',
      where='afterEnd',
      ui=uiOutput(paste0(input$selection1,'_sim_output'))
    )
    prev_sim(input$selection1)
  })
  
  output$ar_sim_output <- renderUI({
    tagList(
      transitionMatrixUI(ns(session)$ns,'phi'),
      innovationMatrixUI(ns(session)$ns,'inno')
    )
  })
  
  output$var_sim_output <- renderUI({
    tagList(
      transitionMatrixUI(ns(session)$ns,'phi'),
      innovationMatrixUI(ns(session)$ns,'inno')
    )
  })
  # getModelUIList.pcvar <- function(model, label){
  #   return(list(
  #   list("loadingMatrixUI",list("ns(session)$ns","label"))
  #   )
  #   )
  # }   
  # 
  # getModelUIList <- function(model, ...){
  #   class(model)<-tolower(model)
  #   UseMethod('getModelUIList',model)
  # }
  
  #original df of currently loaded dataset
  filedata <- reactive({
    input_df$df
  })
  
  data_simulation_parameter_origin <- reactive ({
    if(!is.null(input$select_simulation_parameter_origin)){
      input$select_simulation_parameter_origin
    } else {
      NULL
    }
  }) 
  
  
  selectedSimMod <- reactive({
    input$selection1
  })
  
  selectedLagNum <- reactive({
    input$lagNum
  })
  
  
  selected_nvar <- reactive({
    input$nVar
  })
  
  r <-
    reactiveValues(
      data = NULL,
      nVar = NULL,
      nTime = NULL,
      error = NULL,
      diagPhi = NULL,
      innoVar = NULL,
      innoCovar = NULL,
      offdiagPhi = NULL,
      nModel1 = NULL,
      nModel2 = NULL
    )
  
  cv <-
    reactiveValues(compute = NULL,
                   comparison = NULL)
  
  acc <-
    reactiveValues(comparison = NULL)
  
  output$sim_data_vis <- renderUI({
    if(!is.null(r$data)){
      selectInput('select_sim_data_vis', 'Show simulated dataset',
                  choices= c("Yes","No"),
                  "No")
    }
  })
  
  output$data_tab2_table <- DT::renderDataTable(
    if(input$data_tab2_na_omit == 'Yes'){
      filedata_updated()
      
    } else {
      na.omit(filedata_updated())
    },
    selection = list(selected = 1, 
                     mode = 'single'), 
    options = list(pageLength = 10,
                   lengthMenu = c(10, 25, 50)
    )
  )
  
  observeEvent(input$submit1, {
    r$data <<-
      do.call(computeData,list(input$nVar,
                               input$nTime,
                               0,
                               input$selection1,
                               val=TRUE,
                               burn=1000,
                               currentModelParameters(input$selection1)#model-specific parameters
      )
      )

    
    if(!is.null(r$data)){
      if(any(colMeans(matrix(r$data,ncol=input$nVar))==0)){
        r$data <- NULL
        showNotification("Dataset simulation succesful, but one or multiple output columns are only zero.", type='error')
      } else {
        showNotification("Parameters succesfully loaded. Simulated dataset initialized.",
                         type="message")
      }

    } else {
      showNotification("Parameters failed to load. Simulated dataset failed to initialize.",
                       type="error")
    }
  })
  
  observeEvent(input$submit2, {
    if(is.null(r$data)){
      return(NULL)
    }
    r$nModel2 <- input$selection2
    cv$compute <- computeCV(r$data, r$nModel2, r$nTime, r$nVar, selectedLagNum())
  })
  
  current_k_fold_selected <- reactive({
    input$select_k_fold
  })
  
  current_max_iter_selected <- reactive({
    input$select_max_iter
  })
  
  current_stepsize_init_selected <- reactive({
    ifelse(!is.null(input$select_stepsize_init),input$select_stepsize_init,5)
  })
  
  current_stepsize_scaler_selected<- reactive({
    input$select_stepsize_scaler
  })
  
  tp_selected_model1 <- reactive({
    input$tp_model1
  })
  
  tp_selected_model2 <- reactive({
    input$tp_model2
  })
  
  tp_selected_error_metric <- reactive({
    input$tp_error_metric
  })
  
  output$select_stepsize_init_element <- renderUI({
    numericInput(inputId='select_stepsize_init',
                 label='Choose initial stepsize',
                 min=1,max=1000,
                 value=5
    )
  })

  
  tpModParams <- function(model,...){
    class(model) <- tolower(model)
    UseMethod('tpModParams',model)
  }
  
  
  tpModParams.var <- function(model,...){
  }
  
  tpModParams.ar <- function(model,...){
  }
  observeEvent(input$submitTPS, {
    if (is.null(r$data)) {
      return(NULL)
    }
    
    error_metric <- input$tp_error_metric
    
    K <- input$select_k_fold
    max_iter <- input$select_max_iter
    stepsize_scaler <- input$select_stepsize_scaler
    stepsize_init <-  current_stepsize_init_selected()
    


    
    tp <- searchTP(
      input$nVar,
      time_searchtp(),
      input$error,
      input$selection1,
      tp_selected_model1(),
      tp_selected_model2(),
      input$lagNum,
      K,
      max_iter,
      stepsize_init,
      stepsize_scaler,
      input$threshold,
      loaded_dataset_index_variable(),
      error_metric,
      currentModelParameters(input$selection1),
      tpModParams(input$tp_model1),
      tpModParams(input$tp_model2)
    )
    

    
    #FUNCTION RETURNS NULL IF ERRORS OCCURR AND AS SUCH WE WILL NOT RENDER ANYTHING
    if(!is.null(tp)){
      pldf<-tp[[2]][[1]]
      output$tp <- renderUI({
        valueBox(tp[[1]],'recommended time points.', 
                 icon = icon('hourglass'),
                 color="green")
      })
      
      if(!is.null(tp[[2]][[2]])){
        fold_df <- tp[[2]][[2]]
        # write.csv(fold_df,paste0('fold_df_',input$select_df,
        #                          '_',lubridate::day(lubridate::today()),
        #                          '_',lubridate::month(lubridate::today()),
        #                          '_',lubridate::hour(lubridate::now()),
        #                          '_',lubridate::minute(lubridate::now()),
        #                          '_',lubridate::second(lubridate::now()),
        #                          '_.csv'))
        # write.csv(pldf,paste0('avg_df',input$select_df,
        #                       '_',lubridate::day(lubridate::today()),
        #                       '_',lubridate::month(lubridate::today()),
        #                       '_',lubridate::hour(lubridate::now()),
        #                       '_',lubridate::minute(lubridate::now()),
        #                       '_',lubridate::second(lubridate::now()),
        #                       '_.csv'))
        
      }
      
      modl <- list(tp_selected_model1(),
                   tp_selected_model2())
      
      output$tp_last_1 <- renderUI({
        valueBox(tp[[3]],
                 paste0(toupper(error_metric),' of ', toupper(tp_selected_model1()),' at timepoint ',tp[[1]]),
                 icon = icon('hourglass'),
                 width=8,
                 color="blue")
      })
      output$tp_last_2 <- renderUI({
        valueBox(tp[[4]],
                 paste0(toupper(error_metric),' of ', toupper(tp_selected_model2()),' at timepoint ',tp[[1]]),
                 icon = icon('hourglass'),
                 width=8,color="red")
      })
      
      output$tp_plots <- renderUI({
        fluidRow(
          boxPlus(
            title = paste0(toupper(error_metric),' per fold'),
            closable=TRUE,
            width=NULL,
            collapsible=TRUE,
            plotlyOutput("mse_fold_plot")
          ),
          boxPlus(
            title = paste0('Density plot of average ',toupper(error_metric),' per model across time points'),
            closable=TRUE,
            width=NULL,
            collapsible=TRUE,
            plotOutput("tp_3d_plot")
            
          ),
          # boxPlus(
          #   title = paste0('Plot of average ',toupper(error_metric),' difference across time points'),
          #   closable=TRUE,
          #   width=NULL,
          #   collapsible=TRUE,
          #   plotlyOutput("distr_mse_fold_plot")
          #   
          # ),
          boxPlus(
            title = paste0('Density plot of ',toupper(error_metric),' for both models.'),
            closable=TRUE,
            width=NULL,
            collapsible=TRUE,
            enable_sidebar=TRUE,
            solidheader=TRUE,
            status="success",
            plotlyOutput("distr_mse_fold_plot2"),
            sidebar_width = 25,
            sidebar_start_open = TRUE,
            sidebar_content = tagList(
              selectInput(inputId="select_distr_mse_fold_plot2_tp",
                          label="Which time point",
                          choices=c(unique(pldf$tl)),
                          multiple=TRUE,
                          selectize=TRUE
              )
            )
          )
        )
      })
      
      pldf<-pldf %>% group_by(tl) %>% mutate(count=dplyr::n())
      
      if((fold_df %>% 
          dplyr::filter(model==modl[2]) %>% 
          dplyr::select(1) %>% 
          nrow()
      ) > 1
      ){
        
        output$mse_fold_plot <- renderPlotly({
          max_mse<-max(pldf$mse)
          key <-row.names(fold_df)
          p<- #ggplot(fold_df,aes(x=tl,y=mse,key=row.names(fold_df))) + 
            ggplot(fold_df,aes(x=tl,y=mse)) + 
            geom_line(aes(linetype=as.factor(fold),colour=model),size=.3) + 
            geom_line(data=pldf,aes(x=tl,y=mse,colour=model),size=.8) + 
            #geom_point(data=pldf,aes(x=tl,y=mean(mse),colour=model),size=1.4) + 
            #geom_linerange(data=pldf,position='dodge',aes(ymin=0,ymax=max_mse/10*count)) +
            #geom_text(data=subset(pldf,count<=1),position='dodge',aes(label=count,vjust=max_mse/10*count)) +
            scale_colour_manual(values = c("Blue", "Red")) +
            scale_y_continuous(name=toupper(error_metric)) +
            scale_x_continuous(name="Time points") +
            
            theme_classic() +
            labs(fill="(Model,Fold)")
          
          ggplotly(p) %>% 
            layout(autosize=TRUE)
          #layout(height = input$plotHeight, autosize=TRUE)
          
          #   geom_line(arl,aes(x=tl,y=arl))
          # ggplot(res2, aes(x = res2_y, y = value, fill = variable)) + geom_line(aes(color = variable))
        })
        
        # output$distr_mse_fold_plot <- renderPlotly({
        #   if(!is.null(tp)){
        #     dif <- fold_df %>%dplyr::filter(model==modl[1])-
        #       fold_df %>%dplyr::filter(model==modl[2])
        #     
        #     dif_df <- fold_df %>% dplyr::filter(model==modl[1]) %>% 
        #       dplyr::select(-starts_with('model'))
        #     dif_df$mse <- dif$mse
        #     
        #     
        #     sel_dat <- fold_df %>% dplyr::filter(tl %in% c(current_selected_tp_distr_mse_fold_plot()))
        #     sel_dat$fold <- as.factor(sel_dat$fold)
        #     colnames(sel_dat) <- c('model','tl','fold','mse')
        #     p<- ggplot(sel_dat,aes(x=tl,y=mse)) + 
        #       geom_point(aes(shape=as.factor(fold),colour=model),size=.5) + 
        #       geom_point(data=dif_df,aes(shape=as.factor(fold)),colour='black',size=1) +
        #       #geom_point(data=sel_dat,aes(x=tl,y=mean(mse),colour=model),size=1.4) + 
        #       #geom_linerange(data=pldf,position='dodge',aes(ymin=0,ymax=max_mse/10*count)) +
        #       #geom_text(data=subset(pldf,count<=1),position='dodge',aes(label=count,vjust=max_mse/10*count)) +
        #       scale_colour_manual(values = c("Blue", "Red")) +
        #       theme_classic() 
        #     
        #     ggplotly(p) %>% 
        #       layout(autosize=TRUE)
        #   }
        #   
        #   
        # })
        
        output$tp_3d_plot <- renderPlot({
          tbl_tmp <- table(pldf$tl)
          pldf_binned <- pldf %>% dplyr::arrange(tl)
          # pldf_binned <- cbind(pldf_binned[pldf_binned$model=='var',]$tl,
          # pldf_binned[pldf_binned$model=='var',]$mse - pldf_binned[pldf_binned$model=='ar',]$mse)
          
          pldf_binned <- cbind(pldf_binned[pldf_binned$model==input$tp_model2,]$tl,
                               pldf_binned[pldf_binned$model==input$tp_model2,]$mse - pldf_binned[pldf_binned$model==input$tp_model1,]$mse)
          
          pldf_binned <- data.frame(pldf_binned)
          colnames(pldf_binned)<-c('tl','mse')
          
          tmp <- NULL
          #bin some of the observations together
          #if #obs lower than 10, store in a string the tp
          #if #obs higher than 10, than we will do one of two things
          #if tmp contains more than one observation (from previous #obs lower than 10)
          #we add this string in a tl-tl format to the table
          #else, if tmp == 1, we ignore it and go to next
          #at the end of this we always reset tmp to NULL (in the #obs higher than 10 part)
          flag <- FALSE
          for (i in 1:nrow(tbl_tmp)){
            if(tbl_tmp[i] < 10){
              tmp<-rbind(tmp,as.integer(names(tbl_tmp)[i]))
            } else if (!is.null(tmp)) {
              if(length(tmp)>1){
                tmp_str <- paste0(min(tmp),'-',max(tmp))
              } else {
                tmp_str <- paste0(tmp)
              }
              pldf_binned[pldf_binned$tl %in% tmp,]$tl <- tmp_str
              tmp <- NULL
              flag <- TRUE
            }#else { 
            #   tmp<-rbind(tmp,as.integer(names(tbl_tmp)[i]))
            #   tmp_str <- paste0(tmp)
            #   pldf_binned[pldf_binned$tl %in% tmp,]$tl <- tmp_str
            #   tmp <- NULL
            # }
            
          } 
          
          if(!flag && nrow(tbl_tmp)>1){
            tmp_str <- paste0(min(tmp),'-',max(tmp))
            pldf_binned[pldf_binned$tl %in% tmp,]$tl <- tmp_str
          } else if (!flag && nrow(tbl_tmp)==1){
            pldf_binned[pldf_binned$tl %in% tmp,]$tl <- tmp
          }
          
          tmp <- table(pldf_binned$tl)
          tmp<-data.frame(cbind(names(tmp),sapply(data.frame(tmp)$Freq,as.character)))
          colnames(tmp)<-c('tl','freq')
          tmp$freq <- tmp$freq %>% as.character %>% as.numeric
          
          
          if(length(tmp %>% filter(freq>10))>1){
            tmp <- tmp %>% filter(freq>10)
            pldf_binned <- pldf_binned %>% dplyr::filter(tl %in% tmp$tl)
            p<- ggplot(pldf_binned,aes(x=mse,y=as.factor(tl),fill=..x..)) +
              geom_density_ridges_gradient(
                alpha=.5,
                jittered_points=TRUE,
                point_shape = "|",
                point_size=3,
                position = position_points_jitter(height=0),
                quantile_lines=FALSE,
                scale=1.2) +
              scale_y_discrete(name="Density") +
              scale_x_continuous(name=toupper(error_metric))+
              guides(fill=guide_legend())+
              scale_fill_gradientn(name = "Model2 - Model1",colors=c('Red','Blue'))+
              geom_text(aes(label=..count..), y=0, stat='count', colour="black", size=4) +
              
              theme_ridges(center=TRUE,grid=TRUE)
            
            plot(p)
          } else {
            p<- ggplot(pldf_binned,aes(x=mse,y=as.factor(tl),fill=..x..)) +
              geom_density_ridges_gradient(
                alpha=.5,
                jittered_points=TRUE,
                point_shape = "|",
                point_size=3,
                position = position_points_jitter(height=0),
                quantile_lines=FALSE,
                scale=1.2) +
              scale_y_discrete(name="Density") +
              scale_x_continuous(name=toupper(error_metric))+
              guides(fill=guide_legend())+
              scale_fill_gradientn(name = "Model2 - Model1",colors=c('Red','Blue'))+
              ggtitle("WARNING: Density distribution estimates are unreliable due to small sample sizes in each time point.")+
              theme_ridges(center=TRUE,grid=TRUE)
            
            plot(p)
            
          }
        })
        
        output$distr_mse_fold_plot2 <- renderPlotly({
          if(!is.null(tp)){
            sel_dat <- pldf %>% dplyr::filter(tl %in% c(current_selected_tp_distr_mse_fold_plot2()))
            colnames(sel_dat) <- c('tl','model','mse')
            
            p<- ggplot(sel_dat,aes(x=mse)) +
              geom_density(data=sel_dat,aes(colour=model),size=1) +
              scale_colour_manual(values = c("Blue", "Red")) +
              scale_x_continuous(name=toupper(error_metric)) +
              theme_classic() 
            
            ggplotly(p) %>% 
              layout(autosize=TRUE)
          }
        })
      }
      ##TP SEARCH FAILED
    } else {
      showNotification("TP estimation failed due to an undefined error.",
                       type="error")
    }
  })
  
  current_selected_tp_distr_mse_fold_plot2 <- reactive({
    input$select_distr_mse_fold_plot2_tp
  })
  
  #if initialized, use input value, otherwise use 20
  time_searchtp <- reactive({
    if(!is.null(input$nTime_tp)){
      input$nTime_tp
    } else {
      20
    }
  })
  
  ####Model comparison-------
  
  output$mc_config_ui<-renderUI({
    if(!is.null(r$data) && !is.null(input_df$df)){
      data_list <- c('Simulated dataset', 'Active dataset') 
    } else if(!is.null(r$data) && is.null(input_df$df)){
      data_list <- c('Simulated dataset') 
    } else if(is.null(r$data) && !is.null(input_df$df)){
      data_list <- c('Active dataset') 
    } else {
      data_list <- NULL
    }
    tagList(
      boxPlus(
        selectInput('mc_select_data',
                    'Choose dataset',
                    choices=data_list
        ),
        selectInput(inputId='mc_model1',
                    label='Choose comparison model 1',
                    choices=model_list,
                    selected='ar'
        ),
        selectInput(inputId='mc_model2',
                    label='Choose comparison model 2',
                    choices=model_list,
                    selected='var'
        ),
        actionButton("submitModelComparison", "Submit")
      )
    )
  })
  
  analysis_ready_flag <- reactive({
    if(is.null(r$data)&&is.null(input_df$df)){
      return(FALSE)
    } else {
      return(TRUE)
    }
  })
  
  mc_data <- reactive({
    mc_data <- NULL
    if(input$mc_select_data == 'Simulated dataset'){
      mc_data<-r$data
    } else {
      mc_data<-filedata_updated()
    }
    mc_data
  })
  
  observeEvent({input$submitModelComparison},{
    
    if(!is.null(r$data)|!is.null(input_df$df)){
      
      mod1<-do.call(modelData,list(input$mc_model1,
                                   mc_data(),
                                   selectedLagNum(),
                                   loaded_dataset_index_variable(),
                                   modelDataParams(input$mc_model1)
      )
      )
      mod2<-do.call(modelData,list(input$mc_model2,
                                   mc_data(),
                                   selectedLagNum(),
                                   loaded_dataset_index_variable(),
                                   modelDataParams(input$mc_model2)
      )
      )
      
      mod1_cv<-computeCV(mc_data(),
                         model = input$mc_model1,
                         K=3,
                         loaded_dataset_index_variable(),
                         lagNum = 1,
                         error_metric = 'mse',
                         modelDataParams(input$mc_model1)
      )[[1]] %>% na.omit() %>% mean()
      
      mod2_cv<-computeCV(mc_data(),
                         model = input$mc_model2,
                         K=3,
                         loaded_dataset_index_variable(),
                         lagNum = 1,
                         error_metric = 'mse',
                         modelDataParams(input$mc_model2)
      )[[1]] %>% na.omit() %>% mean()
      
      
      
      output$mc_outcome_ui <- renderUI({
        tagList(
          valueBox(
            ifelse(
              mod1_cv > mod2_cv,
              input$mc_model2,
              input$mc_model1
            ),
            paste0('Best model based on APE by blocked cross-validation (',input$mc_model1,': ',round(mod1_cv,2),'; ',input$mc_model2,': ',round(mod2_cv,2),')'),
            icon=icon("th")
          )
        )
      })
    }
  })
  
  # valueBox(best,"Best model based on true parameter accuracy.",icon=icon("eye"))
  
  
  
  output$mseplot <- renderPlotly({
    if (is.null(r$nVar) | is.null(r$nModel2)) {
      return()
    }
    res2 <- cv$compute[[2]]
    p <-
      ggplot(res2, aes(x = res2_y, y = value, fill = variable)) + 
      geom_line(aes(color = variable)) +
      theme_classic()
    ggplotly(p)
  })
  
  
  observeEvent({input$submit2},{
    output$mse <- renderValueBox({
      if (!(is.null(r$nVar) | is.null(r$nModel2))){
        valueBox(cv$compute[[1]],
                 "Average MSE", 
                 icon = icon("superscript"),
                 color = "light-blue")
      }
    })
    
    output$paramacc <- renderValueBox({
      if (!(is.null(r$nVar) | is.null(r$nModel2))){
        valueBox(ifelse(r$nModel2=='ar',
                        acc$comparison[[1]],
                        acc$comparison[[2]]
        ),"Average squared parameter error",
        icon = icon("superscript"),
        color="light-blue"
        )
      }
    })
    
    output$accuracy <- renderUI({
      if(!(is.null(r$nVar) | is.null(r$nModel2))){
        est <- modelData(r$nModel2,r$data,selectedLagNum(),loaded_dataset_index_variable())
        comp <- computeAccuracy(extractPhi(est), current_phi_input())
        M <- comp
        M <- print(
          xtable(M, align = rep("c", ncol(M) + 1), digits = 6),
          floating = FALSE,
          tabular.environment = "array",
          comment = FALSE,
          print.results = FALSE
        )
        html <- paste0("$$", M, "$$")
        withMathJax(HTML(html))
      }
    })
  })
  
  #####network analysis-----
  
  selected_network_vars <- reactive({
    input$select_network_vars
  })
  
  output$select_network_vars_element <- renderUI({
    selectInput("select_network_vars","Select variables for network analysis",
                multiple=TRUE,
                choices = c('None',names(input_df$df))
    )
  })
  
  output$select_network_graph_type <- renderUI({
    selectInput("select_graph_type","Select graph type",
                multiple=FALSE,
                choices = c('glasso','cor'),
                selected='glasso'
    )
  })
  
  
  output$select_network_treshold <- renderUI({
    selectInput("select_treshold","Select treshold",
                choices=c('none',
                          'sig',
                          'holm',
                          'hochberg',
                          'hommel',
                          'bonferroni',
                          'BH',
                          'BY',
                          'fdr')
    )
  })
  
  output$select_network_tuning <- renderUI({
    conditionalPanel(
      condition="input.select_graph_type == 'glasso'",
      numericInput("select_tuning","Select tuning",
                   min=0,
                   max=1,
                   step=.01,
                   value=.1
      )
    )
  })
  
  
  output$networkplot <- renderPlot({
    if(!is.null(selected_network_vars())){
      selected_cols <- selected_network_vars()
      d <- filedata_updated() %>% dplyr::select(selected_cols)
      Q <- qgraph(cor_auto(d,detectOrdinal = FALSE),
                  graph = input$select_graph_type, 
                  treshold=input$select_treshold,
                  tuning=input$select_tuning,
                  sampleSize = nrow(d),
                  nodeNames = names(d),
                  label.scale = FALSE, label.cex = .8, 
                  legend = TRUE, legend.cex = .5,
                  layout = "spring")
      Q
    } else {
      return(NULL)
    }
    
  })
}
