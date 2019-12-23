#' @import shiny
#' @import ggplot2
#' @import xtable
#' @import ggridges
#' @import shinydashboardPlus
app_server <- function(input, output, session) {
  
  models <- dir('models')
  models <- paste0('models/', models)
  lapply(models, source)
  model_list <<- unlist(strsplit(model_list,'.R'))
  
  
  observeEvent(input$browser,{
    browser()
  })
  
  #------------------------------ Dataset Init -----------------------
  
  data(alt_data95)
  data(sim_var)
  
  df_list <- c(names(which(sapply(.GlobalEnv, is.data.frame))),
               names(which(sapply(.GlobalEnv, is.matrix)))
  )
  ts_list <- c(names(which(sapply(.GlobalEnv, is.ts))))  
  
  #------------------------------ First User Introduction -----------------------
  
  #Use this for the shinyintrojs - currently not implemented
  observeEvent(input$help,{
    introjs(session,
    )
  })
  
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
  
  active_df <- reactiveValues(df = NULL,
                              class = NULL,
                              var_summary = NULL,
                              names_list = NULL)
  
  #------------------------------ SideBar Menu -----------------------
  output$data_select_top <- renderUI({
    selectizeInput(
      'select_df', label = "Select active dataset", choices = input_df$names_list
      #options = list(create = TRUE)
    )
  })
  
  output$menu <- renderMenu({
    sidebarMenu(
    id="tabs",
    uiOutput('data_select_top'),
    menuItem("Data Management", tabName = "data", icon = icon("table"), startExpanded = TRUE,
             menuSubItem("Load dataset", tabName = "data1"),
             menuSubItem("Pre-processing", tabName = "data2"),
             menuSubItem("Visualization",tabName="vis"),
             conditionalPanel(
               condition="input.tabs == 'vis'",
               uiOutput("loaded_ds_list_vis"),
               uiOutput("vis_plot_type"),
               conditionalPanel(condition =  "output.loaded_table_flag == '1' && output.class_df_flag_vis == false && input.plot_type != 'cor' ",
                                conditionalPanel(condition = "input.plot_type != 'density' && input.plot_type != 'box' && input.plot_type != 'hist'",
                                                 uiOutput("vis_x"),
                                                 uiOutput("vis_y")
                                ),
                                conditionalPanel(condition = "input.plot_type == 'density' || input.plot_type == 'hist' || input.plot_type == 'box'",
                                                 uiOutput("vis_one_var")
                                )
               ),
               conditionalPanel(condition = "input.plot_type == 'scatter' || input.plot_type == 'box'|| input.plot_type == 'hist' || input.plot_type == 'density'",
                                box(width = 2,
                                    uiOutput("vis_factor")),
                                uiOutput("sim_data_vis")
               )
             )
    ),
    menuItem("Data Simulation", icon = icon("database"),tabName = "sim"),
    menuItem("Analysis", icon = icon("microscope"), tabName = "analysis",
             menuSubItem("Model Comparison", tabName = "modelcomparison"),
             menuSubItem("Timepoint Estimation", tabName="tpestimation")
    ),
    menuItem("Network Analysis",icon=icon("project-diagram"),tabName="networkanalysis")
    )
  })
  
  #-----------------------------Notification panel---------------
  output$info_top <- renderUI({
    
    dropdownMenu(type="notifications",
                 notificationItem(
                   text = ifelse(is.null(input$select_df),
                                 'No dataset loaded',
                                 paste0('Dataset loaded: ',input$select_df)
                   ),
                   icon("th")
                 ),
                 notificationItem(
                   text = ifelse(is.null(loaded_dataset_id_value()) | loaded_dataset_id_value() == 'None',
                                 'No subject ID selected',
                                 paste0('Subject ID selected: ', loaded_dataset_id_value())
                   ),
                   icon("person")
                 )
    )
  })
  
  #------------------------------ Data Tab 1 - DT1 ---------------------------------------------###############################
  
  #------------------------------ DT1 summary boxes -------------------------------------
  
  output$in_memory_df <- renderValueBox({
    valueBox(
      length(prev_table$data_frame_list), "In Memory Data Frame", icon = icon("superscript"),
      color = "light-blue"
    )
  })
  
  output$load_datasets <- renderValueBox({
    valueBox(
      ifelse(is.null(input_df$df_list), 0, length(input_df$df_list)), "Loaded Datasets", icon = icon("list"),
      color = "maroon"
    )
  })
  
  #------------------------------ DT1 Selecting the Data Input ------------------------------------- 
  #prev_table is short for "preview table"
  
  prev_table <- reactiveValues(inputs_list = NULL, # Get the list of available dataset to load
                               data_frame_list = df_list, # List of available dataframes in memory
                               time_series_list = ts_list, # List of available time series in memory
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
              HTML(paste("There is no any data frame available",
                         "to load in the R Global Environment", 
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
      },
      
      "time_series" = {
        # Case II - load in memory time series
        # If there is no any ts available in memory
        if(length(prev_table$time_series_list) == 0){
          showModal(
            modalDialog(
              title = "Warning - No Time Series Data",
              HTML(paste("There is no time series data available",
                         "to load in the R Global Environment", 
                         sep = "<br/>")
              ), 
              size = "s"
            ))
          
          df_return_list <- NA
          # Set the condition for the load button
          output$load_flag <- reactive('0')
          outputOptions(output, "load_flag", suspendWhenHidden = FALSE)
        } else { # Otherwise return the list of available time series in memory
          df_return_list <- prev_table$time_series_list
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
    } else if(input$data_source == "time_series" ) {
      selectInput("df_to_load", "Select Series",
                  choices = prev_table$inputs_list )
    } else if(input$data_source == "inst_pack" ){
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
      df_view <- get0(input$df_to_load)
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
    } else if(input$data_source == "time_series" & length(prev_table$time_series_list) != 0){
      df_view <- NULL
      prev_table$df_name <- input$df_to_load
      input_df$ts_obj <- get(input$df_to_load)
      df_view <- get(input$df_to_load)
      if(is.mts(df_view)){
        df_view <- data.frame(date=time(df_view), as.matrix(df_view))
      } else if(is.ts(df_view)){
        df_view <- data.frame(date=time(df_view), as.matrix(df_view))
        names(df_view) <- c("date", prev_table$df_name)
      }
      if(length(class(input_df$ts_obj)) > 1 & "ts" %in% class(input_df$ts_obj)){
        prev_table$class <- "ts"
      } else if(length(class(input_df$ts_obj)) > 1){
        prev_table$class <- class(input_df$ts_obj)[1]
      } else{
        prev_table$class <- class(input_df$ts_obj)
      }
      # Loading from installed package
    } else if(input$data_source == "import" & !is.null(prev_table$file_path)){
      df_view <- NULL
      prev_table$class <- NULL
      prev_table$df_name <- substr(prev_table$file_name,1,regexpr(".", prev_table$file_name, fixed = T)-1)
      df_view <- read.csv(prev_table$file_path, stringsAsFactors = FALSE)
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
        if(prev_table$class != "ts"){
          input_df$df_list <- list(df_tbl_view())
        } else {
          input_df$df_list <- list(input_df$ts_obj)
        }
        input_df$df_class <- list(type)
        
      } else {
        if(prev_table$class != "ts"){
          input_df$df_list[[length(input_df$df_list) + 1]] <- df_tbl_view()
        } else {
          input_df$df_list[[length(input_df$df_list) + 1]] <- input_df$ts_obj
        }
        input_df$df_class[[length(input_df$df_list)]] <- type
      }
      names(input_df$df_list)[length(input_df$df_list)] <- name
      input_df$names_list <- names(input_df$df_list)
    } else{
      if(prev_table$class != "ts"){
        input_df$df_list[[which(names(input_df$df_list) == name)]] <- df_tbl_view()
      } else {
        input_df$df_list[[which(names(input_df$df_list) == name)]] <- input_df$ts_obj
      }
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
  
  #------------------------------ DT2 Loaded dataset table ------------------------------------- 
  output$list_loaded_df <- DT::renderDataTable(
    data.frame(input_df$loaded_table), 
    colnames = c("Dataset Name", "Num. of Variables", "Num. of Obs", "Data Type"),
    selection = list(selected = 1, mode = 'single'), 
    options = list(pageLength = 10,
                   lengthMenu = c(10, 25, 50))
  )
  # 
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
      if(id_var()!='None' && !is.null(input_df$df)){
        ncol(input_df$df)-1
      } else if (!is.null(input_df$df)){
        ncol(input_df$df)
      } else {
        3
      },
      min = 1,
      max = 150
    )
  })
  
  output$num_tp_sim <- renderUI({
    numericInput(
      "nTime",
      "Number of timepoints:",
      if(is.null(input_df$df)){
        15
      } else if(loaded_dataset_id_value() != 'None'){
        nrow(input_df$df %>% dplyr::filter_at(id_var_number(), all_vars(.==loaded_dataset_id_value())))
      } else {
        nrow(input_df$df)
      },
      min = 20,
      max = 10000
    )
  })
  
  output$simulation_parameter_origin <- renderUI({
    selectInput(
      "select_simulation_parameter_origin",
      "Select parameter estimate source",
      choices=c('Manual',
                'Active dataset'),
      selected=ifelse(!is.null(input_df$data_name),input_df$data_name,'Manual'),
      multiple = FALSE
    )
  })
  
  observeEvent(input$select_df, {
    if(!is.null(input$select_df)){
      input_df$df <- (
        input_df$df_list[[which(names(input_df$df_list) == input$select_df)]]
      )
      input_df$class <- input_df$df_class[[which(names(input_df$df_list) == input$select_df)]]
      uiOutput('data_tab2_table')
      
      input_df$df <- (
        input_df$df_list[[which(names(input_df$df_list) == input$select_df)]]
      )
      active_df$class <- input_df$df_class[[which(names(input_df$df_list) == input$select_df)]]
      
    } else{
      input_df$df <- NULL
      input_df$class <- NULL
      
      input_df$df <- NULL
      active_df$class <- NULL
      
      output$data_tab2_table <- NULL
    }
  })
  
  
  # Need to change the way that data tables are updated. We need a single loaded dataset variable, which is changeable from all tabs, and changes all the selectinputs.
  # observeEvent(input$select_df_vis, {
  #   if(!is.null(input$select_df_vis)){
  #     input_df$df <- (
  #       input_df$df_list[[which(names(input_df$df_list) == input$select_df_vis)]]
  #     )
  #     input_df$class <- input_df$df_class[[which(names(input_df$df_list) == input$select_df_vis)]]
  #     
  #     
  #   } else{
  #     input_df$df <- NULL
  #     input_df$class <- NULL
  #     output$data_tab2_table <- NULL
  #   }
  # })
  # 
  # input$select_simulation_parameter_origin
  
  #------------------------------ Data tab 2 - Data Prep -------------------------------------    
  #------------------------------ Data tab 2 - Creating Variables Table ------------------------------------- 
  
  #reset, else app will crash when switching from one dataset to another when the selected column is larger than the available columns
  #right now, we have a workaround by only letting the var summary table change when the row is changed. ideally this would also reset when changing datasets.
  proxy_data_tab2_var = dataTableProxy('data_tab2_var')
  observeEvent({
    input$data_option
    input$select_df
    }, {
      proxy_data_tab2_var %>% selectRows(NULL)
      proxy_data_tab2_var %>% selectColumns(NULL)
    },
    priority = 100)
  
  c_input_df_var_summary <-reactive ({
    input_df$df
  })
  
  
  observeEvent({input$data_option
    input_df$df
    input$select_df
  }, {
    if(!is.ts(input_df$df)){            
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
    } else {
      output$data_tab2_ts <- renderPlotly({
        
        
        if(!input$ts_plot_log){
          plot_ly( x = time(input_df$df), y = input_df$df, type  = "scatter", mode = input$ts_prep_mode)
        } else if(input$ts_plot_log){
          plot_ly( x = time(input_df$df), 
                   y = log(input_df$df, base = exp(1)), type  = "scatter", mode = input$ts_prep_mode) %>%
            layout(title = "Log Transformation")
        }
        
      })
    }
  })
  
  output$class_df_flag <- reactive({
    ifelse(is.ts(input_df$df), TRUE, FALSE)
  })
  outputOptions(output, "class_df_flag", suspendWhenHidden = FALSE)    
  #------------------------------ Data tab 2 - Creating Variable Summary -------------------------------------  
  
  observeEvent({input$data_tab2_var_rows_selected},
               priority = -100,{
                 if(!(is.ts(input_df$df)|is.ts(input_df$df))){
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
                 } else {
                   ts_table <- data.frame(c(paste(start(input_df$df), collapse = "-"),
                                            paste(end(input_df$df), collapse = "-"),
                                            min(input_df$df, na.rm = TRUE),
                                            max(input_df$df, na.rm = TRUE),
                                            round(sd(input_df$df, na.rm = TRUE),2)),
                                          row.names = c("Start Date",
                                                        "End Date", "Min Value",
                                                        "Max Value","Standard Deviation"))
                   names(ts_table) <- input$select_df
                   output$ts_table <- renderTable(ts_table, rownames = TRUE)
                   
                 }
                 
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
        HTML(paste("There is no any loaded dataset ",
                   "Please select input and load it", 
                   sep = "<br/>")
        ), size = "s"
      ))
    }
  })
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
      if(!is.ts(vis_df$df)){
        for(i in 1:ncol(vis_df$df)){
          if(is.factor(vis_df$df[,i])){
            vis_df$var_factor <- c(vis_df$var_factor, names(vis_df$df)[i])
          } else if(is.numeric(vis_df$df[,i]) | is.integer(vis_df$df[,i])){
            vis_df$var_numeric <- c(vis_df$var_numeric,names(vis_df$df)[i])
          }
        }
      }
    } else{
      
      vis_df$df <- NULL
      vis_df$class <- NULL
      vis_df$var_factor <- NULL
      vis_df$var_numeric <- NULL
      
      
      
    }
  })
  
  
  observeEvent({input$var_modify
    input$select_df_vis},{
      if(!is.null(vis_df$var_numeric) & !is.ts(vis_df$df)){
        ###################### NEED TO ADD CASE FOR ONLY ONE VARIABE !!!!!!
        if(length(vis_df$var_numeric) == 1 ){
          output$vis_plot_type <- renderUI({
            selectInput("plot_type", "Select the Plot Type",
                        choices = list("Boxplot" = "box",
                                       "Histogram" = "hist",
                                       "Density" = "density"))
          })
          output$vis_one_var <- renderUI({
            selectInput("plot_var", "Select a Variable",
                        choices = vis_df$var_numeric,
                        selected = vis_df$var_numeric[1]
            )
          })
          
          output$vis_factor <- renderUI({
            if(!is.null(vis_df$var_factor)){
              selectInput(
                "plot_factor", "Add Grouping Variable",
                choices = c("None",  c(as.character(vis_df$var_factor),vis_df$var_numeric))
              )
            } else {
              selectInput(
                "plot_factor", "Add Categorical Variable",
                choices = c("None",vis_df$var_numeric),
                selected = "None"
              )
            }
          })
          
        } else if(length(vis_df$var_numeric) > 1 ){
          output$vis_plot_type <- renderUI({
            selectInput("plot_type", "Select the Plot Type",
                        choices = list("Scatter" = "scatter",
                                       "Line" = "line",
                                       "Boxplot" = "box",
                                       "Histogram" = "hist",
                                       "Density" = "density",
                                       "Correlation" = "cor"))
          })
          
          output$vis_one_var <- renderUI({
            selectInput("plot_var", "Select a Variable",
                        choices = vis_df$var_numeric,
                        selected = vis_df$var_numeric[1]
            )
          })
          
          output$vis_x <- renderUI({
            selectInput("plot_x", "Select the X Axis",
                        choices = vis_df$var_numeric,
                        selected = vis_df$var_numeric[1]
            )
          })
          
          output$vis_y <- renderUI({
            selectInput(
              "plot_y", "Select the Y Axis",
              choices = vis_df$var_numeric,
              selected = vis_df$var_numeric[2]
            )
          })
          
          output$vis_factor <- renderUI({
            if(!is.null(vis_df$var_factor)){
              selectInput(
                "plot_factor", "Add Grouping Variable",
                choices = c("None",  c(as.character(vis_df$var_factor),vis_df$var_numeric))
              )
            } else {
              selectInput(
                "plot_factor", "Add Categorical Variable",
                choices = c("None",vis_df$var_numeric),
                selected = "None"
              )
            }
          })
        }
        
      } else if(is.null(vis_df$var_numeric) & !is.ts(vis_df$df)){
        output$vis_x  <- renderUI({
          selectInput("plot_x", "Select Variables",
                      choices = "No Available Numeric Variables"
          )
        })
      } else if(is.ts(vis_df$df)){
        output$vis_plot_type <- renderUI({
          selectInput("plot_type", "Select the Plot Type",
                      choices = list("Scatter" = "scatter",
                                     "Line" = "line",
                                     "Boxplot" = "box",
                                     "Seasonal Plot" = "seasonal_plot",
                                     "Lags Plot" = "lags_plot"))
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
      } else if(is.ts(vis_df$df)){
        ts.df <- data.frame(dec_left = floor(time(vis_df$df)),
                            dec_right = round((time(vis_df$df) - floor(time(vis_df$df))) * 
                                                frequency(vis_df$df) + 1), 
                            value = as.numeric(vis_df$df))
        p <- switch(input$plot_type,
                    "line" = {
                      plot_ly( x = time(vis_df$df), y = vis_df$df, type  = "scatter", mode = "line")
                    },
                    "scatter" = {
                      plot_ly( x = time(vis_df$df), y = vis_df$df, type  = "scatter")
                    },
                    "box" = {
                      plot_ly(data = ts.df, y = ~ value ,
                              color = ~ as.factor(dec_right), 
                              type = "box", 
                              boxpoints = "all", jitter = 0,
                              pointpos = -1.8)
                      
                    },
                    
                    "seasonal_plot" = {
                      if(frequency(vis_df$df) == 1){
                        p <- plot_ly()
                        showModal(modalDialog(
                          title = "Warning - Seasonal Plot is Not Available",
                          HTML(paste("Seasonal plot is not available",
                                     "for time series object with yearly frequancy", 
                                     sep = "<br/>")
                          ), size = "s"
                        ))
                        p
                      } else {
                        ts.df_wide <- reshape2::dcast(ts.df, dec_right ~ dec_left )
                        p <- plot_ly()
                        
                        for(f in 2:ncol(ts.df_wide)){
                          p <- p %>% add_trace(x = ts.df_wide[,1], y = ts.df_wide[,f],
                                               name = paste("time", names(ts.df_wide)[f], sep = " " ),
                                               mode = "line")
                        }
                        p
                      }
                    },
                    "lags_plot" = {
                      lag <- NULL
                      lag_plots <- NULL 
                      max.lags <- 12
                      for(g in 1:max.lags){
                        if(g == 1){
                          lag <- c(NA, ts.df$value[- nrow(ts.df)]) 
                        } else {
                          lag <- c(NA,lag[-nrow(ts.df)])
                        }
                        lag_plots[[g]] <- plot_ly(x = lag, y = ts.df$value, 
                                                  name = paste("Lag", g, sep = " ")) %>%
                          layout(xaxis = list(title = paste("Lag", g, sep = " "),
                                              range = c( min(na.omit(as.numeric(lag))),  
                                                         max(na.omit(as.numeric(lag))))),
                                 yaxis = list(title = paste("Series", sep = ""),
                                              range = c( min(na.omit(as.numeric(ts.df$value))),  
                                                         max(na.omit(as.numeric(ts.df$value))))),
                                 title = paste(input$select_df_vis,"Series vs Lags", sep = " "),
                                 annotations = list(
                                   # x = median(na.omit(as.numeric(lag))),
                                   # y = median(na.omit(as.numeric(ts.df$value))),
                                   showarrow = FALSE,
                                   # arrowhead = 4,
                                   # arrowsize = 0.5,
                                   # ax = 20,
                                   # ay = -20,
                                   xref = paste("x", g, sep = ""),
                                   yref = paste("y", g, sep = ""),
                                   text = paste("Lag", g, sep = " "))
                          )
                      }
                      
                      subplot(lag_plots, 
                              titleX = FALSE, titleY = TRUE,
                              shareX = FALSE, shareY = FALSE, 
                              margin = 0.05,
                              nrows = ceiling(length(lag_plots) / 3))%>% 
                        hide_legend()
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
  
  estParams <- reactive({
    #params init
    model<-selectedSimMod()
    dataset<-filedata_updated()
    lagNum<-selectedLagNum()
    index_vars<-loaded_dataset_index_variable()
    
    #logic
    dataset <- na.omit(dataset)
    tmod <- modelData(model,dataset,lagNum,index_vars)
    colnames <- c('nvar','num','measerror')
    nvar <- ncol(dataset)
    
    resid<-extractResiduals(tmod)
    phi<-extractPhi(tmod)
    inno<-extractInno(tmod)
    
    colnames(phi) <- names(dataset)
    rownames(phi) <- names(dataset)
    colnames(inno) <- names(dataset)
    rownames(inno) <- names(dataset)
    params<-list(resid,phi,inno)
  })
  
  filedata_updated <- reactive ({
    if(!is.null(input_df$df)){
      if(id_var()!='None' && loaded_dataset_id_value() != 'None'){
        input_df$df %>% 
          dplyr::filter_at(id_var_number(), all_vars(. == as.integer(loaded_dataset_id_value()))) %>% 
          dplyr::select(-starts_with(id_var()))
      } else if (id_var()!='None' && loaded_dataset_id_value() == 'None'){
        input_df$df %>% 
          dplyr::select(-starts_with(id_var()))
      } else {
        input_df$df
      }
    }
  })
  
  
  
  id_var <- reactive({
    input$select_dataset_id_var
  })
  
  id_var_number <- reactive({
    which( colnames(input_df$df)==id_var() )
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
    input$current_dataset_id_value
  })
  
  
  
  #original df of currently loaded dataset
  filedata <- reactive({
    input_df$df
  })
  
  #Simulation Tab Phi Matrix and Innovation Matrix
  output$inno <- renderRHandsontable({
    if(!is.null(updated_inno())){
      rhandsontable(updated_inno())
    }
  })
  
  output$phi <- renderRHandsontable({
    if(!is.null(updated_phi())){
      rhandsontable(updated_phi())
    }
  })
  
  data_simulation_parameter_origin <- reactive ({
    if(!is.null(input$select_simulation_parameter_origin)){
      input$select_simulation_parameter_origin
    } else {
      NULL
    }
  }) 
  updated_inno <- reactive({
    if(!is.null(input_df$df) && (data_simulation_parameter_origin() != 'Manual')){
      inno_output <- estParams()[[3]]
    } else if(data_simulation_parameter_origin() == 'Manual'){
      inno_output<-computeSigma(r$nVar, r$innoVar,r$innoCovar)
      colnames(inno_output) <- c(paste("V",1:ncol(inno_output),sep="")) 
      rownames(inno_output) <- c(paste("V",1:nrow(inno_output),sep="")) 
    } else {
      inno_output <- NULL
    }
    inno_output
  })
  
  updated_phi <- reactive({
    if(!is.null(input_df$df) && data_simulation_parameter_origin() != 'Manual'){
      phi_output <- estParams()[[2]]
    } else if(data_simulation_parameter_origin() == 'Manual'){
      phi_output<-computePhi(r$nVar, r$diagPhi, r$offdiagPhi)
      colnames(phi_output) <- c(paste("V",1:ncol(phi_output),sep="")) 
      rownames(phi_output) <- c(paste("V",1:nrow(phi_output),sep="")) 
    } else {
      phi_output <- NULL
    }
    phi_output
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
  
  # selected_ntime <- reactive({
  #   input$nTime
  # })
  # 
  # selected_nerror <- reactive({
  #   input$nError
  # })
  # 
  # selected_ndiagphi <- reactive({
  #   input$nDiagPhi
  # })
  # 
  # selected_ninnovar <- reactive ({
  #   input$nInnoVar
  # })
  # 
  # selected_ninnocovar <- reactive({
  #   input$nInnoCovar
  # })
  # 
  # selected_noffdiagphi <- reactive({
  #   ifelse(selected_nmodel1() == 'var',input$nOffdiagPhi,0)
  # })
  # 
  # selected_nmodel1 <- reactive({
  #   input$nModel1
  # })
  # 
  # selected_nmodel2 <- reactive({
  #   input$nModel2
  # })
  # 
  # r <- list(
  #   data = NULL,
  #   nVar = selected_nvar(),
  #   nTime = selected_ntime(),
  #   error = selected_nerror(),
  #   diagPhi = selected_ndiagphi(),
  #   innoVar = selected_ninnovar(),
  #   innoCovar = selected_ninnocovar(),
  #   offdiagPhi = ifelse(selected_nmodel1() == 'var',selected_noffdiagphi(),0),
  #   nModel1 = selected_nmodel1(),
  #   nModel2 = selected_nmodel2()
  # )
  
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
  
  observeEvent({input$nDiagPhi
    input$nInnoCovar
    input$nOffdiagPhi
    input$nVar
    input$nInnoVar
    input$nTime
    input$selection1
  },{
    r$nVar <<- input$nVar
    r$nTime <<- input$nTime
    r$error <<- input$nError
    r$diagPhi <<- input$nDiagPhi
    r$innoVar <<- input$nInnoVar
    r$innoCovar <<- input$nInnoCovar
    r$nModel1 <<- input$selection1
    if (r$nModel1 == 'var') {
      r$offdiagPhi <<- input$nOffdiagPhi
    } else {
      r$offdiagPhi <<- 0
    }
  })
  
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
  
  
  
  
  
  current_phi_input <- reactive({
    if(!is.null(input_df$df) && input$select_simulation_parameter_origin != 'Manual'){
      dphi <- hot_to_r(input$phi)
      #dphi[lower.tri(dphi)] <- dphi[upper.tri(dphi)]
    } else if (input$select_simulation_parameter_origin == 'Manual'){
      dphi <- hot_to_r(input$phi)
    }
  })
  
  current_inno_input <- reactive({
    if(!is.null(input_df$df) && input$select_simulation_parameter_origin != 'Manual'){
      dinno <- hot_to_r(input$inno) %>% symmetrize.matrix()
    } else if (input$select_simulation_parameter_origin == 'Manual'){
      dinno <- hot_to_r(input$inno) %>% symmetrize.matrix()
    }
  })
  
  observeEvent(input$submit1, {
    r$data <<-
      computeData(
        selected_nvar(),
        r$nTime,
        r$error,
        r$nModel1,
        val=TRUE,
        burn=1000,
        current_phi_input(),
        current_inno_input()
      )
    if(!is.null(r$data)){
      showNotification("Parameters succesfully loaded. Simulated dataset initialized.",
                       type="message")
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
    input$select_stepsize_init
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
    numericInput(inputId='select_stepsize_init',label='Choose initial stepsize',min=1,max=1000,value=as.integer(r$nTime/20))
  })
  
  observeEvent(input$submitTPS, {
    if (is.null(r$data)) {
      return(NULL)
    }
    
    error_metric <- input$tp_error_metric
    
    K <- current_k_fold_selected()
    max_iter <- current_max_iter_selected()
    stepsize_scaler <-current_stepsize_scaler_selected()
    stepsize_init <-current_stepsize_init_selected()
    
    tp <- searchTP(
      selected_nvar(),
      r$nTime,
      r$error,
      r$nModel1,
      tp_selected_model1(),
      tp_selected_model2(),
      current_phi_input(),
      current_inno_input(),
      selectedLagNum(),
      K,
      max_iter,
      stepsize_init,
      stepsize_scaler,
      loaded_dataset_index_variable(),
      error_metric
    )
    
    #FUNCTION RETURNS NULL IF ERRORS OCCURR AND AS SUCH WE WILL NOT RENDER ANYTHING
    if(!is.null(tp)){
      pldf<-tp[[2]][[1]]
      output$tp <- renderUI({
        valueBox(tp[[1]],'# timepoints at which MSE of complexer model is lower than simpler model', 
                 icon = icon('hourglass'),
                 width=8,
                 color="green")
      })
      
      if(!is.null(tp[[2]][[2]])){
        fold_df <- tp[[2]][[2]]
        #write.csv(fold_df,paste0('fold_df_',input$select_df,'_',now(),'_.csv'))
        write.csv(fold_df,paste0('fold_df_',input$select_df,
                                 '_',lubridate::day(lubridate::today()),
                                 '_',lubridate::month(lubridate::today()),
                                 '_',lubridate::hour(lubridate::now()),
                                 '_',lubridate::minute(lubridate::now()),
                                 '_',lubridate::second(lubridate::now()),
                                 '_.csv'))
        write.csv(pldf,paste0('avg_df',input$select_df,
                                 '_',lubridate::day(lubridate::today()),
                                 '_',lubridate::month(lubridate::today()),
                                 '_',lubridate::hour(lubridate::now()),
                                 '_',lubridate::minute(lubridate::now()),
                                 '_',lubridate::second(lubridate::now()),
                                 '_.csv'))
        
      }
      
      modl <- list(tp_selected_model1(),
      tp_selected_model2())
      output$tp_last_1 <- renderUI({
        valueBox(tp[[3]],
                 # fold_df %>% 
                 #          dplyr::filter(model==modl[1]) %>% 
                 #          tail(n=1) %>% 
                 #          dplyr::select(mse),
                 paste0(toupper(error_metric),' of ', toupper(tp_selected_model1()),' at timepoint ',tp[[1]]),
                 icon = icon('hourglass'),
                 width=8,
                 color="blue")
      })
      output$tp_last_2 <- renderUI({
        valueBox(tp[[4]],
                 # fold_df %>%
                 #          dplyr::filter(model==modl[2]) %>%
                 #          tail(n=1) %>%
                 #          dplyr::select(mse),
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
          title = paste0('Density plot of average ',toupper(error_metric),'per model across time points'),
          closable=TRUE,
          width=NULL,
          collapsible=TRUE,
          plotOutput("tp_3d_plot")
          
        ),
        selectInput(inputId="select_tp_distr_mse_fold_plot",
                    label="Which time point",
                    choices=c(unique(fold_df$tl)),
                    multiple=TRUE,
                    selectize=TRUE
        ),
        boxPlus(
          title = paste0('Plot of average ',toupper(error_metric),' difference across time points'),
          closable=TRUE,
          width=NULL,
          collapsible=TRUE,
          plotlyOutput("distr_mse_fold_plot")
          
        ),
        boxPlus(
          title = paste0('Density plot of average and fold errors for time points: ', input$select_tp_distr_mse_fold_plot),
          closable=TRUE,
          width=NULL,
          collapsible=TRUE,
          plotlyOutput("distr_mse_fold_plot2")
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
            theme_classic() 
            
          
          ggplotly(p) %>% 
            layout(autosize=TRUE)
          #layout(height = input$plotHeight, autosize=TRUE)
          
          #   geom_line(arl,aes(x=tl,y=arl))
          # ggplot(res2, aes(x = res2_y, y = value, fill = variable)) + geom_line(aes(color = variable))
        })
        
        output$distr_mse_fold_plot <- renderPlotly({
          if(!is.null(tp)){
            dif <- fold_df %>%dplyr::filter(model==modl[1])-
              fold_df %>%dplyr::filter(model==modl[2])
            
            dif_df <- fold_df %>% dplyr::filter(model==modl[1]) %>% 
              dplyr::select(-starts_with('model'))
            dif_df$mse <- dif$mse
            
            
            sel_dat <- fold_df %>% dplyr::filter(tl %in% c(current_selected_tp_distr_mse_fold_plot()))
            sel_dat$fold <- as.factor(sel_dat$fold)
            colnames(sel_dat) <- c('model','tl','fold','mse')
            p<- ggplot(sel_dat,aes(x=tl,y=mse)) + 
              geom_point(aes(shape=as.factor(fold),colour=model),size=.5) + 
              geom_point(data=dif_df,aes(shape=as.factor(fold)),colour='black',size=1) +
              #geom_point(data=sel_dat,aes(x=tl,y=mean(mse),colour=model),size=1.4) + 
              #geom_linerange(data=pldf,position='dodge',aes(ymin=0,ymax=max_mse/10*count)) +
              #geom_text(data=subset(pldf,count<=1),position='dodge',aes(label=count,vjust=max_mse/10*count)) +
              scale_colour_manual(values = c("Blue", "Red")) +
              theme_classic() 
            
            ggplotly(p) %>% 
              layout(autosize=TRUE)
          }
        
          
        })
        
        output$tp_3d_plot <- renderPlot({
          tbl_tmp <- table(pldf$tl)
          pldf_binned <- pldf %>% dplyr::arrange(tl)
          tmp <- NULL
          for (i in 1:nrow(tbl_tmp)){
            if(tbl_tmp[i] < 5){
              tmp<-rbind(tmp,as.integer(names(tbl_tmp)[i]))
            } else if (!is.null(tmp)) {
              if(length(tmp)>1){
                tmp_str <- paste0(min(tmp),'-',max(tmp))
                
              } else {
                tmp_str <- paste0(tmp)
              }
              pldf_binned[pldf_binned$tl %in% tmp,]$tl <- tmp_str
              tmp <- NULL
            }
          }
          tmp <- table(pldf_binned$tl)
          tmp<-data.frame(cbind(names(tmp),sapply(data.frame(tmp)$Freq,as.character)))
          colnames(tmp)<-c('tl','freq')
          tmp$freq <- tmp$freq %>% as.character %>% as.numeric
          tmp <- tmp %>% filter(freq>10)
          pldf_binned <- pldf_binned %>% dplyr::filter(tl %in% tmp$tl)
          
          p<- ggplot(pldf_binned,aes(x=mse,y=as.factor(tl),fill=model)) +
            geom_density_ridges(aes(fill=model,point_color=model,point_fill=model),
                                alpha=.5,
                                jittered_points=TRUE,
                                point_shape = "|",
                                point_size=3,
                                position = position_points_jitter(height=0),
                                quantile_lines=TRUE,
                                scale=1.2) +
            ggridges::scale_discrete_manual(aesthetics = "point_color", values = c("darkcyan","deeppink")) +
            ggridges::scale_discrete_manual(aesthetics = "point_fill", values = c("Cyan","Pink")) +
            ggridges::scale_discrete_manual(aesthetics = "point_shape", values = c(22, 24))+
            scale_colour_manual(values = c("Black", "Black")) +
            scale_fill_cyclical(values= c("Cyan","Pink")) +
            scale_y_discrete(name="Number of observations") +
            guides(fill=guide_legend())+
            theme_ridges(center=TRUE,grid=TRUE)
          
          plot(p)
        })
        
        output$distr_mse_fold_plot2 <- renderPlotly({
          if(!is.null(tp)){

            sel_dat <- fold_df %>% dplyr::filter(tl %in% c(current_selected_tp_distr_mse_fold_plot()))
            print(colnames(sel_dat))
            sel_dat$fold <- as.factor(sel_dat$fold)
            colnames(sel_dat) <- c('model','tl','fold','mse')
            
            sel_dat2 <- pldf %>% dplyr::filter(tl %in% c(current_selected_tp_distr_mse_fold_plot()))
            print(sel_dat2)
            p<- ggplot(sel_dat,aes(x=mse)) +
              geom_density(aes(linetype=as.factor(fold),colour=model),size=.3) +
              geom_density(data=sel_dat2,aes(colour=model),size=1) +
              scale_colour_manual(values = c("Blue", "Red")) +
              theme_classic() 
            
            ggplotly(p) %>% 
              layout(autosize=TRUE)
          }
        })
          
        
        
      }
      # } else if (fold_df %>% 
      #            dplyr::filter(model==modl[2]) %>% 
      #            dplyr::select(mse) == 1){
      #   
      #   
      # }
      
    }
  })
  
  current_selected_tp_distr_mse_fold_plot <- reactive({
    input$select_tp_distr_mse_fold_plot
  })
  

    # eventdata <- event_data("plotly_selected")
    # if(!is.null(eventdata)){
    #   sel_dat <- fold_df[eventData$key,]
    # } else {
    #   sel_dat <- NULL
    # }
    # 
    # p<- ggplot(sel_dat,aes(x=tl,y=mse)) + 
    #   geom_point(data=sel_dat,aes(x=tl,y=mean(mse),colour=model),size=1.4) + 
    #   #geom_linerange(data=pldf,position='dodge',aes(ymin=0,ymax=max_mse/10*count)) +
    #   #geom_text(data=subset(pldf,count<=1),position='dodge',aes(label=count,vjust=max_mse/10*count)) +
    #   scale_colour_manual(values = c("Blue", "Red")) +
    #   theme_classic() +
    #   scale_y_continuous(limits = c(0, NA))
    # 
    # 
    # ggplotly(p,tooltip="tooltip") %>% 
    #   layout(autosize=TRUE)

  
  
  observeEvent({input$submitModelComparison},{
    withProgress(message = 'Computing parameter accuracy', value = 0, {
      acc$comparison <-
        compareAccuracy(r$data,
                        current_phi_input(),
                        current_inno_input(),
                        TRUE,
                        selectedLagNum(),
                        loaded_dataset_index_variable(),
                        'ar',
                        'var'
        )
    })
    cv$comparison <- compareCV(r$data, r$nVar, r$nTime, selectedLagNum(),loaded_dataset_index_variable())
    print("--------")
    output$best <- renderUI({
      if (typeof(acc$comparison) == "character" |
          is.null(acc$comparison)) {
        return(" ")
      } else if (anyNA(acc$comparison[[1]]) == TRUE &
                 anyNA(acc$comparison[[2]]) == TRUE) {
        return("Stationarity violated")
      } else if (mean(acc$comparison[[1]]) > mean(acc$comparison[[2]])) {
        best <- "VAR"
      } else {
        best <- "AR"
      }
      valueBox(best,"Best model based on true parameter accuracy.",icon=icon("eye"))
    })
    output$cvbest <- renderUI({
      valueBox(cv$comparison,'Best model based on APE by blocked cross-validation',icon=icon("th"))
    })
  })
  
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

  
  output$downloadInnoDataset <- downloadHandler(
    filename = function() {
      paste("inno-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(estParams()[[3]], file)
    }
  )
  
  output$downloadPhiDataset <- downloadHandler(
    filename = function() {
      paste("phi-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(estParams()[[2]], file)
    }
  )
  
  #network analysis
  
  selected_network_vars <- reactive({
    input$select_network_vars
  })
  
  output$select_network_vars_element <- renderUI({
    selectInput("select_network_vars","Select variables for network analysis",
                multiple=TRUE,
                choices = c('None',names(input_df$df))
    )
  })
  
  output$networkplot <- renderPlot({
    if(!is.null(selected_network_vars())){
      selected_cols <- selected_network_vars()
      d <- input_df$df %>% dplyr::select(selected_cols)
      Q <- qgraph(cor_auto(d,detectOrdinal = FALSE),
                  graph = "glasso", sampleSize = nrow(d),
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
