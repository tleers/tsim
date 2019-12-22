#------------------------------ Data Tab 1 - DT1 ---------------------------------------------###############################
#------------------------------ DT1 - UI ---------------------------------------------###############################
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
  prev_table$inputs_list <- 
    if(input$data_source == "data_frame") {
      # Case I - load in memory data frames
      # If there is no any data frame available in memory
      if(length(prev_table$data_frame_list) == 0){
        showModal(
          modalDialog(
            title = "Warning: No data frame",
            HTML(paste("There is no data frame available",
                       "in R Global Environment", 
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
  else {
    NA
  }
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
#available datasets to the menu selection
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

#-------------------------------- DT1 - Table Dat ----
# Load the data according to the user selection  
df_tbl_view <- reactive({
  prev_table$class <- NULL
  if(input$data_source == "data_frame" & 
     length(prev_table$data_frame_list) != 0){
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
