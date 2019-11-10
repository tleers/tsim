app_ui <- function() {
  tagList(
    fluidPage(
      verbatimTextOutput("all"),
      verbatimTextOutput("opt"), 
      verbatimTextOutput("glob")
    )
  )
}