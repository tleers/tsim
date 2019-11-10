app_ui <- function() {
  golem::favicon("www/favicon.ico")
  tagList(
    fluidPage(
      verbatimTextOutput("all"),
      verbatimTextOutput("opt"), 
      verbatimTextOutput("glob")
    )
  )
}