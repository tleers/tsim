
app_server <- function(input, output,session) {
  
  output$all <- renderPrint({ get_golem_options() })
  output$opt <- renderPrint({ get_golem_options("a") })
  output$glob <- renderPrint({ getOption("golem.app.name") })
}
