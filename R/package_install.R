if(!require(golem)){
  install.packages('golem')
}

install_package_list <- function(list.of.packages){
  new.packages <-
    list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
  if (length(new.packages) > 0) {
    install.packages(new.packages)
  }
  lapply(list.of.packages, usethis::use_package)
}

list.of.packages = c(
  "ggridges",
  "shinythemes",
  "qgraph",
  "shinydashboardPlus",
  "Matrix",
  "shinyWidgets",
  "plotly", 
  #"caret",
  "dplyr", 
  "data.table", 
  "lubridate", 
  "reshape2",
  "DT", 
  "knitr", 
  "kableExtra",
  "datasets",
  "ggplot2",
  "MASS",
  "psych",
  "stats",
  "shinydashboard",
  "reshape2",
  "vars",
  "xtable",
  "Hmisc",
  "reshape",
  "rowr",
  "grid",
  "gridExtra",
  "rlist",
  "pracma",
  "latex2exp",
  "dplyr",
  "rhandsontable",
  "lubridate",
  #"RSQLite",
  "here",
  "devtools",
  #"sqldf",
  #"DBI",
  #"dbplyr",
  "pool",
  #"RMySQL",
  #"tidyverse",
  "tibble",
  "tsibble",
  "rintrojs",
  "sparsevar"
)
status <- try(install_package_list(list.of.packages))
if(status=='try-error'){
    new.packages <-
      list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
    if (length(new.packages) > 0) {
      install.packages(new.packages)
    }
    lapply(list.of.packages, require,character.only = T)
}
