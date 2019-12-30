list.of.packages = c( #-------------------------
                      # "V8",
                      # "shinyjs",
                      "dplyr",
                      "viridis",
                      "stringr",
                      "roxygen2",
                      "devtools",
                      "golem",
                      "mvtnorm",
                      "ggridges",
                      "shinythemes",
                      "qgraph",
                      "shinydashboardPlus",
                      "Matrix",
                      "shinyWidgets",
                      "plotly", 
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
                      "here",
                      "devtools",
                      "tibble",
                      #"rintrojs",
                      "matrixcalc"
)

print('')


#----------
install_package_list <- function(list.of.packages,install=TRUE){
  
  if(install==TRUE){
  new.packages <-
    list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
  if (length(new.packages) > 0) {
    install.packages(new.packages)
  }
  lapply(list.of.packages, install.packages ,character.only = T)
  
  }
  new.packages <-
    list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
  if (length(new.packages) > 0) {
    install.packages(new.packages)
  }
  lapply(list.of.packages, usethis::use_package)
  
}

# status <- try(install_package_list(list.of.packages))
# if(status=='try-error'){
#     new.packages <-
#       list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
#     if (length(new.packages) > 0) {
#       install.packages(new.packages)
#     }
#     lapply(list.of.packages, usethis::use_package("pkg"),character.only = T)
# }
# 
# github.list <- c('daattali/shinyjs')
# devtools::install_github("daattali/shinyjs")
# 
# 
# if(status=='try-error'){
#   new.packages <-
#     github.list[!(github.list %in% installed.packages()[, "Package"])]
#   if (length(new.packages) > 0) {
#     install.packages(new.packages)
#   }
#   lapply(github.list, devtools::install_github,character.only = T)
# }