# Building a Prod-Ready, Robust Shiny Application.
# 
# Each step is optional. 
# 

# 2. All along your project

## 2.1 Add modules
## 
golem::add_module( name = "my_first_module" ) # Name of the module
golem::add_module( name = "my_other_module" ) # Name of the module

## 2.2 Add dependencies

source('package_install.R')
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
#----------
install_package_list(list.of.packages,install=FALSE)

## 2.3 Add tests

usethis::use_test( "app" )

## 2.4 Add a browser button

golem::browser_button()

## 2.5 Add external files

golem::add_js_file( "script" )
golem::add_js_handler( "handlers" )
golem::add_css_file( "custom" )

# 3. Documentation

## 3.1 Vignette
usethis::use_vignette("tsim")
devtools::build_vignettes() #crashes right now?

## 3.2 Code coverage
## You'll need GitHub there
usethis::use_github()
# usethis::use_travis()
# usethis::use_appveyor()

# You're now set! 
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")
