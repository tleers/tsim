# Building a Prod-Ready, Robust Shiny Application.
# 
# Each step is optional. 
# 
install_package_list <- function(list.of.packages){
  new.packages <-
    list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
  if (length(new.packages) > 0) {
    install.packages(new.packages)
  }
  lapply(list.of.packages, usethis::use_package, character.only = T)
}
# 2. All along your project

## 2.1 Add modules
## 
golem::add_module( name = "my_first_module" ) # Name of the module
golem::add_module( name = "my_other_module" ) # Name of the module

## 2.2 Add dependencies

list.of.packages = c(
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
  "shiny",
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
  "latex2exp"
)
#install_package_list(list.of.packages)


usethis::use_package("pracma")
usethis::use_package("latex2exp")
usethis::use_package("shinyWidgets")
usethis::use_package("rlist")
usethis::use_package("shinydashboard")
usethis::use_package("plotly")
usethis::use_package("rintrojs")
usethis::use_package("tibble")
#usethis::use_package("tidyverse")
usethis::use_package("rhandsontable")
usethis::use_package("dplyr")

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
