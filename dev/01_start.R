# Building a Prod-Ready, Robust Shiny Application.
# 
# Each step is optional. 
# 
# 1 - On init
# 
## 1.1 - Fill the descripion & set options
## 
## Add information about the package that will contain your app

golem::fill_desc(
  pkg_name = "tsim", # The Name of the package containing the App 
  pkg_title = "Sample size planning application for time series research", # The Title of the package containing the App 
  pkg_description = "Time series data are increasingly used as they allow researchers to capture the dynamics of psychological processes. Especially models that estimate the relationships between variables over time, such as the vector autoregression model (VAR), are frequently used. Unfortunately, due to the high complexity of estimating the relationships between all variables, VAR models are at risk of overfitting the data. Moreover, it is not clear how large the sample size must be to avoid overfitting the data. In this thesis, we propose a method that allows us to estimate the number of needed time points to fit a VAR model. To do so, we rely on a comparison with the simpler autoregression model (AR), which only estimates the relationships between each variable and itself at previous time points. By using blocked cross-validation (CV), a method taking into account the dependency of time series, we can estimate how well a model generalizes to out-of-sample data. We use blocked CV to fit both models to a series of simulated datasets, based on a user-defined data-generating model. By varying the number of time points in these datasets, we are able to estimate at which time point VAR generalizes better than AR. These methods are implemented into a Shiny application in R to allow for code-free interaction through a user interface, including the input of model parameters based on estimates from previously collected data.", # The Description of the package containing the App 
  author_first_name = "Tim", # Your First Name
  author_last_name = "Leers",  # Your Last Name
  author_email = "mail@timleers.com",      # Your Email
  repo_url = 'https://github.com/tleers/tsim' # The (optional) URL of the GitHub Repo
)     

## Use this desc to set {golem} options

golem::set_golem_options()

## 1.2 - Set common Files 
## 
## If you want to use the MIT licence, README, code of conduct, lifecycle badge, and news

usethis::use_mit_license( name = "Golem User" )  # You can set another licence here
usethis::use_readme_rmd( open = FALSE )
usethis::use_code_of_conduct()
usethis::use_lifecycle_badge( "Experimental" )

usethis::use_news_md( open = FALSE )
usethis::use_git()
#usethis::use_github()
## 1.3 - Add a data-raw folder
## 
## If you have data in your package
usethis::use_data_raw( name = "Bringmann2016", open = FALSE ) 
#usethis::use_data_raw( name = "alt_data95", open = FALSE ) 
usethis::use_data_raw( name = "sim_var", open = FALSE ) 
#usethis::use_data(alt_data95)
usethis::use_data(sim_var)
usethis::use_data(Bringmann2016)


## 1.4 - Init Tests
## 
## Create a template for tests

golem::use_recommended_tests()

## 1.5 : Use Recommended Package

golem::use_recommended_deps()

## 1.6 Add various tools

# If you want to change the favicon (default is golem's one)
golem::remove_favicon()
golem::use_favicon() # path = "path/to/ico". Can be an online file. 

# Add helper functions 
golem::use_utils_ui()
golem::use_utils_server()

# You're now set! 
# go to dev/02_dev.R
rstudioapi::navigateToFile( "dev/02_dev.R" )

