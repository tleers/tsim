# Load datasets -------------------
#datasets we use for testing purposes
library(dplyr)

alt_data95 <- read.csv(paste0('data/alt_data95/alt_data95.csv'))

sim_var <- read.csv(paste0('data/simvar/simulated_var.csv')) %>% dplyr::select(-starts_with('X'))