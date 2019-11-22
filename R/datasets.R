# Load datasets -------------------
#datasets we use for testing purposes
root <- 'C:/Users/Tim/Dropbox/Master Thesis/tsim/data'
library(dplyr)
alt_data95 <- read.csv(paste0(root,'/alt_data95/alt_data95.csv'))
alt_data95 <- alt_data95 %>% dplyr:::select(-'DEPRE')

sim_var <- read.csv(paste0(root,'/simvar/simulated_var.csv')) %>% dplyr::select(-starts_with('X'))