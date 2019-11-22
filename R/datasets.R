# Load datasets -------------------
#datasets we use for testing purposes

alt_data95 <- read.csv('../input/alt_data95.csv')
alt_data95 <- alt_data95 %>% dplyr:::select(-'DEPRE')

sim_var <- read.csv('../input/simulated_var.csv') %>% dplyr::select(-starts_with('X'))