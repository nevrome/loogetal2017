library(magrittr)

#### load data ####
load("2D-sim-results.RData")
sim_results <- final_output

#### create distance matrizes ####

# temporal distance
time <- sim_results$n25
temporal_distance_matrix <- time %>% dist %>% as.matrix

# spatial distance 
space <- sim_results %>% dplyr::select(x, y)
spatial_distance_matrix <- space %>% dist %>% as.matrix

# genetic distance
genes <- sim_results %>% dplyr::select(tidyselect::starts_with("X", ignore.case = F))
genetic_distance_matrix <- genes %>% dist %>% as.matrix

#### measuring correlation ####
tT <- vegan::mantel(genetic_distance_matrix, temporal_distance_matrix)
sT <- vegan::mantel(genetic_distance_matrix, spatial_distance_matrix)


