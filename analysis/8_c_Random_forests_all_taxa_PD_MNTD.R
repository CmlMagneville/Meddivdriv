################################################################################
##
## Script to compute one random forest with all variables for each taxas
## ... - 5 random forests - to see which variable drive the most MNTD.
##
## Camille Magneville
##
## 04/06/2024
##
## 8_c_Random_forests_all_taxa_PD_MNTD.R
##
################################################################################




# Define the pipe symbol so I can use it:
`%>%` <- magrittr::`%>%`


# 1 - Load environmental data and PD ==================================


# Load environmental drivers (with no NA for predictors and only cells which
# .. have values for all the studied taxa):
envdriv_full_db <- readRDS(here::here("transformed_data", "env_db",
                                      "env_drivers_final_noNA_db.rds"))

# Load SES PD - mntd:
mntd_ses_birds_df <- readRDS(here::here("transformed_data",
                                       "div_values_null_models",
                                       "PD_MNTD_null_models_metrics_50km_BIRDS.rds"))
mntd_ses_trees_df <- readRDS(here::here("transformed_data",
                                       "div_values_null_models",
                                       "PD_MNTD_null_models_metrics_50km_TREES.rds"))
mntd_ses_reptiles_df <- readRDS(here::here("transformed_data",
                                          "div_values_null_models",
                                          "PD_MNTD_null_models_metrics_50km_REPTILES.rds"))

# Load grid data(for locating grid cells):
grid_50km <- sf::st_read(here::here("integradiv_db",
                                    "spgrid_50x50km_EUROMEDIT_EPSG3035.shp"))
# Rename the GRD_ID column as Idgrid:
grid_50km <- dplyr::rename(grid_50km, Idgrid = GRD_ID)


# 2 - Subset diversity db and link the two databases (diversity + drivers) =====


# NOTE: If joining drivers db and diversity db, the final db would have
# ... 671 rows (grid cells) but for some of these grid cells, we don't have
# ... occurrence data for now : (birds 664 grid cells, reptiles 624 grid cells,
# ... trees 667 grid cells)
# ... SO: Only keep the grid cells for which I have occ information for all taxa
# ... already done for the environmental db (cf 7_Clean_environmental_var.R)


# Get the names of the Idgrid to keep (diversity data for all taxa):
cells_ok_birds <- unique(mntd_ses_birds_df$Idgrid)
cells_ok_reptiles <- unique(mntd_ses_reptiles_df$Idgrid)
cells_ok_trees <- unique(mntd_ses_trees_df$Idgrid)
cells_to_keep <- intersect(intersect(cells_ok_birds,
                                     cells_ok_reptiles),
                           cells_ok_trees)
locate.cells(cell_vect = cells_to_keep,
             grid = grid_50km)

# Only keep these cells in the diversity df:
mntd_ses_birds_df <- mntd_ses_birds_df %>%
  dplyr::filter(Idgrid %in% cells_to_keep)
mntd_ses_reptiles_df <- mntd_ses_reptiles_df %>%
  dplyr::filter(Idgrid %in% cells_to_keep)
mntd_ses_trees_df <- mntd_ses_trees_df %>%
  dplyr::filter(Idgrid %in% cells_to_keep)


# Link the two tables (drivers + diversity):
rf_mntd_birds_df <- dplyr::left_join(envdriv_full_db,
                                    mntd_ses_birds_df[, c("Idgrid", "ses")],
                                    by = "Idgrid")
rf_mntd_trees_df <- dplyr::left_join(envdriv_full_db,
                                    mntd_ses_trees_df[, c("Idgrid", "ses")],
                                    by = "Idgrid")
rf_mntd_reptiles_df <- dplyr::left_join(envdriv_full_db,
                                       mntd_ses_reptiles_df[, c("Idgrid", "ses")],
                                       by = "Idgrid")


# Check that diversity metrics don't have NA:
rf_mntd_birds_df$Idgrid[which(is.na(rf_mntd_birds_df$ses) == TRUE)]
rf_mntd_trees_df$Idgrid[which(is.na(rf_mntd_trees_df$ses) == TRUE)]
rf_mntd_reptiles_df$Idgrid[which(is.na(rf_mntd_reptiles_df$ses) == TRUE)]

# Only keep the cells where the diversity metric can be computed for all taxa
# ... otherwise one driver can be important for a given taxa but not for others
# ... given that this driver has specific values on the grid cells that do not
# ... contain all taxa:
to_remove <- unique(c(rf_mntd_birds_df$Idgrid[which(is.na(rf_mntd_birds_df$ses) == TRUE)],
                      rf_mntd_trees_df$Idgrid[which(is.na(rf_mntd_trees_df$ses) == TRUE)],
                      rf_mntd_reptiles_df$Idgrid[which(is.na(rf_mntd_reptiles_df$ses) == TRUE)]))
rf_mntd_birds_df <- rf_mntd_birds_df %>%
  dplyr::filter(! Idgrid %in% to_remove)
rf_mntd_reptiles_df <- rf_mntd_reptiles_df %>%
  dplyr::filter(! Idgrid %in% to_remove)
rf_mntd_trees_df <- rf_mntd_trees_df %>%
  dplyr::filter(! Idgrid %in% to_remove)

# Put Idgrid as rownames:
rf_mntd_birds_df <- rf_mntd_birds_df %>%
  tibble::column_to_rownames(var = "Idgrid")
rf_mntd_trees_df <- rf_mntd_trees_df %>%
  tibble::column_to_rownames(var = "Idgrid")
rf_mntd_reptiles_df <- rf_mntd_reptiles_df %>%
  tibble::column_to_rownames(var = "Idgrid")


# With which cells are we finally doing the rf?
locate.cells(cell_vect = rownames(rf_mntd_reptiles_df),
             grid = grid_50km)


# 4 - Random forest for birds ==================================================


# Check types of variables
str(rf_mntd_birds_df)

# Change SES from names num to num:
rf_mntd_birds_df$ses <- as.numeric(rf_mntd_birds_df$ses)

# Set seed for randomisation:
set.seed(42)

# See if 500 trees and mtry = 16 ok:
# Run the random forest model mtry = 16 and 500 trees:
rf_birds <- randomForest::randomForest(ses~.,
                                       data = rf_mntd_birds_df,
                                       ntree = 500,
                                       mtry = 16,
                                       importance = TRUE)
# ntree:
plot(rf_birds)

# mtry:
mtry <- randomForest::tuneRF(rf_mntd_birds_df[-ncol(rf_mntd_birds_df)],
                             rf_mntd_birds_df$ses,
                             mtryStart = 16,
                             ntreeTry = 500,
                             stepFactor = 1.5,
                             improve = 0.00001,
                             trace = TRUE,
                             plot = TRUE)
print(mtry) # mtry = 16 seems ok (after a few tries)


# Compute 100 random forests and mean importance of each variable:
# % Var explained around 50%
varimp_birds <- test.rf.model(rf_data = rf_mntd_birds_df,
                              iteration_nb = 100)
# Save it:
saveRDS(varimp_birds, here::here("transformed_data",
                                 "rf_birds_PD_mntd_50.rds"))

# Plot the variables importance (for scaling cf after all rf done for all taxa):
max(varimp_birds$mean_imp)
# max(varimp_trees$mean_imp)
# max(varimp_reptiles$mean_imp)
varimp_plot_birds <- varimp.plot(var_imp_df = varimp_birds,
                                 max = 24)

# Save it:
ggplot2::ggsave(plot = varimp_plot_birds,
                filename = here::here("outputs",
                                      "varimp_PD_mntd_50_BIRDS.pdf"),
                device = "pdf",
                scale = 1,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)



# 4 - Random forest for reptiles ==================================================


# Check types of variables
str(rf_mntd_reptiles_df)

# Change SES from names num to num:
rf_mntd_reptiles_df$ses <- as.numeric(rf_mntd_reptiles_df$ses)

# Set seed for randomisation:
set.seed(42)

# See if 500 trees and mtry = 16 ok:
# Run the random forest model mtry = 16 and 500 trees:
rf_reptiles <- randomForest::randomForest(ses~.,
                                          data = rf_mntd_reptiles_df,
                                          ntree = 500,
                                          mtry = 16,
                                          importance = TRUE)
# ntree:
plot(rf_reptiles)

# mtry:
mtry <- randomForest::tuneRF(rf_mntd_reptiles_df[-ncol(rf_mntd_reptiles_df)],
                             rf_mntd_reptiles_df$ses,
                             mtryStart = 16,
                             ntreeTry = 500,
                             stepFactor = 1.5,
                             improve = 0.00001,
                             trace = TRUE,
                             plot = TRUE)
print(mtry) # mtry = 16 seems ok (after a few tries)


# Compute 100 random forests and mean importance of each variable:
# % Var explained between 25-30%
varimp_reptiles <- test.rf.model(rf_data = rf_mntd_reptiles_df,
                                 iteration_nb = 100)

# Save it:
saveRDS(varimp_reptiles, here::here("transformed_data",
                                    "rf_reptiles_PD_mntd_50.rds"))

# Plot the variables importance:
varimp_plot_reptiles <- varimp.plot(var_imp_df = varimp_reptiles,
                                    max = 24)

# Save it:
ggplot2::ggsave(plot = varimp_plot_reptiles,
                filename = here::here("outputs",
                                      "varimp_PD_mntd_50_REPTILES.pdf"),
                device = "pdf",
                scale = 1,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)

# 5 - Random forest for trees ==================================================


# Check types of variables
str(rf_mntd_trees_df)

# Change SES from names num to num:
rf_mntd_trees_df$ses <- as.numeric(rf_mntd_trees_df$ses)

# Set seed for randomisation:
set.seed(42)

# See if 500 trees and mtry = 16 ok:
# Run the random forest model mtry = 16 and 500 trees:
rf_trees <- randomForest::randomForest(ses~.,
                                       data = rf_mntd_trees_df,
                                       ntree = 500,
                                       mtry = 16,
                                       importance = TRUE)
# ntree:
plot(rf_trees)

# mtry:
mtry <- randomForest::tuneRF(rf_mntd_trees_df[-ncol(rf_mntd_trees_df)],
                             rf_mntd_trees_df$ses,
                             mtryStart = 16,
                             ntreeTry = 500,
                             stepFactor = 1.5,
                             improve = 0.00001,
                             trace = TRUE,
                             plot = TRUE)
print(mtry) # mtry = 16 seems ok (after a few tries)


# Compute 100 random forests and mean importance of each variable:
# % Var explained between 25-30%:
varimp_trees <- test.rf.model(rf_data = rf_mntd_trees_df,
                              iteration_nb = 100)

# Save it:
saveRDS(varimp_trees, here::here("transformed_data",
                                 "rf_trees_PD_mntd_50.rds"))

# Plot the variables importance:
varimp_plot_trees <- varimp.plot(var_imp_df = varimp_trees,
                                 max = 24)

# Save it:
ggplot2::ggsave(plot = varimp_plot_trees,
                filename = here::here("outputs",
                                      "varimp_PD_mntd_50_TREES.pdf"),
                device = "pdf",
                scale = 1,
                height = 5000,
                width = 8000,
                units = "px",
                dpi = 600)


# 6 - Plot a heatmap comparing variables importance across taxa ================


# a - Load rf data:
birds_rf <- readRDS(here::here("transformed_data", "rf_birds_PD_mntd_50.rds"))
reptiles_rf <- readRDS(here::here("transformed_data", "rf_reptiles_PD_mntd_50.rds"))
trees_rf <- readRDS(here::here("transformed_data", "rf_trees_PD_mntd_50.rds"))

rf_all_taxa_list <- list("birds_rf" = birds_rf,
                         "reptiles_rf" = reptiles_rf,
                         "trees_rf" = trees_rf)

# Plot and save (only colors, no nb):
PD_heatmap_nonb <- heatmap.varimp(rf_all_taxa_list,
                                  metric_nm = "MNTD - PD",
                                  plot_nb = FALSE)
# Plot and save (plot also nb):
PD_heatmap_nb <- heatmap.varimp(rf_all_taxa_list,
                                metric_nm = "MNTD - PD",
                                plot_nb = TRUE)
