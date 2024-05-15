################################################################################
##
## Script to compute one random forest with all variables for each taxas
## ... - 5 random forests - to get a list of variables for the SEM with Faith's PD
## ... and create a df for SEM with only variables to keep in the SEM according
## ... to randomforest results
##
## Camille Magneville
##
## 14/05/2024
##
## 9_a_Random_forests_all_taxa_PD_Faith.R
##
################################################################################


# Define the pipe symbol so I can use it:
`%>%` <- magrittr::`%>%`


# 1 - Load environmental data and PD ==================================


# Load environmental drivers (with no NA for predictors and only cells which
# .. have values for all the studied taxa):
envdriv_full_db <- readRDS(here::here("transformed_data", "env_db",
                                       "env_drivers_final_noNA_db.rds"))

# Load SES PD - Faith:
faith_ses_birds_df <- readRDS(here::here("transformed_data",
                                         "div_values_null_models",
                                         "PD_Faith_null_models_metrics_50km_BIRDS.rds"))
faith_ses_trees_df <- readRDS(here::here("transformed_data",
                                         "div_values_null_models",
                                         "PD_Faith_null_models_metrics_50km_TREES.rds"))
faith_ses_reptiles_df <- readRDS(here::here("transformed_data",
                                         "div_values_null_models",
                                         "PD_Faith_null_models_metrics_50km_REPTILES.rds"))

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
cells_ok_birds <- unique(faith_ses_birds_df$Idgrid)
cells_ok_reptiles <- unique(faith_ses_reptiles_df$Idgrid)
cells_ok_trees <- unique(faith_ses_trees_df$Idgrid)
cells_to_keep <- intersect(intersect(cells_ok_birds,
                                     cells_ok_reptiles),
                           cells_ok_trees)
locate.cells(cell_vect = cells_to_keep,
             grid = grid_50km)

# Only keep these cells in the diversity df:
faith_ses_birds_df <- faith_ses_birds_df %>%
  dplyr::filter(Idgrid %in% cells_to_keep)
faith_ses_reptiles_df <- faith_ses_reptiles_df %>%
  dplyr::filter(Idgrid %in% cells_to_keep)
faith_ses_trees_df <- faith_ses_trees_df %>%
  dplyr::filter(Idgrid %in% cells_to_keep)


# Link the two tables (drivers + diversity):
rf_faith_birds_df <- dplyr::left_join(envdriv_full_db,
                                      faith_ses_birds_df[, c("Idgrid", "ses")],
                                      by = "Idgrid")
rf_faith_trees_df <- dplyr::left_join(envdriv_full_db,
                                      faith_ses_trees_df[, c("Idgrid", "ses")],
                                      by = "Idgrid")
rf_faith_reptiles_df <- dplyr::left_join(envdriv_full_db,
                                      faith_ses_reptiles_df[, c("Idgrid", "ses")],
                                      by = "Idgrid")

# Put Idgrid as rownames:
rf_faith_birds_df <- rf_faith_birds_df %>%
  tibble::column_to_rownames(var = "Idgrid")
rf_faith_trees_df <- rf_faith_trees_df %>%
  tibble::column_to_rownames(var = "Idgrid")
rf_faith_reptiles_df <- rf_faith_reptiles_df %>%
  tibble::column_to_rownames(var = "Idgrid")


# 4 - Random forest for birds ==================================================


# Check types of variables
str(rf_faith_birds_df)

# Check that diversity metric doesn't have NA:
rownames(rf_faith_birds_df[which(is.na(rf_faith_birds_df$ses) == TRUE), ])

# Change SES from names num to num:
rf_faith_birds_df$ses <- as.numeric(rf_faith_birds_df$ses)


# Set seed for randomisation:
set.seed(42)

# Run the random forest model with mtry = predictors / 3 and 1000 trees:
rf1 <- randomForest::randomForest(ses~.,
                                  data = rf_faith_birds_df,
                                  ntree= 1000,
                                  mtry = floor((ncol(rf_faith_birds_df) - 1)/3),
                                  importance = TRUE)
plot(rf1) # so can redo it with 500 trees - seems stable
print(rf1)
randomForest::importance(rf1)
randomForest::varImpPlot(rf1)

# Run the random forest model mtry = predictors / 3 and 500 trees:
rf2 <- randomForest::randomForest(ses~.,
                                  data = rf_faith_birds_df,
                                  ntree = 500,
                                  mtry = floor((ncol(rf_faith_birds_df) - 1)/3),
                                  importance = TRUE)
plot(rf2)
print(rf2)
randomForest::importance(rf2)
randomForest::varImpPlot(rf2)

# Find the optimal value for trees nb and mtry:
mtry <- randomForest::tuneRF(rf_faith_birds_df[-ncol(rf_faith_birds_df)],
                             rf_faith_birds_df$ses,
                             mtryStart = 16,
                             ntreeTry = 500,
                             stepFactor = 1.5,
                             improve = 0.00001,
                             trace = TRUE,
                             plot = TRUE)
print(mtry) # mtry = 24 seems the best one


# Run the random forest model mtry = 24 and 500 trees:
rf_birds <- randomForest::randomForest(ses~.,
                                  data = rf_faith_birds_df,
                                  ntree = 500,
                                  mtry = 24,
                                  importance = TRUE)
plot(rf_birds)
print(rf_birds)
randomForest::importance(rf_birds)
randomForest::varImpPlot(rf_birds)


# 4 - Random forest for reptiles ==================================================


# Check types of variables
str(rf_faith_reptiles_df)

# Check that diversity metric doesn't have NA:
rownames(rf_faith_reptiles_df[which(is.na(rf_faith_reptiles_df$ses) == TRUE), ])

# Change SES from names num to num:
rf_faith_reptiles_df$ses <- as.numeric(rf_faith_reptiles_df$ses)


# Set seed for randomisation:
set.seed(42)

# Run the random forest model with mtry = predictors / 3 and 1000 trees:
rf1 <- randomForest::randomForest(ses~.,
                                  data = rf_faith_reptiles_df,
                                  ntree= 1000,
                                  mtry = floor((ncol(rf_faith_reptiles_df) - 1)/3),
                                  importance = TRUE)
plot(rf1) # so can redo it with 500 trees - seems stable
print(rf1)
randomForest::importance(rf1)
randomForest::varImpPlot(rf1)

# Run the random forest model mtry = predictors / 3 and 500 trees:
rf2 <- randomForest::randomForest(ses~.,
                                  data = rf_faith_reptiles_df,
                                  ntree = 500,
                                  mtry = floor((ncol(rf_faith_reptiles_df) - 1)/3),
                                  importance = TRUE)
plot(rf2)
print(rf2)
randomForest::importance(rf2)
randomForest::varImpPlot(rf2)

# Find the optimal value for trees nb and mtry:
mtry <- randomForest::tuneRF(rf_faith_reptiles_df[-ncol(rf_faith_reptiles_df)],
                             rf_faith_reptiles_df$ses,
                             mtryStart = 16,
                             ntreeTry = 500,
                             stepFactor = 1.5,
                             improve = 0.00001,
                             trace = TRUE,
                             plot = TRUE)
print(mtry) # mtry = 8 seems the best one


# Run the random forest model mtry = 8 and 500 trees:
rf_reptiles <- randomForest::randomForest(ses~.,
                                  data = rf_faith_reptiles_df,
                                  ntree = 500,
                                  mtry = 8,
                                  importance = TRUE)
plot(rf_reptiles)
print(rf_reptiles)
randomForest::importance(rf_reptiles)
randomForest::varImpPlot(rf_reptiles)


# 5 - Random forest for trees ==================================================


# Check types of variables
str(rf_faith_trees_df)

# Check that diversity metric doesn't have NA:
rownames(rf_faith_trees_df[which(is.na(rf_faith_trees_df$ses) == TRUE), ])

# Change SES from names num to num:
rf_faith_trees_df$ses <- as.numeric(rf_faith_trees_df$ses)


# Set seed for randomisation:
set.seed(42)

# Run the random forest model with mtry = predictors / 3 and 1000 trees:
rf1 <- randomForest::randomForest(ses~.,
                                  data = rf_faith_trees_df,
                                  ntree= 1000,
                                  mtry = floor((ncol(rf_faith_trees_df) - 1)/3),
                                  importance = TRUE)
plot(rf1) # so can redo it with 500 trees - seems stable
print(rf1)
randomForest::importance(rf1)
randomForest::varImpPlot(rf1)

# Run the random forest model mtry = predictors / 3 and 500 trees:
rf2 <- randomForest::randomForest(ses~.,
                                  data = rf_faith_trees_df,
                                  ntree = 500,
                                  mtry = floor((ncol(rf_faith_trees_df) - 1)/3),
                                  importance = TRUE)
plot(rf2)
print(rf2)
randomForest::importance(rf2)
randomForest::varImpPlot(rf2)

# Find the optimal value for trees nb and mtry:
mtry <- randomForest::tuneRF(rf_faith_trees_df[-ncol(rf_faith_trees_df)],
                             rf_faith_trees_df$ses,
                             mtryStart = 16,
                             ntreeTry = 500,
                             stepFactor = 1.5,
                             improve = 0.00001,
                             trace = TRUE,
                             plot = TRUE)
print(mtry) # mtry = 36


# Run the random forest model mtry = 36 and 500 trees:
rf_trees <- randomForest::randomForest(ses~.,
                                          data = rf_faith_trees_df,
                                          ntree = 500,
                                          mtry = 36,
                                          importance = TRUE)
plot(rf_trees)
print(rf_trees)
randomForest::importance(rf_trees)
randomForest::varImpPlot(rf_trees)


# 6 - Compare the rf for the taxa and chose drivers ============================


drivers_birds <- dplyr::arrange(as.data.frame(randomForest::importance(rf_birds)),
                                desc("%IncMSE"))
drivers_reptiles <- dplyr::arrange(as.data.frame(randomForest::importance(rf_reptiles)),
                                desc("%IncMSE"))
drivers_trees <- dplyr::arrange(as.data.frame(randomForest::importance(rf_trees)),
                                desc("%IncMSE"))



