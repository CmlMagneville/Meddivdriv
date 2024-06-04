################################################################################
##
## Script to compute PCA for each driver category to get synthetic variables
##
## Camille Magneville
##
## 03/06/2024
##
## 10_PCA_environmental_variables.R
##
################################################################################


# Define the pipe symbol so I can use it:
`%>%` <- magrittr::`%>%`


# 1 - Load data ================================================================


# Load the drivers database:
# Note: only where no NA for drivers and taxa:
drivers_db <- readRDS(here::here("transformed_data",
                                 "env_db",
                                 "env_drivers_final_noNA_db.rds"))

# Put cells as rownames:
rownames(drivers_db) <- NULL
drivers_db <- drivers_db %>%
  tibble::column_to_rownames("Idgrid")


# Load grid data(for locating grid cells):
grid_50km <- sf::st_read(here::here("integradiv_db",
                                    "spgrid_50x50km_EUROMEDIT_EPSG3035.shp"))
# Rename the GRD_ID column as Idgrid:
grid_50km <- dplyr::rename(grid_50km, Idgrid = GRD_ID)


# 2 - PCA for past climate stability ============================================


# Compute PCA and scale drivers (to avoid some variables to be dominant because of units):
past_clim_pca <- FactoMineR::PCA(drivers_db[,
                                           c("Past_CCVelHolocene_mean.voccMag",
                                             "Past_CCVelLGM_mean.voccMag",
                                             "Past_CCVelShortTerm_mean.voccMag",
                                             "Past_CCVelYoungerDryas_mean.voccMag",
                                             "Past_MAT_sd",
                                             "Past_TAP_sd")],
                                 graph = TRUE,
                                 scale.unit = TRUE,
                                 ncp = 2)

# Get percentage of variance explained by these two first axes: 77% ok
past_clim_pca$eig

# Get variables contribution to the first two axes:
past_clim_pca$var$contrib

# Retrieve grid cells coordinates across 2 axes = new synthetic variables:
syntvar_past_clim_stab <- as.data.frame(past_clim_pca$ind$coord)
# Rename columns:
colnames(syntvar_past_clim_stab) <- c("PastClimStab_dim1",
                                      "PastClimStab_dim2")


# 3 - PCA for present climate mean =============================================


# Compute PCA and scale drivers (to avoid some variables to be dominant because of units):
present_clim_mean_pca <- FactoMineR::PCA(drivers_db[,
                                            c("Present_AI_mean",
                                              "Present_MAT_mean",
                                              "Present_TAP_mean")],
                                 graph = TRUE,
                                 scale.unit = TRUE,
                                 ncp = 2)

# Get percentage of variance explained by these two first axes: 92% ok
present_clim_mean_pca$eig

# Get variables contribution to the first two axes:
present_clim_mean_pca$var$contrib

# Retrieve grid cells coordinates across 2 axes = new synthetic variables:
syntvar_present_clim_mean <- as.data.frame(present_clim_mean_pca$ind$coord)
# Rename columns:
colnames(syntvar_present_clim_mean) <- c("PresentClimMean_dim1",
                                          "PresentClimMean_dim2")



# 4 - PCA for present habitat mean =============================================


# Compute PCA and scale drivers (to avoid some variables to be dominant because of units):
present_hab_mean_pca <- FactoMineR::PCA(drivers_db[,
                                                    c("Depth_mean",
                                                      "Elv_mean",
                                                      "OC_mean",
                                                      "pH_mean",
                                                      "VWC_mean")],
                                         graph = TRUE,
                                         scale.unit = TRUE,
                                         ncp = 3)

# Get percentage of variance explained by these three first axes: 77% ok
present_hab_mean_pca$eig

# Get variables contribution to the first two axes:
present_hab_mean_pca$var$contrib

# Retrieve grid cells coordinates across 3 axes = new synthetic variables:
syntvar_present_hab_mean <- as.data.frame(present_hab_mean_pca$ind$coord)
# Rename columns:
colnames(syntvar_present_hab_mean) <- c("PresentHabMean_dim1",
                                         "PresentHabMean_dim2",
                                         "PresentHabMean_dim3")


# 5 - PCA for present climate variations =======================================


# Compute PCA and scale drivers (to avoid some variables to be dominant because of units):
present_clim_sd_pca <- FactoMineR::PCA(drivers_db[,
                                                    c("Present_AI_stdev",
                                                      "Present_MAT_stdev",
                                                      "Present_TAP_stdev")],
                                         graph = TRUE,
                                         scale.unit = TRUE,
                                         ncp = 2)

# Get percentage of variance explained by these two first axes: 90% ok
present_clim_sd_pca$eig

# Get variables contribution to the first two axes:
present_clim_sd_pca$var$contrib

# Retrieve grid cells coordinates across 2 axes = new synthetic variables:
syntvar_present_clim_sd <- as.data.frame(present_clim_sd_pca$ind$coord)
# Rename columns:
colnames(syntvar_present_clim_sd) <- c("PresentClimSd_dim1",
                                       "PresentClimSd_dim2")


# 6 - PCA for present habitat variations =======================================


# Compute PCA and scale drivers (to avoid some variables to be dominant because of units):
present_hab_sd_pca <- FactoMineR::PCA(drivers_db[,
                                                   c("Depth_stdev",
                                                     "Elv_stdev",
                                                     "OC_stdev",
                                                     "pH_stdev",
                                                     "VWC_stdev")],
                                        graph = TRUE,
                                        scale.unit = TRUE,
                                        ncp = 3)

# Get percentage of variance explained by these three first axes: 81% ok
present_hab_sd_pca$eig

# Get variables contribution to the first two axes:
present_hab_sd_pca$var$contrib

# Retrieve grid cells coordinates across 3 axes = new synthetic variables:
syntvar_present_hab_sd <- as.data.frame(present_hab_sd_pca$ind$coord)
# Rename columns:
colnames(syntvar_present_hab_sd) <- c("PresentHabSd_dim1",
                                      "PresentHabSd_dim2",
                                      "PresentHabSd_dim3")


# 7 - PCA for fire =============================================================


# Compute PCA and scale drivers (to avoid some variables to be dominant because of units):
fire_pca <- FactoMineR::PCA(drivers_db[,
                                       c("Pr_FInt_2000_2023_mean",
                                         "Pr_FInt_2000_2023_sd",
                                         "Pr_FSurf_2000_2023_pixels")],
                                        graph = TRUE,
                                        scale.unit = TRUE,
                                        ncp = 2)

# Get percentage of variance explained by these two first axes: 99% ok
fire_pca$eig

# Get variables contribution to the first two axes:
fire_pca$var$contrib

# Retrieve grid cells coordinates across 3 axes = new synthetic variables:
syntvar_fire <- as.data.frame(fire_pca$ind$coord)
# Rename columns:
colnames(syntvar_fire) <- c("Fire_dim1",
                            "Fire_dim2")


# 8 - PCA for present land use =================================================


# Compute PCA and scale drivers (to avoid some variables to be dominant because of units):
present_lu_pca <- FactoMineR::PCA(drivers_db[,
                                       c("Present_Perc_croplands_Weighted_Mean",
                                         "Present_Perc_croplands_Weighted_Sd",
                                         "Present_Perc_dense_settlements_Weighted_Mean",
                                         "Present_Perc_dense_settlements_Weighted_Sd",
                                         "Present_Perc_rangelands_Weighted_Mean",
                                         "Present_Perc_rangelands_Weighted_Sd",
                                         "Present_Perc_seminatural_lands_Weighted_Mean",
                                         "Present_Perc_seminatural_lands_Weighted_Sd",
                                         "Present_Perc_villages_Weighted_Mean",
                                         "Present_Perc_villages_Weighted_Sd",
                                         "Present_Perc_wild_lands_Weighted_Mean",
                                         "Present_Perc_wild_lands_Weighted_Sd")],
                            graph = TRUE,
                            scale.unit = TRUE,
                            ncp = 4)

# Get percentage of variance explained by these two four axes: 74% ok
present_lu_pca$eig

# Get variables contribution to the first four axes:
present_lu_pca$var$contrib

# Retrieve grid cells coordinates across 3 axes = new synthetic variables:
syntvar_present_lu <- as.data.frame(present_lu_pca$ind$coord)
# Rename columns:
colnames(syntvar_present_lu) <- c("PresentLandUSe_dim1",
                                  "PresentLandUse_dim2",
                                  "PresentLandUse_dim3",
                                  "PresentLandUse_dim4")


# 9 - PCA for past land use ====================================================


# Compute PCA and scale drivers (to avoid some variables to be dominant because of units):
past_lu_pca <- FactoMineR::PCA(drivers_db[,
                                             c("Past_Perc_croplands_Weighted_Mean",
                                               "Past_Perc_croplands_Weighted_Sd",
                                               "Past_Perc_dense_settlements_Weighted_Mean",
                                               "Past_Perc_dense_settlements_Weighted_Sd",
                                               "Past_Perc_rangelands_Weighted_Mean",
                                               "Past_Perc_rangelands_Weighted_Sd",
                                               "Past_Perc_seminatural_lands_Weighted_Mean",
                                               "Past_Perc_seminatural_lands_Weighted_Sd",
                                               "Past_Perc_villages_Weighted_Mean",
                                               "Past_Perc_villages_Weighted_Sd",
                                               "Past_Perc_wild_lands_Weighted_Mean",
                                               "Past_Perc_wild_lands_Weighted_Sd")],
                                  graph = TRUE,
                                  scale.unit = TRUE,
                                  ncp = 4)

# Get percentage of variance explained by these two four axes: 79% ok
past_lu_pca$eig

# Get variables contribution to the first four axes:
past_lu_pca$var$contrib

# Retrieve grid cells coordinates across 3 axes = new synthetic variables:
syntvar_past_lu <- as.data.frame(past_lu_pca$ind$coord)
# Rename columns:
colnames(syntvar_past_lu) <- c("PastLandUSe_dim1",
                                  "PastLandUse_dim2",
                                  "PastLandUse_dim3",
                                  "PastLandUse_dim4")


# 10 - Build the final db  for SEM (include long/lat also) =======================


# To be able to join by cell id, rownames to column:
syntvar_past_clim_stab <- tibble::rownames_to_column(syntvar_past_clim_stab,
                                                     var = "Idgrid")
syntvar_present_clim_mean <- tibble::rownames_to_column(syntvar_present_clim_mean,
                                                     var = "Idgrid")
syntvar_present_clim_sd <- tibble::rownames_to_column(syntvar_present_clim_sd,
                                                     var = "Idgrid")
syntvar_present_hab_mean <- tibble::rownames_to_column(syntvar_present_hab_mean,
                                                     var = "Idgrid")
syntvar_present_hab_sd <- tibble::rownames_to_column(syntvar_present_hab_sd,
                                                       var = "Idgrid")
syntvar_fire <- tibble::rownames_to_column(syntvar_fire,
                                           var = "Idgrid")
syntvar_present_lu <- tibble::rownames_to_column(syntvar_present_lu,
                                                 var = "Idgrid")
syntvar_past_lu <- tibble::rownames_to_column(syntvar_past_lu,
                                                 var = "Idgrid")

# Join the synthetic variables:
SEM_first_db <- syntvar_past_clim_stab %>%
      dplyr::left_join(syntvar_present_clim_mean,
                       by = "Idgrid") %>%
      dplyr::left_join(syntvar_present_clim_sd,
                       by = "Idgrid") %>%
      dplyr::left_join(syntvar_present_hab_mean,
                       by = "Idgrid") %>%
      dplyr::left_join(syntvar_present_hab_sd,
                       by = "Idgrid") %>%
      dplyr::left_join(syntvar_fire,
                       by = "Idgrid") %>%
      dplyr::left_join(syntvar_past_lu,
                       by = "Idgrid") %>%
      dplyr::left_join(syntvar_present_lu,
                       by = "Idgrid")


# Add human population:
human_pop_db <- drivers_db[, c("Pr_Pop_2020_mean",
                               "Pr_RatePop_2020_mean")]
# Cell id as column in the drivers_db:
human_pop_db <- tibble::rownames_to_column(human_pop_db,
                                         var = "Idgrid")
# Link with synthetic var df:
SEM_second_db <- SEM_first_db %>%
  dplyr::left_join(human_pop_db,
                   by = "Idgrid")

# Add herbivore consumption: TO DO ONCE HERB CONSUMPTION IS USED !!!!!!!!!!!!!!!


# Retreive latitude and longitude from integradiv grid:
# chose the centroid for each grid:
sf::st_crs(grid_50km)$srid
centroid <- sf::st_transform(grid_50km, "EPSG:3035")
lat_long <- sf::st_coordinates(grid_50km, "EPSG:3035")

# Add latitude and longitude:
SEM_final_db <- SEM_second_db



