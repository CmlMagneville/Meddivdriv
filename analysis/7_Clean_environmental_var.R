################################################################################
##
## Script to get a table with all environmental variables
##
## Camille Magneville
##
## 24/04/2024
##
## 7_Clean_environnemental_var.R
##
################################################################################



# Define the pipe symbol so I can use it:
`%>%` <- magrittr::`%>%`


# 1 - Load data ================================================================


# Habitat characteristics:
soil_db <- readRDS("C:/Users/au749321/OneDrive - Aarhus universitet/Postdoc/3_Papers_and_associated_analyses/0_Environmental_var_retrieve/INT_Environmental/database/ENV_Soil.rds")
topo_db <- readRDS("C:/Users/au749321/OneDrive - Aarhus universitet/Postdoc/3_Papers_and_associated_analyses/0_Environmental_var_retrieve/INT_Environmental/database/ENV_topography.rds")

# Present climate:
present_clim_db <- readRDS("C:/Users/au749321/OneDrive - Aarhus universitet/Postdoc/3_Papers_and_associated_analyses/0_Environmental_var_retrieve/INT_Environmental/database/all_present_clim_var.rds")
aridity_db <- readRDS("C:/Users/au749321/OneDrive - Aarhus universitet/Postdoc/3_Papers_and_associated_analyses/0_Environmental_var_retrieve/INT_Environmental/database/AI_current.rds")

# Past climate:
past_velocity_db <- readRDS("C:/Users/au749321/OneDrive - Aarhus universitet/Postdoc/3_Papers_and_associated_analyses/0_Environmental_var_retrieve/INT_Environmental/database/all_past_clim_velocity.rds")
past_clim_db <- readRDS("C:/Users/au749321/OneDrive - Aarhus universitet/Postdoc/3_Papers_and_associated_analyses/0_Environmental_var_retrieve/INT_Environmental/database/all_pastclim_var.rds")

# Disturbance:
fire_db <- readRDS("C:/Users/au749321/OneDrive - Aarhus universitet/Postdoc/3_Papers_and_associated_analyses/0_Environmental_var_retrieve/INT_Environmental/database/Environmental_data_Juan_Fire.rds")
#herb_db # to come once mammal list updated by Manu

# Human:
pop_db <- readRDS("C:/Users/au749321/OneDrive - Aarhus universitet/Postdoc/3_Papers_and_associated_analyses/0_Environmental_var_retrieve/INT_Environmental/database/Environmental_data_Juan_Pop.rds")
pres_past_landuse_crop_db <- readRDS("C:/Users/au749321/OneDrive - Aarhus universitet/Postdoc/3_Papers_and_associated_analyses/0_Environmental_var_retrieve/INT_Environmental/database/Environmental_data_Juan_anthromes_croplands.rds")
pres_past_landuse_denseset_db <- readRDS("C:/Users/au749321/OneDrive - Aarhus universitet/Postdoc/3_Papers_and_associated_analyses/0_Environmental_var_retrieve/INT_Environmental/database/Environmental_data_Juan_anthromes_dense_settlements.rds")
pres_past_landuse_rangeland_db <- readRDS("C:/Users/au749321/OneDrive - Aarhus universitet/Postdoc/3_Papers_and_associated_analyses/0_Environmental_var_retrieve/INT_Environmental/database/Environmental_data_Juan_anthromes_rangelands.rds")
pres_past_landuse_seminat_db <- readRDS("C:/Users/au749321/OneDrive - Aarhus universitet/Postdoc/3_Papers_and_associated_analyses/0_Environmental_var_retrieve/INT_Environmental/database/Environmental_data_Juan_anthromes_seminatural_lands.rds")
pres_past_landuse_village_db <- readRDS("C:/Users/au749321/OneDrive - Aarhus universitet/Postdoc/3_Papers_and_associated_analyses/0_Environmental_var_retrieve/INT_Environmental/database/Environmental_data_Juan_anthromes_villages.rds")
pres_past_landuse_wild_db <- readRDS("C:/Users/au749321/OneDrive - Aarhus universitet/Postdoc/3_Papers_and_associated_analyses/0_Environmental_var_retrieve/INT_Environmental/database/Environmental_data_Juan_anthromes_wildlands.rds")


# 2 - Restrict data at 50*50 and wanted var and save  ==========================


# Habitat characteristics:
# Also rename column so all db have same name:
soil_db <- soil_db %>%
  dplyr::filter(Scale == "50") %>%
  dplyr::filter(Variable_code %in% c("pH", "OC", "VWC", "Depth")) %>%
  dplyr::filter(Metric %in% c("mean", "stdev")) %>%
  dplyr::rename(FinalVariableCode = Variable_code)

topo_db <- topo_db %>%
  dplyr::filter(Scale == "50") %>%
  dplyr::filter(Variable_code == "Elv") %>%
  dplyr::filter(Metric %in% c("mean", "stdev")) %>%
  dplyr::rename(FinalVariableCode = Variable_code)


habitat_var_db <- rbind(soil_db, topo_db)

saveRDS(habitat_var_db, here::here("transformed_data",
                            "env_db",
                            "soil_topo_final_db.rds"))


# Present climate:
present_clim_db <- present_clim_db %>%
  dplyr::filter(Scale == "50") %>%
  dplyr::filter(FinalVariableCode %in% c("MAT", "TAP")) %>%
  dplyr::filter(Metric %in% c("mean", "stdev"))


aridity_db <- aridity_db %>%
  dplyr::filter(Metric %in% c("mean", "stdev")) %>%
  dplyr::rename(FinalVariableCode = Variable_code)


present_clim_var_db <- rbind(present_clim_db, aridity_db)

saveRDS(present_clim_var_db, here::here("transformed_data",
                            "env_db",
                            "present_clim_final_db.rds"))


# Past Climate:
past_velocity_db <- past_velocity_db %>%
  dplyr::filter(Scale == "50") %>%
  dplyr::filter(Metric %in% c("mean.voccMag"))

past_clim_db <- past_clim_db %>%
  dplyr::filter(Scale == "50")

past_clim_all_db <- rbind(past_velocity_db, past_clim_db)

saveRDS(past_clim_all_db, here::here("transformed_data",
                                        "env_db",
                                        "past_veloc_heterog_final_db.rds"))


# Disturbance: # CHANGE MEDIAN TO MEAN when Juan has computed it again (NA?)
fire_db <- fire_db %>%
  dplyr::filter(Scale == "50") %>%
  dplyr::filter(FinalVariableCode %in% c("Pr_FInt_2000-2023",
                                         "Pr_FSurf_2000-2023")) %>%
  dplyr::filter(metric %in% c("median", "stdev", "pixels")) %>%
  dplyr::rename(Metric = metric)

# Add herbivory when Manu has the mammals data ready and rbind the two db -------

saveRDS(fire_db, here::here("transformed_data",
                                     "env_db",
                                     "fire_herb_final_db.rds"))


# Population: # CHANGE MEDIAN TO MEAN when Juan has computed it again (NA?)
# Add the Type column:
pop_db <- pop_db %>%
  dplyr::filter(Scale == "50") %>%
  dplyr::filter(FinalVariableCode %in% c("Pr_Pop_2020",
                                         "Pr_RatePop_2020")) %>%
  dplyr::filter(metric %in% c("median")) %>%
  dplyr::mutate(Type = "Pop") %>%
  dplyr::rename(Metric = metric)


saveRDS(pop_db, here::here("transformed_data",
                            "env_db",
                            "present_pop_final_db.rds"))


# Present and past Land Use

## Reduce to 50*50 scale:
pres_past_landuse_crop_db <- pres_past_landuse_crop_db %>%
  dplyr::filter(Scale == "50")
pres_past_landuse_denseset_db <- pres_past_landuse_denseset_db %>%
  dplyr::filter(Scale == "50")
pres_past_landuse_rangeland_db <- pres_past_landuse_rangeland_db %>%
  dplyr::filter(Scale == "50")
pres_past_landuse_seminat_db <- pres_past_landuse_seminat_db %>%
  dplyr::filter(Scale == "50")
pres_past_landuse_village_db <- pres_past_landuse_village_db %>%
  dplyr::filter(Scale == "50")
pres_past_landuse_wild_db <- pres_past_landuse_wild_db %>%
  dplyr::filter(Scale == "50")



# 3 - Compute variables for past land use =======================================


# Compute weighted mean and sd for each land use, for past (8000BC - 1840)
# ... and present (1850-2023):
# Crops:
metrics_crop_df <- compute.wm.wsd(landuse_df = pres_past_landuse_crop_db)
metrics_present_crop_df <- metrics_crop_df$present_metrics_df
metrics_past_crop_df <- metrics_crop_df$past_metrics_df
# Rangelands:
metrics_rangelands_df <- compute.wm.wsd(landuse_df = pres_past_landuse_rangeland_db)
metrics_present_rangelands_df <- metrics_rangelands_df$present_metrics_df
metrics_past_rangelands_df <- metrics_rangelands_df$past_metrics_df
# Dense settlements:
metrics_denseset_df <- compute.wm.wsd(landuse_df = pres_past_landuse_denseset_db)
metrics_present_denseset_df <- metrics_denseset_df$present_metrics_df
metrics_past_denseset_df <- metrics_denseset_df$past_metrics_df
# Semi natural lands:
metrics_seminat_df <- compute.wm.wsd(landuse_df = pres_past_landuse_seminat_db)
metrics_present_seminat_df <- metrics_seminat_df$present_metrics_df
metrics_past_seminat_df <- metrics_seminat_df$past_metrics_df
# Villages:
metrics_village_df <- compute.wm.wsd(landuse_df = pres_past_landuse_village_db)
metrics_present_village_df <- metrics_village_df$present_metrics_df
metrics_past_village_df <- metrics_village_df$past_metrics_df
# Wild:
metrics_wild_df <- compute.wm.wsd(landuse_df = pres_past_landuse_wild_db)
metrics_present_wild_df <- metrics_wild_df$present_metrics_df
metrics_past_wild_df <- metrics_wild_df$past_metrics_df


# Add a column which refers to the type of land use:
# Croplands:
metrics_past_crop_df <- metrics_past_crop_df %>%
  dplyr::mutate("LandUse" = "croplands")
metrics_present_crop_df <- metrics_present_crop_df %>%
  dplyr::mutate("LandUse" = "croplands")
# Rangelands:
metrics_past_rangelands_df <- metrics_past_rangelands_df %>%
  dplyr::mutate("LandUse" = "rangelands")
metrics_present_rangelands_df <- metrics_present_rangelands_df %>%
  dplyr::mutate("LandUse" = "rangelands")
# Dense settlements:
metrics_past_denseset_df <- metrics_past_denseset_df %>%
  dplyr::mutate("LandUse" = "dense_settlements")
metrics_present_denseset_df <- metrics_present_denseset_df %>%
  dplyr::mutate("LandUse" = "dense_settlements")
# Semi natural:
metrics_past_seminat_df <- metrics_past_seminat_df %>%
  dplyr::mutate("LandUse" = "seminatural_lands")
metrics_present_seminat_df <- metrics_present_seminat_df %>%
  dplyr::mutate("LandUse" = "seminatural_lands")
# Villages :
metrics_past_village_df <- metrics_past_village_df %>%
  dplyr::mutate("LandUse" = "villages")
metrics_present_village_df <- metrics_present_village_df %>%
  dplyr::mutate("LandUse" = "villages")
# Wild lands:
metrics_past_wild_df <- metrics_past_wild_df %>%
  dplyr::mutate("LandUse" = "wild_lands")
metrics_present_wild_df <- metrics_present_wild_df %>%
  dplyr::mutate("LandUse" = "wild_lands")


# Gather data for past and add columns as in the INTEGRADIV db:
past_landuse_df <- rbind(metrics_past_crop_df,
                         metrics_past_rangelands_df,
                         metrics_past_denseset_df,
                         metrics_past_seminat_df,
                         metrics_past_village_df,
                         metrics_past_wild_df)
past_landuse_df <- past_landuse_df %>%
  reshape2::melt(id.vars = c("Idgrid", "LandUse")) %>%
  dplyr::mutate("Type" = "LU") %>%
  dplyr::mutate("Scale" = "50") %>%
  dplyr::mutate("FinalVariableCode" = paste0("Perc", sep = "_",
                                             LandUse)) %>%
  dplyr::rename(Metric = variable) %>%
  dplyr::rename(Value = value) %>%
  dplyr::select(c("Idgrid", "Type", "Scale", "FinalVariableCode",
                  "Metric", "Value"))


# Gather data for present and add columns as in the INTEGRADIV db:
present_landuse_df <- rbind(metrics_present_crop_df,
                         metrics_present_rangelands_df,
                         metrics_present_denseset_df,
                         metrics_present_seminat_df,
                         metrics_present_village_df,
                         metrics_present_wild_df)
present_landuse_df <- present_landuse_df %>%
  reshape2::melt(id.vars = c("Idgrid", "LandUse")) %>%
  dplyr::mutate("Type" = "LU") %>%
  dplyr::mutate("Scale" = "50") %>%
  dplyr::mutate("FinalVariableCode" = paste0("Perc", sep = "_",
                                             LandUse)) %>%
  dplyr::rename(Metric = variable) %>%
  dplyr::rename(Value = value) %>%
  dplyr::select(c("Idgrid", "Type", "Scale", "FinalVariableCode",
                  "Metric", "Value"))

# Save them:
saveRDS(past_landuse_df, here::here("transformed_data",
                            "env_db",
                            "past_landuse_final_db.rds"))
saveRDS(present_landuse_df, here::here("transformed_data",
                            "env_db",
                            "present_landuse_final_db.rds"))

# 4 - Create a db containing all environmental variables =======================


# Load data:
soil_topo_db <- readRDS(here::here("transformed_data",
                                   "env_db",
                                   "soil_topo_final_db.rds"))
disturb_db <- readRDS(here::here("transformed_data",
                                 "env_db",
                                 "fire_herb_final_db.rds"))
past_clim_db <- readRDS(here::here("transformed_data",
                                   "env_db",
                                   "past_veloc_heterog_final_db.rds"))
present_clim_db <- readRDS(here::here("transformed_data",
                                      "env_db",
                                      "present_clim_final_db.rds"))
past_lu_db <- readRDS(here::here("transformed_data",
                                 "env_db",
                                 "past_landuse_final_db.rds"))
present_lu_db <- readRDS(here::here("transformed_data",
                                    "env_db",
                                    "present_landuse_final_db.rds"))
present_pop_db <- readRDS(here::here("transformed_data",
                                     "env_db",
                                     "present_pop_final_db.rds"))

# Long format for all db:
# Soil:
soil_topo_long_db <- soil_topo_db %>%
  dplyr::mutate("Full_metric_nm" = paste0(FinalVariableCode, sep = "_",
                                          Metric)) %>%

  reshape::cast(Idgrid ~ Full_metric_nm,
                value = "Value")
# Disturbances:
disturb_long_db <- disturb_db %>%
  dplyr::mutate("Full_metric_nm" = paste0(FinalVariableCode, sep = "_",
                                          Metric)) %>%

  reshape::cast(Idgrid ~ Full_metric_nm,
                value = "Value") %>%
  # Remove the "-" in the names because otherwise rf doesn't work:
  dplyr::rename(Pr_FInt_2000_2023_median = "Pr_FInt_2000-2023_median") %>%
  dplyr::rename(Pr_FInt_2000_2023_sd = "Pr_FInt_2000-2023_stdev") %>%
  dplyr::rename(Pr_FSurf_2000_2023_pixels = "Pr_FSurf_2000-2023_pixels")

# Past climate:
past_clim_long_db <- past_clim_db %>%
  dplyr::mutate("Full_metric_nm" = paste0("Past", sep = "_",
                                          FinalVariableCode, sep = "_",
                                          Metric)) %>%

  reshape::cast(Idgrid ~ Full_metric_nm,
                value = "Value")
# Present climate:
present_clim_long_db <- present_clim_db %>%
  dplyr::mutate("Full_metric_nm" = paste0("Present", sep = "_",
                                          FinalVariableCode, sep = "_",
                                          Metric)) %>%

  reshape::cast(Idgrid ~ Full_metric_nm,
                value = "Value")
# Past lu:
past_lu_long_db <- past_lu_db %>%
  dplyr::mutate("Full_metric_nm" = paste0("Past", sep = "_",
                                          FinalVariableCode, sep = "_",
                                          Metric)) %>%

  reshape::cast(Idgrid ~ Full_metric_nm,
                value = "Value")
# Present lu:
present_lu_long_db <- present_lu_db %>%
  dplyr::mutate("Full_metric_nm" = paste0("Present", sep = "_",
                                          FinalVariableCode, sep = "_",
                                          Metric)) %>%

  reshape::cast(Idgrid ~ Full_metric_nm,
                value = "Value")
# Present pop:
present_pop_long_db <- present_pop_db %>%
  dplyr::mutate("Full_metric_nm" = paste0(FinalVariableCode, sep = "_",
                                          Metric)) %>%

  reshape::cast(Idgrid ~ Full_metric_nm,
                value = "Value")

# Bind all the drivers together:
envdriv_full_db <- soil_topo_long_db %>%
  dplyr::left_join(disturb_long_db, by = "Idgrid") %>%
  dplyr::left_join(past_clim_long_db, by = "Idgrid") %>%
  dplyr::left_join(present_clim_long_db, by = "Idgrid") %>%
  dplyr::left_join(past_lu_long_db, by = "Idgrid") %>%
  dplyr::left_join(present_lu_long_db, by = "Idgrid") %>%
  dplyr::left_join(present_pop_long_db, by = "Idgrid")

# Save it:
saveRDS(envdriv_full_db, here::here("transformed_data",
                                    "env_db",
                                    "env_drivers_final_db.rds"))


# 5 - Check the environmental database - where are NA and type predictors ======


## Load environmental drivers:
envdriv_full_db <- readRDS(here::here("transformed_data",
                                      "env_db",
                                      "env_drivers_final_db.rds"))

## 1 - Load the integradiv grid so I can localise where the NA are:
# Load grid data:
grid_50km <- sf::st_read(here::here("integradiv_db",
                                    "spgrid_50x50km_EUROMEDIT_EPSG3035.shp"))
# Rename the GRD_ID column as Idgrid:
grid_50km <- dplyr::rename(grid_50km, Idgrid = GRD_ID)


# Check columns with NA:
names(which(colSums(is.na(envdriv_full_db)) > 0))

# Elysa's work:
miss_cell <- envdriv_full_db$Idgrid[which(is.na(envdriv_full_db$Depth_mean) == TRUE)]
miss_cell <- envdriv_full_db$Idgrid[which(is.na(envdriv_full_db$Depth_stdev) == TRUE)]
locate.cells(cell_vect = miss_cell,
             grid = grid_50km)

miss_cell <- envdriv_full_db$Idgrid[which(is.na(envdriv_full_db$VWC_mean) == TRUE)]
miss_cell <- envdriv_full_db$Idgrid[which(is.na(envdriv_full_db$VWC_stdev) == TRUE)]
locate.cells(cell_vect = miss_cell,
             grid = grid_50km)

# My work:
envdriv_full_db$Idgrid[which(is.na(envdriv_full_db$Past_CCVelLGM_mean.voccMag) == TRUE)]
envdriv_full_db$Idgrid[which(is.na(envdriv_full_db$Past_CCVelHolocene_mean.voccMag) == TRUE)]
envdriv_full_db$Idgrid[which(is.na(envdriv_full_db$Past_CCVelShortTerm_mean.voccMag) == TRUE)]
miss_cell <- envdriv_full_db$Idgrid[which(is.na(envdriv_full_db$Past_CCVelYoungerDryas_mean.voccMag) == TRUE)]
locate.cells(cell_vect = miss_cell,
             grid = grid_50km)

envdriv_full_db$Idgrid[which(is.na(envdriv_full_db$Past_TAP_sd) == TRUE)]
envdriv_full_db$Idgrid[which(is.na(envdriv_full_db$Past_MAT_sd) == TRUE)]
envdriv_full_db$Idgrid[which(is.na(envdriv_full_db$Present_MAT_mean) == TRUE)]
envdriv_full_db$Idgrid[which(is.na(envdriv_full_db$Present_MAT_mean) == TRUE)]
envdriv_full_db$Idgrid[which(is.na(envdriv_full_db$Present_MAT_stdev) == TRUE)]
miss_cell <- envdriv_full_db$Idgrid[which(is.na(envdriv_full_db$Present_TAP_stdev) == TRUE)]
locate.cells(cell_vect = miss_cell,
             grid = grid_50km)

# Manuel's work:
envdriv_full_db$Idgrid[which(is.na(envdriv_full_db$Present_AI_mean) == TRUE)]
miss_cell <- envdriv_full_db$Idgrid[which(is.na(envdriv_full_db$Present_AI_stdev) == TRUE)]
locate.cells(cell_vect = miss_cell,
             grid = grid_50km)

# Juan work
envdriv_full_db$Idgrid[which(is.na(envdriv_full_db$Pr_Pop_2020_median) == TRUE)]
miss_cell <- envdriv_full_db$Idgrid[which(is.na(envdriv_full_db$Pr_RatePop_2020_median) == TRUE)]
locate.cells(cell_vect = miss_cell,
             grid = grid_50km)
miss_cell <- envdriv_full_db$Idgrid[which(is.na(envdriv_full_db$"Pr_FInt_2000-2023_median") == TRUE)]
locate.cells(cell_vect = miss_cell,
             grid = grid_50km)


## 2 - Check types of variables
str(envdriv_full_db)

# Change Populations to numeric:
envdriv_full_db$Pr_Pop_2020_median <- as.numeric(envdriv_full_db$Pr_Pop_2020_median)
envdriv_full_db$Pr_RatePop_2020_median <- as.numeric(envdriv_full_db$Pr_RatePop_2020_median)

# Change Fire to numeric:
envdriv_full_db$Pr_FInt_2000_2023_median <- as.numeric(envdriv_full_db$Pr_FInt_2000_2023_median)
envdriv_full_db$Pr_FInt_2000_2023_sd <- as.numeric(envdriv_full_db$Pr_FInt_2000_2023_sd)
envdriv_full_db$Pr_FSurf_2000_2023_pixels <- as.numeric(envdriv_full_db$Pr_FSurf_2000_2023_pixels)


# 6 - Create the final environmental db - only cells with occ data and pred values =============


# NOTE: If joining drivers db and diversity db, the final db would have
# ... 671 rows (grid cells) but for some of these grid cells, we don't have
# ... occurrence data for now : (birds 664 grid cells, reptiles 624 grid cells,
# ... trees 667 grid cells)
# ... SO: 1/ Only keep the grid cells for which I have occ information for all taxa
# ... 2/ Remove environmental values for rows that are not studied anymore


# Load species occurrences:
birds_occ_df <- readRDS(here::here("transformed_data",
                                   "sp_asb_50km_BIRDS.rds"))
trees_occ_df <- readRDS(here::here("transformed_data",
                                   "sp_asb_50km_TREES.rds"))
reptiles_occ_df <- readRDS(here::here("transformed_data",
                                      "sp_asb_50km_REPTILES.rds"))

## DIVERSITY db

# Get the names of the Idgrid to keep (diversity data for all taxa):
cells_ok_birds <- rownames(birds_occ_df)
cells_ok_reptiles <- rownames(reptiles_occ_df)
cells_ok_trees <- rownames(trees_occ_df)
cells_to_keep <- intersect(intersect(cells_ok_birds,
                                     cells_ok_reptiles),
                           cells_ok_trees)
locate.cells(cell_vect = cells_to_keep,
             grid = grid_50km)

# Only keep these cells in the environmental drivers db:
envdriv_full_db <- envdriv_full_db %>%
  dplyr::filter(Idgrid %in% cells_to_keep)


## DRIVERS db

# Then, remove cells which have NA for at last one predictor:
noNA_envdriv_full_db <- na.omit(envdriv_full_db)

## Save this final environmental db:
saveRDS(noNA_envdriv_full_db,
        here::here("transformed_data", "env_db",
                   "env_drivers_final_noNA_db.rds"))
