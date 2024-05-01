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
soil_db <- soil_db %>%
  dplyr::filter(Scale == "50") %>%
  dplyr::filter(Variable_code %in% c("pH", "OC", "VWC", "Depth")) %>%
  dplyr::filter(Metric %in% c("mean", "stdev"))

topo_db <- topo_db %>%
  dplyr::filter(Scale == "50") %>%
  dplyr::filter(Variable_code == "Elv") %>%
  dplyr::filter(Metric %in% c("mean", "stdev"))

habitat_var_db <- rbind(soil_db, topo_db)

saveRDS(habitat_var_db, here::here("transformed_data",
                            "env_db",
                            "soil_topo_final_db.rds"))


# Present climate:
present_clim_db <- present_clim_db %>%
  dplyr::filter(Scale == "50") %>%
  dplyr::filter(FinalVariableCode %in% c("MAT", "TAP")) %>%
  dplyr::filter(Metric %in% c("mean", "stdev")) %>%
  dplyr::rename(Variable_code = "FinalVariableCode")

aridity_db <- aridity_db %>%
  dplyr::filter(Metric %in% c("mean", "stdev"))

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
  dplyr::filter(metric %in% c("median", "stdev"))

# Add herbivory when Manu has the mammals data ready and rbind the two db -------

saveRDS(fire_db, here::here("transformed_data",
                                     "env_db",
                                     "fire_herb_final_db.rds"))


# Population: # CHANGE MEDIAN TO MEAN when Juan has computed it again (NA?)
pop_db <- pop_db %>%
  dplyr::filter(Scale == "50") %>%
  dplyr::filter(FinalVariableCode %in% c("Pr_Pop_2020",
                                         "Pr_RatePop_2020")) %>%
  dplyr::filter(metric %in% c("median"))

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

