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
  dplyr::filter(Variable_code %in% c("pH", "OC", "VWC")) %>%
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

## Divide data into past and present for each land use type and compute
## ... weighted mean and sd:





# Clean the R environment because big databases take a lot of space:
rm(list = ls(all.names = TRUE), envir = .GlobalEnv)


# 3 - Compute variables for past land use =======================================










