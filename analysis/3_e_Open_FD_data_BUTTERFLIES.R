################################################################################
##
## Script open the traits from the integradiv database and check for
## ... potential issues - for BUTTERFLIES
##
## Camille Magneville
##
## 09/2024
##
## 3_a_Open_FD_data_BUTTERFLIES.R
##
################################################################################


# Define the pipe symbol so I can use it:
`%>%` <- magrittr::`%>%`


# 1 - Load INTEGRADIV occurrence data ================================


INTEGRADIV_butterflies_occ_df <- readRDS(here::here("transformed_data",
                                                "sp_asb_50km_BUTTERFLIES.rds"))


# 2 - Load and clean traits data ================================


# Load traits data:
INTEGRADIV_traits <- read.csv(file = here::here("integradiv_db",
                                                "INTEGRADIV_traits_v4.csv"))

# Only keep TREE data:
butterflies_traits <- dplyr::filter(INTEGRADIV_traits,
                                Taxon == "Butterflies")

# Check if all species in the occurrence df are in the traits db: YES :)
setdiff(colnames(INTEGRADIV_butterflies_occ_df),
        unique(butterflies_traits$Species))

# Check traits db only contains species in the occurrence df: YES :)
setdiff(unique(butterflies_traits$Species),
        colnames(INTEGRADIV_butterflies_occ_df))

# Keep only fuzzy traits (formatted afterwards):
butterflies_fuzzy_traits <- butterflies_traits %>%
  dplyr::filter(Trait %in% c("BaskingSite",
                             "EggLayingLoc",
                             "HostPlantType",
                             "MateLocatingLoc",
                             "OverwinteringLoc",
                             "OverwinteringStage",
                             "PupalLoc"))

# Remove fuzzy traits (and circular one I don't use):
butterflies_no_fuzzy_traits <- butterflies_traits %>%
  dplyr::filter(!Trait %in% c("BaskingSite",
                             "EggLayingLoc",
                             "HostPlantType",
                             "MateLocatingLoc",
                             "OverwinteringLoc",
                             "OverwinteringStage",
                             "PupalLoc",
                             "FlyingPeriodPosition"))

# Put it in the right format (species = rows, traits = columns)
butterflies_no_fuzzy_traits_df <- butterflies_no_fuzzy_traits %>%
  dplyr::select(c("Species", "Trait", "Value")) %>%
  tidyr::pivot_wider(names_from = Trait, values_from = Value)

# 2 - a - Clean fuzzy traits ===================================================

# Put it in the right format - fuzzy traits - BaskingSite:
## Keep only interesting columns:
butterflies_fuzzy_basking_df <- butterflies_fuzzy_traits %>%
  dplyr::filter(Trait == "BaskingSite") %>%
  dplyr::select(-c("Taxon", "Trait", "Source", "Origin"))
## Add a column to be able to get 0/1 in the new df:
butterflies_fuzzy_basking_df$Val <- 1
## Rename categories names to get trait name it them:
butterflies_fuzzy_basking_df$Value <- paste0("basking",
                                             sep = "_",
                                             butterflies_fuzzy_basking_df$Value)
## Format new df:
butterflies_fuzzy_basking_df <- butterflies_fuzzy_basking_df %>%
  tidyr::pivot_wider(names_from = Value, values_from = Val)
## Add 0 where NA:
butterflies_fuzzy_basking_df[is.na(butterflies_fuzzy_basking_df)] <- 0
butterflies_fuzzy_basking_df[, -1] <- apply(butterflies_fuzzy_basking_df[, -1], 2, as.factor)

# Put it in the right format - fuzzy traits - EggLayingLoc:
## Keep only interesting columns:
butterflies_fuzzy_egglaying_df <- butterflies_fuzzy_traits %>%
  dplyr::filter(Trait == "EggLayingLoc") %>%
  dplyr::select(-c("Taxon", "Trait", "Source", "Origin"))
## Add a column to be able to get 0/1 in the new df:
butterflies_fuzzy_egglaying_df$Val <- 1
## Rename categories names to get trait name it them:
butterflies_fuzzy_egglaying_df$Value <- paste0("egglaying",
                                             sep = "_",
                                             butterflies_fuzzy_egglaying_df$Value)
## Format new df:
butterflies_fuzzy_egglaying_df <- butterflies_fuzzy_egglaying_df %>%
  tidyr::pivot_wider(names_from = Value, values_from = Val)
## Add 0 where NA:
butterflies_fuzzy_egglaying_df[is.na(butterflies_fuzzy_egglaying_df)] <- 0
butterflies_fuzzy_egglaying_df[, -1] <- apply(butterflies_fuzzy_egglaying_df[, -1], 2, as.factor)

# Put it in the right format - fuzzy traits - HostPlantType:
## Keep only interesting columns:
butterflies_fuzzy_hostplant_df <- butterflies_fuzzy_traits %>%
  dplyr::filter(Trait == "HostPlantType") %>%
  dplyr::select(-c("Taxon", "Trait", "Source", "Origin"))
## Add a column to be able to get 0/1 in the new df:
butterflies_fuzzy_hostplant_df$Val <- 1
## Rename categories names to get trait name it them:
butterflies_fuzzy_hostplant_df$Value <- paste0("hostplant",
                                             sep = "_",
                                             butterflies_fuzzy_hostplant_df$Value)
## Format new df:
butterflies_fuzzy_hostplant_df <- butterflies_fuzzy_hostplant_df %>%
  tidyr::pivot_wider(names_from = Value, values_from = Val)
## Add 0 where NA:
butterflies_fuzzy_hostplant_df[is.na(butterflies_fuzzy_hostplant_df)] <- 0
butterflies_fuzzy_hostplant_df[, -1] <- apply(butterflies_fuzzy_hostplant_df[, -1], 2, as.factor)

# Put it in the right format - fuzzy traits - MateLocatingLoc:
## Keep only interesting columns:
butterflies_fuzzy_mateloc_df <- butterflies_fuzzy_traits %>%
  dplyr::filter(Trait == "MateLocatingLoc") %>%
  dplyr::select(-c("Taxon", "Trait", "Source", "Origin"))
## Add a column to be able to get 0/1 in the new df:
butterflies_fuzzy_mateloc_df$Val <- 1
## Rename categories names to get trait name it them:
butterflies_fuzzy_mateloc_df$Value <- paste0("mateloc",
                                             sep = "_",
                                             butterflies_fuzzy_mateloc_df$Value)
## Format new df:
butterflies_fuzzy_mateloc_df <- butterflies_fuzzy_mateloc_df %>%
  tidyr::pivot_wider(names_from = Value, values_from = Val)
## Add 0 where NA:
butterflies_fuzzy_mateloc_df[is.na(butterflies_fuzzy_mateloc_df)] <- 0
butterflies_fuzzy_mateloc_df[, -1] <- apply(butterflies_fuzzy_mateloc_df[, -1], 2, as.factor)

# Put it in the right format - fuzzy traits - OverwinteringLoc:
## Keep only interesting columns:
butterflies_fuzzy_overwintloc_df <- butterflies_fuzzy_traits %>%
  dplyr::filter(Trait == "OverwinteringLoc") %>%
  dplyr::select(-c("Taxon", "Trait", "Source", "Origin"))
## Add a column to be able to get 0/1 in the new df:
butterflies_fuzzy_overwintloc_df$Val <- 1
## Rename categories names to get trait name it them:
butterflies_fuzzy_overwintloc_df$Value <- paste0("overwintloc",
                                             sep = "_",
                                             butterflies_fuzzy_overwintloc_df$Value)
## Format new df:
butterflies_fuzzy_overwintloc_df <- butterflies_fuzzy_overwintloc_df %>%
  tidyr::pivot_wider(names_from = Value, values_from = Val)
## Add 0 where NA:
butterflies_fuzzy_overwintloc_df[is.na(butterflies_fuzzy_overwintloc_df)] <- 0
butterflies_fuzzy_overwintloc_df[, -1] <- apply(butterflies_fuzzy_overwintloc_df[, -1], 2, as.factor)

# Put it in the right format - fuzzy traits - OverwinteringStage:
## Keep only interesting columns:
butterflies_fuzzy_overwintstage_df <- butterflies_fuzzy_traits %>%
  dplyr::filter(Trait == "OverwinteringStage") %>%
  dplyr::select(-c("Taxon", "Trait", "Source", "Origin"))
## Add a column to be able to get 0/1 in the new df:
butterflies_fuzzy_overwintstage_df$Val <- 1
## Rename categories names to get trait name it them:
butterflies_fuzzy_overwintstage_df$Value <- paste0("overwintstage",
                                                   sep = "_",
                                                   butterflies_fuzzy_overwintstage_df$Value)
## Format new df:
butterflies_fuzzy_overwintstage_df <- butterflies_fuzzy_overwintstage_df %>%
  tidyr::pivot_wider(names_from = Value, values_from = Val)
## Add 0 where NA:
butterflies_fuzzy_overwintstage_df[is.na(butterflies_fuzzy_overwintstage_df)] <- 0
butterflies_fuzzy_overwintstage_df[, -1] <- apply(butterflies_fuzzy_overwintstage_df[, -1], 2, as.factor)

# Put it in the right format - fuzzy traits - PupalLoc:
## Keep only interesting columns:
butterflies_fuzzy_pupalloc_df <- butterflies_fuzzy_traits %>%
  dplyr::filter(Trait == "PupalLoc") %>%
  dplyr::select(-c("Taxon", "Trait", "Source", "Origin"))
## Remove duplicated rows (pb while building the db):
butterflies_fuzzy_pupalloc_df <- dplyr::distinct(butterflies_fuzzy_pupalloc_df)
## Add a column to be able to get 0/1 in the new df:
butterflies_fuzzy_pupalloc_df$Val <- 1
## Rename categories names to get trait name it them:
butterflies_fuzzy_pupalloc_df$Value <- paste0("pupalloc",
                                             sep = "_",
                                             butterflies_fuzzy_pupalloc_df$Value)
## Format new df:
butterflies_fuzzy_pupalloc_df <- butterflies_fuzzy_pupalloc_df %>%
  tidyr::pivot_wider(names_from = Value, values_from = Val)
## Add 0 where NA:
butterflies_fuzzy_pupalloc_df[is.na(butterflies_fuzzy_pupalloc_df)] <- 0
butterflies_fuzzy_pupalloc_df[, -1] <- apply(butterflies_fuzzy_pupalloc_df[, -1], 2, as.factor)

# Link fuzzy traits df:
butterflies_fuzzy_all_df <- butterflies_fuzzy_basking_df %>%
  dplyr::full_join(butterflies_fuzzy_egglaying_df,
                   by = "Species") %>%
  dplyr::full_join(butterflies_fuzzy_hostplant_df,
                   by = "Species") %>%
  dplyr::full_join(butterflies_fuzzy_mateloc_df,
                   by = "Species") %>%
  dplyr::full_join(butterflies_fuzzy_overwintloc_df,
                   by = "Species") %>%
  dplyr::full_join(butterflies_fuzzy_overwintstage_df,
                   by = "Species") %>%
  dplyr::full_join(butterflies_fuzzy_pupalloc_df,
                   by = "Species")
# Make sure they are seen as numerical:
butterflies_fuzzy_all_df[, -1] <- apply(butterflies_fuzzy_all_df[, -1],
                                        2,
                                        as.numeric)

# 2 - b - Clean non fuzzy traits ===============================================

# Format the right class for the traits - non fuzzy:
butterflies_no_fuzzy_traits_df$EggLayingType <- ordered(butterflies_no_fuzzy_traits_df$EggLayingType,
                                                       levels = c("1_single_egg",
                                                                  "2_intermediate",
                                                                  "3_small_batch",
                                                                  "4_intermediate",
                                                                  "5_large_batch"))
butterflies_no_fuzzy_traits_df$FlyingPeriodBreadth <- as.numeric(butterflies_no_fuzzy_traits_df$FlyingPeriodBreadth)
butterflies_no_fuzzy_traits_df$WingIndex <- as.numeric(butterflies_no_fuzzy_traits_df$WingIndex)
butterflies_no_fuzzy_traits_df$WingSpan <- as.numeric(butterflies_no_fuzzy_traits_df$WingSpan)
# For VoltinismMax, need to convert 1,5 to 1.5:
butterflies_no_fuzzy_traits_df$VoltinismMax[which(butterflies_no_fuzzy_traits_df$VoltinismMax == "1,5")] <- "1.5"
butterflies_no_fuzzy_traits_df$VoltinismMax <- as.numeric(butterflies_no_fuzzy_traits_df$VoltinismMax)
# For HostPlantSpec, show convert NULL into NA first:
butterflies_no_fuzzy_traits_df$HostPlantSpec[which(butterflies_no_fuzzy_traits_df$HostPlantSpec == "NULL")] <- NA
butterflies_no_fuzzy_traits_df$HostPlantSpec <- as.numeric(butterflies_no_fuzzy_traits_df$HostPlantSpec)

# 2 - c - Link fuzzy and non fuzzy traits ======================================

# Link the two fuzzy and non-fuzzy:
butterflies_traits_df <- dplyr::left_join(butterflies_no_fuzzy_traits_df,
                                          butterflies_fuzzy_all_df,
                                          by = "Species")

# Save the traits:
saveRDS(butterflies_traits_df, file = here::here("transformed_data",
                                                 "raw_traits_BUTTERFLIES.rds"))


# 3 - Check for missing data ===================================================


# Change first column name for funbiogeo use:
funbiogeo_df <- dplyr::rename(butterflies_traits_df, species = Species)

# Get the percentage of completedness per traits:
funbiogeo::fb_plot_number_species_by_trait(funbiogeo_df)

# Get the percentage of species with for than 1, 2, 3 etc traits:
funbiogeo::fb_plot_number_traits_by_species(funbiogeo_df)






