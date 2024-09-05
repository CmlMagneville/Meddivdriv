################################################################################
##
## Script open the traits from the integradiv database and check for
## ... potential issues - for REPTILES
##
## Camille Magneville
##
## 08/04/2024 - 09/2024
##
## 3_a_Open_FD_data_REPTILES.R
##
################################################################################


# Define the pipe symbol so I can use it:
`%>%` <- magrittr::`%>%`


# 1 - Load INTEGRADIV occurrence data ================================


INTEGRADIV_reptiles_occ_df <- readRDS(here::here("transformed_data",
                                              "sp_asb_50km_REPTILES.rds"))


# 2 - Load and clean traits data ================================


# Load traits data:
# INTEGRADIV_traits <- read.csv(file = here::here("integradiv_db",
#                                                 "INTEGRADIV_traits_v3.csv"))

reptiles_traits <- read.csv(file = here::here("integradiv_db",
                                                "reptiles_traits_170424.csv"))

# Only keep REPTILES data:
reptiles_traits <- dplyr::filter(INTEGRADIV_traits,
                              Taxon == "Reptiles")

# Check if all species in the occurrence df are in the traits db: YES :)
setdiff(colnames(INTEGRADIV_reptiles_occ_df),
        unique(reptiles_traits$Species))

# Check traits db only contains species in the occurrence df: YES :)
setdiff(unique(reptiles_traits$Species),
        colnames(INTEGRADIV_reptiles_occ_df))

# Remove fuzzy traits (formatted afterwards):
reptiles_fuzzy_traits <- reptiles_traits %>%
  dplyr::filter(Trait == "Substrate")

reptiles_no_fuzzy_traits <- reptiles_traits %>%
  dplyr::filter(Trait != "Substrate")

# Put it in the right format (species = rows, traits = columns)
reptiles_no_fuzzy_traits_df <- reptiles_no_fuzzy_traits %>%
  dplyr::select(c("Species", "Trait", "Value")) %>%
  tidyr::pivot_wider(names_from = Trait, values_from = Value)

# Put it in the right format - fuzzy traits - Substrate:
## Keep only interesting columns:
reptiles_fuzzy_substrate_df <- reptiles_fuzzy_traits %>%
  dplyr::select(-c("Taxon", "Trait", "Source", "Origin"))
## Add a column to be able to get 0/1 in the new df:
reptiles_fuzzy_substrate_df$Val <- 1
## Format new df:
reptiles_fuzzy_substrate_df <- reptiles_fuzzy_substrate_df %>%
  tidyr::pivot_wider(names_from = Value, values_from = Val)
## Add 0 where NA:
reptiles_fuzzy_substrate_df[is.na(reptiles_fuzzy_substrate_df)] <- 0
reptiles_fuzzy_substrate_df[, -1] <- apply(reptiles_fuzzy_substrate_df[, -1], 2, as.factor)


# Format the right class for the traits - non fuzzy:
reptiles_no_fuzzy_traits_df$ActivityTime <- ordered(reptiles_no_fuzzy_traits_df$ActivityTime,
                                           levels = c("1_nocturnal",
                                                      "2_intermediate",
                                                      "3_diurnal"))
reptiles_no_fuzzy_traits_df$SVLMax <- as.numeric(reptiles_no_fuzzy_traits_df$SVLMax)
reptiles_no_fuzzy_traits_df$ReproPerYear <- round(as.numeric(reptiles_no_fuzzy_traits_df$ReproPerYear), 2)
reptiles_no_fuzzy_traits_df$OffspringPerRepro <- round(as.numeric(reptiles_no_fuzzy_traits_df$OffspringPerRepro), 2)
reptiles_no_fuzzy_traits_df$TrophicLevel <- ordered(reptiles_no_fuzzy_traits_df$TrophicLevel,
                                           levels = c("1_herbivorous",
                                                      "2_omnivorous",
                                                      "3_carnivorous"))
reptiles_no_fuzzy_traits_df$LimbDev <- ordered(reptiles_no_fuzzy_traits_df$LimbDev,
                                      levels = c("1_limbless",
                                                 "2_reduced",
                                                 "3_developed"))
reptiles_no_fuzzy_traits_df$ActivitySeasonLength <- as.numeric(reptiles_no_fuzzy_traits_df$ActivitySeasonLength)
reptiles_no_fuzzy_traits_df$BodyTemperature <- as.numeric(reptiles_no_fuzzy_traits_df$BodyTemperature)
reptiles_no_fuzzy_traits_df$FirstBreedingAge <- round(as.numeric(reptiles_no_fuzzy_traits_df$FirstBreedingAge), 2)
reptiles_no_fuzzy_traits_df$LongevityMax <- round(as.numeric(reptiles_no_fuzzy_traits_df$LongevityMax), 2)


# Link the two dataframes:
reptiles_traits_df <- dplyr::left_join(reptiles_no_fuzzy_traits_df,
                                       reptiles_fuzzy_substrate_df,
                                       by = "Species")

# Save the traits:
saveRDS(reptiles_traits_df, file = here::here("transformed_data",
                                           "raw_traits_REPTILES.rds"))


# 3 - Check for missing data ===================================================


# Change first column name for funbiogeo use:
funbiogeo_df <- dplyr::rename(reptiles_traits_df, species = Species)

# Get the percentage of completedness per traits:
funbiogeo::fb_plot_number_species_by_trait(funbiogeo_df)

# Get the percentage of species with for than 1, 2, 3 etc traits:
funbiogeo::fb_plot_number_traits_by_species(funbiogeo_df)

