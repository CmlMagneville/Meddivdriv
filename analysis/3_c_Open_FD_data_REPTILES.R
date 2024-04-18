################################################################################
##
## Script open the traits from the integradiv database and check for
## ... potential issues - for REPTILES
##
## Camille Magneville
##
## 08/04/2024
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

# Only keep TREE data:
# reptiles_traits <- dplyr::filter(INTEGRADIV_traits,
#                               Taxon == "Reptiles")

# Check if all species in the occurrence df are in the traits db: NO :/
setdiff(colnames(INTEGRADIV_reptiles_occ_df),
        unique(reptiles_traits$Species))

# Check traits db only contains species in the occurrence df: NO :/
setdiff(unique(reptiles_traits$Species),
        colnames(INTEGRADIV_reptiles_occ_df))

# The species is the same - homonym - so correct it:
reptiles_traits_corrected <- reptiles_traits
reptiles_traits_corrected[which(reptiles_traits_corrected$Species == "Xerotyphlops vermicularis"),
                          "Species"] <- "Typhlops vermicularis"


# Put it in the right format (species = rows, traits = columns)
reptiles_traits_df <- reptiles_traits_corrected %>%
  dplyr::select(c("Species", "Trait", "Value")) %>%
  tidyr::pivot_wider(names_from = Trait, values_from = Value)


# Select the traits that will be used:
reptiles_traits_df <- dplyr::select(reptiles_traits_df,
                                 c("Species",
                                   "Active_period",
                                   "BodyMass_max_g",
                                   "Brood_per_year",
                                   "Clutch_mean", "Diet", "Limbs", "SVL_mm_max",
                                   "Substrate", "Activity_season_m",
                                   "BodyTemp_mean", "First_breeding_m",
                                   "Longevity_y"))

# Format the right class for the traits:
reptiles_traits_df$Active_period <- as.factor(reptiles_traits_df$Active_period)
reptiles_traits_df$BodyMass_max_g <- as.numeric(reptiles_traits_df$BodyMass_max_g)
reptiles_traits_df$Brood_per_year <- as.numeric(reptiles_traits_df$Brood_per_year)
reptiles_traits_df$Clutch_mean <- as.numeric(reptiles_traits_df$Clutch_mean)
reptiles_traits_df$Diet <- as.factor(reptiles_traits_df$Diet)
reptiles_traits_df$Limbs <- as.factor(reptiles_traits_df$Limbs)
reptiles_traits_df$SVL_mm_max <- as.numeric(reptiles_traits_df$SVL_mm_max)
reptiles_traits_df$Substrate <- as.factor(reptiles_traits_df$Substrate)
reptiles_traits_df$Activity_season_m <- as.numeric(reptiles_traits_df$Activity_season_m)
reptiles_traits_df$BodyTemp_mean <- as.numeric(reptiles_traits_df$BodyTemp_mean)
reptiles_traits_df$First_breeding_m <- as.numeric(reptiles_traits_df$First_breeding_m)
reptiles_traits_df$Longevity_y <- as.numeric(reptiles_traits_df$Longevity_y)

# Correct typo Carnivorous with a *:
reptiles_traits_df$Diet[which(reptiles_traits_df$Diet == "*Carnivorous")] <- "Carnivorous"


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

