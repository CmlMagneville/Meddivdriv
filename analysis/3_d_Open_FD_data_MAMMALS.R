################################################################################
##
## Script open the traits from the integradiv database and check for
## ... potential issues - for MAMMALS
##
## Camille Magneville
##
## 09/2024
##
## 3_a_Open_FD_data_MAMMALS.R
##
################################################################################


# Define the pipe symbol so I can use it:
`%>%` <- magrittr::`%>%`


# 1 - Load INTEGRADIV occurrence data ================================


INTEGRADIV_mammals_occ_df <- readRDS(here::here("transformed_data",
                                              "sp_asb_50km_MAMMALS.rds"))


# 2 - Load and clean traits data ================================


# Load traits data:
INTEGRADIV_traits <- read.csv(file = here::here("integradiv_db",
                                                "INTEGRADIV_traits_v4.csv"))

# Only keep TREE data:
mammals_traits <- dplyr::filter(INTEGRADIV_traits,
                              Taxon == "Mammals")

# Check if all species in the occurrence df are in the traits db: NO :/
setdiff(colnames(INTEGRADIV_mammals_occ_df),
        unique(mammals_traits$Species))

# Remove this species becaue it has no value for any trait (so can't impute them):
mammals_traits_corrected <- dplyr::filter(mammals_traits,
                                         ! Species %in% c( "Arvicola italicus"))

# Check traits db only contains species in the occurrence df: YES :)
setdiff(unique(mammals_traits$Species),
        colnames(INTEGRADIV_mammals_occ_df))

# Put it in the right format (species = rows, traits = columns)
mammals_traits_df <- mammals_traits_corrected %>%
  dplyr::select(c("Species", "Trait", "Value")) %>%
  tidyr::pivot_wider(names_from = Trait, values_from = Value)

# Traits with the right format:
mammals_traits_df$ActivityTime <- ordered(mammals_traits_df$ActivityTime,
                                          levels = c("1_nocturnal",
                                                     "2_intermediate",
                                                     "3_diurnal"))
mammals_traits_df$BodyMass <- as.numeric(mammals_traits_df$BodyMass)
mammals_traits_df$Dispersal <- as.numeric(mammals_traits_df$Dispersal)
mammals_traits_df$ForagingStratum <- as.factor(mammals_traits_df$ForagingStratum)
mammals_traits_df$GenerationLength <- as.numeric(mammals_traits_df$GenerationLength)
mammals_traits_df$Hibernation <- as.factor(mammals_traits_df$Hibernation)
mammals_traits_df$ReproPerYear <- as.numeric(mammals_traits_df$ReproPerYear)
mammals_traits_df$OffspringPerRepro <- as.numeric(mammals_traits_df$OffspringPerRepro)
mammals_traits_df$LongevityMax <- as.numeric(mammals_traits_df$LongevityMax)
mammals_traits_df$TrophicLevel <- ordered(mammals_traits_df$TrophicLevel,
                                          levels = c("1_herbivorous",
                                                     "2_omnivorous",
                                                     "3_carnivorous"))
mammals_traits_df$WeaningAge <- as.numeric(mammals_traits_df$WeaningAge)

# Save the traits:
saveRDS(mammals_traits_df, file = here::here("transformed_data",
                                           "raw_traits_MAMMALS.rds"))


# 3 - Check for missing data ===================================================


# Change first column name for funbiogeo use:
funbiogeo_df <- dplyr::rename(mammals_traits_df, species = Species)

# Get the percentage of completedness per traits:
funbiogeo::fb_plot_number_species_by_trait(funbiogeo_df)

# Get the percentage of species with for than 1, 2, 3 etc traits:
funbiogeo::fb_plot_number_traits_by_species(funbiogeo_df)






