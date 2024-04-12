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


# Load phylogeny data:
INTEGRADIV_traits <- read.csv(file = here::here("integradiv_db",
                                                "INTEGRADIV_traits_v3.csv"))

# Only keep TREE data:
reptiles_traits <- dplyr::filter(INTEGRADIV_traits,
                              Taxon == "Reptiles")

# Check if all species in the occurrence df are in the traits db: NO :/
setdiff(colnames(INTEGRADIV_reptiles_occ_df),
        unique(reptiles_traits$Species))

# Check traits db only contains species in the occurrence df: YES :)
setdiff(unique(reptiles_traits$Species),
        colnames(INTEGRADIV_reptiles_occ_df))


# Put it in the right format (species = rows, traits = columns)
reptiles_traits_df <- reptiles_traits %>%
  dplyr::select(c("Species", "Trait", "Value")) %>%
  tidyr::pivot_wider(names_from = Trait, values_from = Value)


# Select the traits that will be used:
reptiles_traits_df <- dplyr::select(reptiles_traits_df,
                                 c("Species",
                                   "adult_svl_cm",
                                   "litter_or_clutch_size_n"))



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

