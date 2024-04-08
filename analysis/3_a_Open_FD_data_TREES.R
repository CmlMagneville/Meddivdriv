################################################################################
##
## Script open the traits from the integradiv database and check for
## ... potential issues - for TREES
##
## Camille Magneville
##
## 08/04/2024
##
## 3_a_Open_FD_data_TREES.R
##
################################################################################


# Define the pipe symbol so I can use it:
`%>%` <- magrittr::`%>%`


# 1 - Load INTEGRADIV occurrence data ================================


INTEGRADIV_trees_occ_df <- readRDS(here::here("transformed_data",
                                              "sp_asb_50km_TREES.rds"))


# 2 - Load and clean traits data ================================


# Load phylogeny data:
INTEGRADIV_traits <- read.csv(file = here::here("integradiv_db",
                                                "INTEGRADIV_traits_v3.csv"))

# Only keep TREE data:
trees_traits <- dplyr::filter(INTEGRADIV_traits,
                              Taxon == "Trees")

# Check if all species in the occurrence df are in the traits db: YES :)
setdiff(colnames(INTEGRADIV_trees_occ_df),
        unique(trees_traits$Species))

# Check traits db only contains species in the occurrence df: NO :/
setdiff(unique(trees_traits$Species),
        colnames(INTEGRADIV_trees_occ_df))

# Remove the 2 species present in the traits db but not in our occ data:
trees_traits_corrected <- dplyr::filter(trees_traits,
                                        ! Species %in% c( "Pyrus syriaca",
                                                          "Tamarix passerinoides"))

# Check again: Ok :)
setdiff(unique(trees_traits_corrected$Species),
        colnames(INTEGRADIV_trees_occ_df))


# Put it in the right format (species = rows, traits = columns)
trees_traits_df <- trees_traits_corrected %>%
  dplyr::select(c("Species", "Trait", "Value")) %>%
  tidyr::pivot_wider(names_from = Trait, values_from = Value)


# All traits of this dataframe will be used for tha analysis, keep them all :)


# Save the traits:
saveRDS(trees_traits_df, file = here::here("transformed_data",
                                           "raw_traits_TREES.rds"))


# 3 - Check for missing data ===================================================


# Change first column name for funbiogeo use:
funbiogeo_df <- dplyr::rename(trees_traits_df, species = Species)

# Get the percentage of completedness per traits:
funbiogeo::fb_plot_number_species_by_trait(funbiogeo_df)

# Get the percentage of species with for than 1, 2, 3 etc traits:
funbiogeo::fb_plot_number_traits_by_species(funbiogeo_df)






