################################################################################
##
## Script open the traits from the integradiv database and check for
## ... potential issues - for BIRDS
##
## Camille Magneville
##
## 08/04/2024
##
## 3_a_Open_FD_data_BIRDS.R
##
################################################################################


# Define the pipe symbol so I can use it:
`%>%` <- magrittr::`%>%`


# 1 - Load INTEGRADIV occurrence data ================================


INTEGRADIV_birds_occ_df <- readRDS(here::here("transformed_data",
                                              "sp_asb_50km_BIRDS.rds"))


# 2 - Load and clean traits data ================================


# Load phylogeny data:
INTEGRADIV_traits <- read.csv(file = here::here("integradiv_db",
                                                "INTEGRADIV_traits_v3.csv"))

# Only keep TREE data:
birds_traits <- dplyr::filter(INTEGRADIV_traits,
                              Taxon == "Birds")

# Check if all species in the occurrence df are in the traits db: YES :)
setdiff(colnames(INTEGRADIV_birds_occ_df),
        unique(birds_traits$Species))

# Check traits db only contains species in the occurrence df: NO :/
setdiff(unique(birds_traits$Species),
        colnames(INTEGRADIV_birds_occ_df))


# Put it in the right format (species = rows, traits = columns)
birds_traits_df <- birds_traits %>%
  dplyr::select(c("Species", "Trait", "Value")) %>%
  tidyr::pivot_wider(names_from = Trait, values_from = Value)


# Select the traits that will be used:
birds_traits_df <- dplyr::select(birds_traits_df,
                                 c("Species",
                                   "Beak.Length_Culmen",
                                   "Beak.Width",
                                   "Age of first breeding",
                                   "Mass",
                                   "Hand-Wing.Index",
                                   "Migration",
                                   "Tail.Length",
                                   "Tarsus.Length",
                                   "Clutch_MEAN",
                                   "Broods per year",
                                   "Life span",
                                   "Fledging period"))


# Making a new variable: Beak_Ratio = depth/width:
birds_traits_df$Beak.Width <- as.numeric(birds_traits_df$Beak.Width)
birds_traits_df$Beak.Length_Culmen <- as.numeric(birds_traits_df$Beak.Length_Culmen)

birds_traits_df <- birds_traits_df %>%
  dplyr::mutate(Beak.Ratio = round(Beak.Length_Culmen / Beak.Width, 3)) %>%
  dplyr::select(-c("Beak.Length_Culmen", "Beak.Width"))


# Give new names to the traits:
birds_traits_df <- birds_traits_df %>%
  dplyr::rename(Age_First_Breeding = "Age of first breeding") %>%
  dplyr::rename(Hand_Wing_Index = "Hand-Wing.Index") %>%
  dplyr::rename(Tail_Length = "Tail.Length") %>%
  dplyr::rename(Tarsus_Length = "Tarsus.Length") %>%
  dplyr::rename(Clutch_Mean = "Clutch_MEAN") %>%
  dplyr::rename(Broods_Per_Year = "Broods per year") %>%
  dplyr::rename(Life_Span = "Life span") %>%
  dplyr::rename(Fledging_Period = "Fledging period") %>%
  dplyr::rename(Beak_Ratio = "Beak.Ratio")


# Format the traits:
birds_traits_df$Age_First_Breeding <- ordered(birds_traits_df$Age_First_Breeding,
                                               levels = c("1", "2", "3", "4", "5", "6"))
birds_traits_df$Mass <- as.numeric(birds_traits_df$Mass)
birds_traits_df$Hand_Wing_Index <- as.numeric(birds_traits_df$Hand_Wing_Index)
birds_traits_df$Migration <- ordered(birds_traits_df$Migration,
                                 levels = c("1", "2", "3"))
birds_traits_df$Tail_Length <- as.numeric(birds_traits_df$Tail_Length)
birds_traits_df$Tarsus_Length <- as.numeric(birds_traits_df$Tarsus_Length)
birds_traits_df$Clutch_Mean <- as.numeric(birds_traits_df$Clutch_Mean)
birds_traits_df$Broods_Per_Year <- ordered(birds_traits_df$Broods_Per_Year,
                                         levels = c("1", "1.5", "2", "2.5",
                                                    "3", "4.5"))
birds_traits_df$Life_Span <- as.numeric(birds_traits_df$Life_Span)
birds_traits_df$Fledging_Period <- as.numeric(birds_traits_df$Fledging_Period)
birds_traits_df$Beak_Ratio <- as.numeric(birds_traits_df$Beak_Ratio)


# Save the traits:
saveRDS(birds_traits_df, file = here::here("transformed_data",
                                           "raw_traits_BIRDS.rds"))


# 3 - Check for missing data ===================================================


# Change first column name for funbiogeo use:
funbiogeo_df <- dplyr::rename(birds_traits_df, species = Species)

# Get the percentage of completedness per traits:
funbiogeo::fb_plot_number_species_by_trait(funbiogeo_df)

# Get the percentage of species with for than 1, 2, 3 etc traits:
funbiogeo::fb_plot_number_traits_by_species(funbiogeo_df)






