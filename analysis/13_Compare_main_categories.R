################################################################################
##
## Script to compare the main categories importance between taxa and metrics
##
## Camille Magneville
##
## 11/2024
##
## 13_Compare_main_categories.R
##
################################################################################

# Define the pipe symbol so I can use it:
`%>%` <- magrittr::`%>%`

# 1 - FD =======================================================================
# 1 - a - Load data ================================================================

# Load data for all taxa for richness
richn_trees <- readRDS(here::here("transformed_data",
                                  "std_rf_trees_FD_fric_50.rds"))
richn_butterflies <- readRDS(here::here("transformed_data",
                                        "std_rf_butterflies_FD_fric_50.rds"))
richn_birds <- readRDS(here::here("transformed_data",
                                  "std_rf_birds_FD_fric_50.rds"))
richn_reptiles <- readRDS(here::here("transformed_data",
                                     "std_rf_reptiles_FD_fric_50.rds"))
richn_mammals <- readRDS(here::here("transformed_data",
                                    "std_rf_mammals_FD_fric_50.rds"))

# Load data for all taxa for dispersion
disp_trees <- readRDS(here::here("transformed_data",
                                  "std_rf_trees_FD_fmpd_50.rds"))
disp_butterflies <- readRDS(here::here("transformed_data",
                                        "std_rf_butterflies_FD_fmpd_50.rds"))
disp_birds <- readRDS(here::here("transformed_data",
                                  "std_rf_birds_FD_fmpd_50.rds"))
disp_reptiles <- readRDS(here::here("transformed_data",
                                     "std_rf_reptiles_FD_fmpd_50.rds"))
disp_mammals <- readRDS(here::here("transformed_data",
                                    "std_rf_mammals_FD_fmpd_50.rds"))

# Load data for all taxa for originality
orig_trees <- readRDS(here::here("transformed_data",
                                  "std_rf_trees_FD_fori_50.rds"))
orig_butterflies <- readRDS(here::here("transformed_data",
                                        "std_rf_butterflies_FD_fori_50.rds"))
orig_birds <- readRDS(here::here("transformed_data",
                                  "std_rf_birds_FD_fori_50.rds"))
orig_reptiles <- readRDS(here::here("transformed_data",
                                     "std_rf_reptiles_FD_fori_50.rds"))
orig_mammals <- readRDS(here::here("transformed_data",
                                    "std_rf_mammals_FD_fori_50.rds"))

# 1 - b - Heatmap of drivers categories importance =============================

list_richness <- list("Trees" = richn_trees,
                      "Butterfies" = richn_butterflies,
                      "Birds" = richn_birds,
                      "Reptiles" = richn_reptiles,
                      "Mammals" = richn_mammals)
list_dispersion <- list("Trees" = disp_trees,
                        "Butterfies" = disp_butterflies,
                        "Birds" = disp_birds,
                        "Reptiles" = disp_reptiles,
                        "Mammals" = disp_mammals)
list_originality <- list("Trees" = orig_trees,
                         "Butterfies" = orig_butterflies,
                         "Birds" = orig_birds,
                         "Reptiles" = orig_reptiles,
                         "Mammals" = orig_mammals)

heatmap.categories(list_richness = list_richness,
                   list_dispersion = list_dispersion,
                   list_originality = list_originality,
                   facet_nm = "Functional diversity")

# 2 - PD =======================================================================
# 2 - a - Load data ================================================================

# Load data for all taxa for richness
richn_trees <- readRDS(here::here("transformed_data",
                                  "std_rf_trees_PD_faith_50.rds"))
richn_butterflies <- readRDS(here::here("transformed_data",
                                        "std_rf_butterflies_PD_faith_50.rds"))
richn_birds <- readRDS(here::here("transformed_data",
                                  "std_rf_birds_PD_faith_50.rds"))
richn_reptiles <- readRDS(here::here("transformed_data",
                                     "std_rf_reptiles_PD_faith_50.rds"))
richn_mammals <- readRDS(here::here("transformed_data",
                                    "std_rf_mammals_PD_faith_50.rds"))

# Load data for all taxa for dispersion
disp_trees <- readRDS(here::here("transformed_data",
                                 "std_rf_trees_PD_mpd_50.rds"))
disp_butterflies <- readRDS(here::here("transformed_data",
                                       "std_rf_butterflies_PD_mpd_50.rds"))
disp_birds <- readRDS(here::here("transformed_data",
                                 "std_rf_birds_PD_mpd_50.rds"))
disp_reptiles <- readRDS(here::here("transformed_data",
                                    "std_rf_reptiles_PD_mpd_50.rds"))
disp_mammals <- readRDS(here::here("transformed_data",
                                   "std_rf_mammals_PD_mpd_50.rds"))

# Load data for all taxa for originality
orig_trees <- readRDS(here::here("transformed_data",
                                 "std_rf_trees_PD_mntd_50.rds"))
orig_butterflies <- readRDS(here::here("transformed_data",
                                       "std_rf_butterflies_PD_mntd_50.rds"))
orig_birds <- readRDS(here::here("transformed_data",
                                 "std_rf_birds_PD_mntd_50.rds"))
orig_reptiles <- readRDS(here::here("transformed_data",
                                    "std_rf_reptiles_PD_mntd_50.rds"))
orig_mammals <- readRDS(here::here("transformed_data",
                                   "std_rf_mammals_PD_mntd_50.rds"))

# 1 - b - Heatmap of drivers categories importance =============================

list_richness <- list("Trees" = richn_trees,
                      "Butterfies" = richn_butterflies,
                      "Birds" = richn_birds,
                      "Reptiles" = richn_reptiles,
                      "Mammals" = richn_mammals)
list_dispersion <- list("Trees" = disp_trees,
                        "Butterfies" = disp_butterflies,
                        "Birds" = disp_birds,
                        "Reptiles" = disp_reptiles,
                        "Mammals" = disp_mammals)
list_originality <- list("Trees" = orig_trees,
                         "Butterfies" = orig_butterflies,
                         "Birds" = orig_birds,
                         "Reptiles" = orig_reptiles,
                         "Mammals" = orig_mammals)

heatmap.categories(list_richness = list_richness,
                   list_dispersion = list_dispersion,
                   list_originality = list_originality,
                   facet_nm = "Phylogenetic diversity")


