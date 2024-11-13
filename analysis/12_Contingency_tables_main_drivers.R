################################################################################
##
## Script to test whether +/- SES have associated with high/low values of
## ... certain drivers - directionality of each driver for the main ones
##
## Camille Magneville
##
## 11/2024
##
## 12_Contingency_tables_main_drivers.R
##
################################################################################

# Define the pipe symbol so I can use it:
`%>%` <- magrittr::`%>%`

# 1 - TREES ====================================================================

# 1 - a - PD ===================================================================

# Note: Get a contingency tables for the main drivers of PD dimensions, ...
# ... associated X squared, odd ratios ...
# ... and graphs: mosaic plot and scatter plot (to see continuous effect)

## Load data -------------------------------------------------------------------

# Load diversity data:
faith_ses_trees_df <- readRDS(here::here("transformed_data",
                                        "div_values_null_models",
                                        "PD_Faith_null_models_metrics_50km_TREES.rds"))
mpd_ses_trees_df <- readRDS(here::here("transformed_data",
                                         "div_values_null_models",
                                         "PD_MPD_null_models_metrics_50km_TREES.rds"))
mntd_ses_trees_df <- readRDS(here::here("transformed_data",
                                         "div_values_null_models",
                                         "PD_MNTD_null_models_metrics_50km_TREES.rds"))
# Load environmental drivers:
envdriv_full_db <- readRDS(here::here("transformed_data", "env_db",
                                      "env_drivers_final_restricted_db.rds"))

# Load drivers names:
drivers_nm_df <- read.csv(here::here("env_db",
                                     "Drivers_short_nm.csv"))

## Link diversity and drivers data ---------------------------------------------

driv_faith_trees_df <- dplyr::inner_join(envdriv_full_db,
                                      faith_ses_trees_df[, c("Idgrid", "ses")],
                                      by = "Idgrid")
driv_mpd_trees_df <- dplyr::inner_join(envdriv_full_db,
                                         mpd_ses_trees_df[, c("Idgrid", "ses")],
                                         by = "Idgrid")
driv_mntd_trees_df <- dplyr::inner_join(envdriv_full_db,
                                         mntd_ses_trees_df[, c("Idgrid", "ses")],
                                         by = "Idgrid")

## Richness --------------------------------------------------------------------

# Drivers to study: Past MAT sd, Herbivores Consumption, Depth Mean,
# ... Cl Vel YD increase, Growth Rate Pop

# have to call the ggmosaic package (can't figure out why necessary):
library(ggmosaic)

# Past MAT sd (function saves automatically graphs):
contingency.analyses(driver_ses_df = driv_faith_trees_df,
                     driver_nm = "Past_MAT_sd",
                     color_nms = c("#0881bd", "#ccdbe2"),
                     drivers_nm_df = drivers_nm_df,
                     facet_nm = "PD",
                     dim_nm = "Richness",
                     taxa_nm = "TREES")



## Dispersion ------------------------------------------------------------------

## Originality -----------------------------------------------------------------


# 1 - b - FD ===================================================================

# Note: Get a contingency tables for the main drivers of FD dimensions, ...
# ... associated X squared, odd ratios ...
# ... and graphs: mosaic plot and scatter plot (to see continuous effect)



