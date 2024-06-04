################################################################################
##
## Script to plot per taxa, a circular barplot illustrating the main drivers
## ... of each facet of diversity
##
## Camille Magneville
##
## 04/06/2024
##
## 8_d_Random_forests_plot_all_metrics.R
##
################################################################################



# 1 - Load data ================================================================


# PD - Richness:
birds_Faith_rf <- readRDS(here::here("transformed_data", "rf_birds_PD_Faith_50.rds"))
reptiles_Faith_rf <- readRDS(here::here("transformed_data", "rf_reptiles_PD_Faith_50.rds"))
trees_Faith_rf <- readRDS(here::here("transformed_data", "rf_trees_PD_Faith_50.rds"))

# PD - Originality:
birds_mntd_rf <- readRDS(here::here("transformed_data", "rf_birds_PD_mntd_50.rds"))
reptiles_mntd_rf <- readRDS(here::here("transformed_data", "rf_reptiles_PD_mntd_50.rds"))
trees_mntd_rf <- readRDS(here::here("transformed_data", "rf_trees_PD_mntd_50.rds"))

# PD - Dispersion:
birds_mpd_rf <- readRDS(here::here("transformed_data", "rf_birds_PD_mpd_50.rds"))
reptiles_mpd_rf <- readRDS(here::here("transformed_data", "rf_reptiles_PD_mpd_50.rds"))
trees_mpd_rf <- readRDS(here::here("transformed_data", "rf_trees_PD_mpd_50.rds"))


# 2 - Build data frame for the circular plot ====================================


# Note: One data frame per taxa, so we can compare drivers between
# ... diversity metrics for each taxa:

# For birds:
rf_df_list <- list("PD Richness" = birds_Faith_rf,
                   "PD Dispersion" = birds_mpd_rf,
                   "PD Originality" = birds_mntd_rf)
var_nb <- 7


