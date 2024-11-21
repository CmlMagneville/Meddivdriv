################################################################################
##
## Script to test
## ... correlations among diversity metrics
##
## Camille Magneville
##
## 11/2024
##
## 15_Checks_diversity_correlation.R
##
################################################################################



# 1 - Check spatial autocorrelation ============================================


# 1 - a - Load random forest data for each random forest =======================

# Load grid data:
grid_50km <- sf::st_read(here::here("integradiv_db",
                                    "spgrid_50x50km_EUROMEDIT_EPSG3035.shp"))
# Rename the GRD_ID column as Idgrid:
grid_50km <- dplyr::rename(grid_50km, Idgrid = GRD_ID)

# TREES:
trees_pd_richn_rf <- readRDS(here::here("transformed_data",
                                        "residuals_rf_trees_PD_Faith_50.rds"))
trees_pd_disp_rf <- readRDS(here::here("transformed_data",
                                      "residuals_rf_trees_PD_mpd_50.rds"))
trees_pd_orig_rf <- readRDS(here::here("transformed_data",
                                       "residuals_rf_trees_PD_mntd_50.rds"))
trees_fd_richn_rf <- readRDS(here::here("transformed_data",
                                        "residuals_rf_trees_FD_fric_50.rds"))
trees_fd_disp_rf <- readRDS(here::here("transformed_data",
                                       "residuals_rf_trees_FD_fmpd_50.rds"))
trees_fd_orig_rf <- readRDS(here::here("transformed_data",
                                       "residuals_rf_trees_FD_fori_50.rds"))

# BIRDS

# REPTILES

# MAMMALS

# BUTTERFLIES


# 1 - b - Compute mean residuals over the 100 random forests ===================


# TREES
mean_resid_pd_richn
residuals_list <- trees_pd_richn_rf



# 1 - c - Compute Moran's I spatial autocorrelation and map it =================




# 2 - Compute correlation between metrics ======================================

# Cf already coded function

