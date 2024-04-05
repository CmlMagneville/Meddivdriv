################################################################################
##
## Script to plot SES for PD and FD indices for all taxas
##
## Camille Magneville
##
## 04/04/2024
##
## 6_Maps_FD_PD_all_taxas.R
##
################################################################################


# !!! NOTE: DONE AT 50*50km SCALE !!!


# Define the pipe symbol so I can use it:
`%>%` <- magrittr::`%>%`


# 1 - TREES ####################################################################


# 1 a - Maps SES PD (Faith, MPD, MNTD) =========================================


# Load diversity data:
trees_ses_faith_df <- readRDS(here::here("transformed_data",
                              "PD_Faith_null_models_metrics_50km_TREES.rds"))
trees_ses_mpd_df <- readRDS(here::here("transformed_data",
                                       "PD_MPD_null_models_metrics_50km_TREES.rds"))
trees_ses_mntd_df <- readRDS(here::here("transformed_data",
                                        "PD_MNTD_null_models_metrics_50km_TREES.rds"))

# Load grid data:
grid_50km <- sf::st_read(here::here("integradiv_db",
                                    "spgrid_50x50km_EUROMEDIT_EPSG3035.shp"))
# Rename the GRD_ID column as Idgrid:
grid_50km <- dplyr::rename(grid_50km, Idgrid = GRD_ID)


# Transform the data frames to plot:
trees_ses_faith_clean_df <- trees_ses_faith_df %>%
  dplyr::select(c("Idgrid", "ses")) %>%
  dplyr::rename(metric = "ses") %>%
  dplyr::mutate(Grid = "50x50") %>%
  dplyr::mutate(Taxon = "Trees") %>%
  dplyr::select(c("Idgrid", "metric", "Grid", "Taxon"))
trees_ses_mpd_clean_df <- trees_ses_mpd_df %>%
  dplyr::select(c("Idgrid", "ses")) %>%
  dplyr::rename(metric = "ses") %>%
  dplyr::mutate(Grid = "50x50") %>%
  dplyr::mutate(Taxon = "Trees") %>%
  dplyr::select(c("Idgrid", "metric", "Grid", "Taxon"))
trees_ses_mntd_clean_df <- trees_ses_mntd_df %>%
  dplyr::select(c("Idgrid", "ses")) %>%
  dplyr::rename(metric = "ses") %>%
  dplyr::mutate(Grid = "50x50") %>%
  dplyr::mutate(Taxon = "Trees") %>%
  dplyr::select(c("Idgrid", "metric", "Grid", "Taxon"))


# Map and save:
trees_SES_faith_50km_map <- div.maps.plot(div_per_cell_df = trees_ses_faith_clean_df,
                                          div_facet_nm = "PD",
                                          metric_nm = "SES Faith",
                                          grid = grid_50km,
                                          continuous = TRUE,
                                          plot_title = TRUE,
                                          save = TRUE)
trees_SES_MPD_50km_map <- div.maps.plot(div_per_cell_df = trees_ses_mpd_clean_df,
                                          div_facet_nm = "PD",
                                          metric_nm = "SES MPD",
                                          grid = grid_50km,
                                          continuous = TRUE,
                                          plot_title = TRUE,
                                          save = TRUE)
trees_SES_MNTD_50km_map <- div.maps.plot(div_per_cell_df = trees_ses_mntd_clean_df,
                                        div_facet_nm = "PD",
                                        metric_nm = "SES MNTD",
                                        grid = grid_50km,
                                        continuous = TRUE,
                                        plot_title = TRUE,
                                        save = TRUE)




# 1 b - Maps SES FD (FRic, FMPD, FOri) =========================================






# 2 - BIRDS ####################################################################


# 2 a - Maps SES PD (Faith, MPD, MNTD) =========================================


# Load diversity data:
birds_ses_faith_df <- readRDS(here::here("transformed_data",
                                         "PD_Faith_null_models_metrics_50km_BIRDS.rds"))
birds_ses_mpd_df <- readRDS(here::here("transformed_data",
                                       "PD_MPD_null_models_metrics_50km_BIRDS.rds"))
birds_ses_mntd_df <- readRDS(here::here("transformed_data",
                                        "PD_MNTD_null_models_metrics_50km_BIRDS.rds"))

# Load grid data:
grid_50km <- sf::st_read(here::here("integradiv_db",
                                    "spgrid_50x50km_EUROMEDIT_EPSG3035.shp"))
# Rename the GRD_ID column as Idgrid:
grid_50km <- dplyr::rename(grid_50km, Idgrid = GRD_ID)


# Transform the data frames to plot:
birds_ses_faith_clean_df <- birds_ses_faith_df %>%
  dplyr::select(c("Idgrid", "ses")) %>%
  dplyr::rename(metric = "ses") %>%
  dplyr::mutate(Grid = "50x50") %>%
  dplyr::mutate(Taxon = "Birds") %>%
  dplyr::select(c("Idgrid", "metric", "Grid", "Taxon"))
birds_ses_mpd_clean_df <- birds_ses_mpd_df %>%
  dplyr::select(c("Idgrid", "ses")) %>%
  dplyr::rename(metric = "ses") %>%
  dplyr::mutate(Grid = "50x50") %>%
  dplyr::mutate(Taxon = "Birds") %>%
  dplyr::select(c("Idgrid", "metric", "Grid", "Taxon"))
birds_ses_mntd_clean_df <- birds_ses_mntd_df %>%
  dplyr::select(c("Idgrid", "ses")) %>%
  dplyr::rename(metric = "ses") %>%
  dplyr::mutate(Grid = "50x50") %>%
  dplyr::mutate(Taxon = "Birds") %>%
  dplyr::select(c("Idgrid", "metric", "Grid", "Taxon"))


# Map and save:
birds_SES_faith_50km_map <- div.maps.plot(div_per_cell_df = birds_ses_faith_clean_df,
                                          div_facet_nm = "PD",
                                          metric_nm = "SES Faith",
                                          grid = grid_50km,
                                          continuous = TRUE,
                                          plot_title = TRUE,
                                          save = TRUE)
birds_SES_MPD_50km_map <- div.maps.plot(div_per_cell_df = birds_ses_mpd_clean_df,
                                        div_facet_nm = "PD",
                                        metric_nm = "SES MPD",
                                        grid = grid_50km,
                                        continuous = TRUE,
                                        plot_title = TRUE,
                                        save = TRUE)
birds_SES_MNTD_50km_map <- div.maps.plot(div_per_cell_df = birds_ses_mntd_clean_df,
                                         div_facet_nm = "PD",
                                         metric_nm = "SES MNTD",
                                         grid = grid_50km,
                                         continuous = TRUE,
                                         plot_title = TRUE,
                                         save = TRUE)




# 2 b - Maps SES FD (FRic, FMPD, FOri) =========================================



# 3 - BUTTERFLIES ####################################################################





