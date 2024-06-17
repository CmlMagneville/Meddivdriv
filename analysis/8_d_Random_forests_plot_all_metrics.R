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


# Define the pipe symbol so I can use it:
`%>%` <- magrittr::`%>%`


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

# Load the file which contain drivers shorter names:
drivers_nm_df <- read.csv(here::here("env_db",
                                     "Drivers_short_nm.csv"))


# 2 - Build data frame for the circular plot ====================================


# Note: One data frame per taxa, so we can compare drivers between
# ... diversity metrics for each taxa:

# For birds:
rf_df_list <- list("PD Richness" = birds_Faith_rf,
                   "PD Dispersion" = birds_mpd_rf,
                   "PD Originality" = birds_mntd_rf)
rf_plot_birds_df <- create.df.circular.plot(rf_df_list = rf_df_list,
                                            var_nb = 15)

# For trees:
rf_df_list <- list("PD Richness" = trees_Faith_rf,
                   "PD Dispersion" = trees_mpd_rf,
                   "PD Originality" = trees_mntd_rf)
rf_plot_trees_df <- create.df.circular.plot(rf_df_list = rf_df_list,
                                            var_nb = 15)

# For reptiles:
rf_df_list <- list("PD Richness" = reptiles_Faith_rf,
                   "PD Dispersion" = reptiles_mpd_rf,
                   "PD Originality" = reptiles_mntd_rf)
rf_plot_reptiles_df <- create.df.circular.plot(rf_df_list = rf_df_list,
                                            var_nb = 15)


# 3 - Plot the circular plots ==================================================


# For birds: (palette according to which variables are chosen - put the colors
# ... which are not represented at the end)
palette <- c("darkslategray3",
             "palegreen4",
             "palegreen2",
             "orchid4",
             "orchid",
             "plum2",
             "tan1")
circular_plot_birds <- circular.drivers.plot(taxa_plot_df = rf_plot_birds_df,
                                             drivers_nm_df = drivers_nm_df,
                                             palette = palette)
circular_plot_birds

# Save it:
ggplot2::ggsave(plot = circular_plot_birds,
                filename = here::here("outputs",
                                      "circular_plot_50_BIRDS.pdf"),
                device = "pdf",
                scale = 0.7,
                height = 6500,
                width = 6500,
                units = "px",
                dpi = 600)
ggplot2::ggsave(plot = circular_plot_birds,
                filename = here::here("outputs",
                                      "circular_plot_50_BIRDS.jpeg"),
                device = "jpeg",
                scale = 0.7,
                height = 6500,
                width = 6500,
                units = "px",
                dpi = 600)


# For reptiles: (palette according to which variables are chosen - put the colors
# ... which are not represented at the end)
palette <- c("darkslategray3",
             "palegreen4",
             "palegreen2",
             "orchid4",
             "orchid",
             "plum2",
             "tan1")
circular_plot_reptiles <- circular.drivers.plot(taxa_plot_df = rf_plot_reptiles_df,
                                             drivers_nm_df = drivers_nm_df,
                                             palette = palette)
circular_plot_reptiles

# Save it:
ggplot2::ggsave(plot = circular_plot_reptiles,
                filename = here::here("outputs",
                                      "circular_plot_50_REPTILES.pdf"),
                device = "pdf",
                scale = 0.7,
                height = 6500,
                width = 6500,
                units = "px",
                dpi = 600)
ggplot2::ggsave(plot = circular_plot_reptiles,
                filename = here::here("outputs",
                                      "circular_plot_50_REPTILES.jpeg"),
                device = "jpeg",
                scale = 0.7,
                height = 6500,
                width = 6500,
                units = "px",
                dpi = 600)

# For trees: (palette according to which variables are chosen - put the colors
# ... which are not represented at the end)
unique(rf_plot_trees_df$Drivers_cat)
palette <- c("darkslategray3",
             "palegreen4",
             "palegreen2",
             "tan1",
             "orchid4",
             "orchid",
             "plum2")
circular_plot_trees <- circular.drivers.plot(taxa_plot_df = rf_plot_trees_df,
                                             drivers_nm_df = drivers_nm_df,
                                             palette = palette)
circular_plot_trees

# Save it:
ggplot2::ggsave(plot = circular_plot_trees,
                filename = here::here("outputs",
                                      "circular_plot_50_TREES.pdf"),
                device = "pdf",
                scale = 0.7,
                height = 6500,
                width = 6500,
                units = "px",
                dpi = 600)
ggplot2::ggsave(plot = circular_plot_trees,
                filename = here::here("outputs",
                                      "circular_plot_50_TREES.jpeg"),
                device = "jpeg",
                scale = 0.7,
                height = 6500,
                width = 6500,
                units = "px",
                dpi = 600)
