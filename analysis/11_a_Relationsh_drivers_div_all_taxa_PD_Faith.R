################################################################################
##
## Script to study the relationships between drivers and PD Faith for all taxa
##
## Camille Magneville
##
## 16/05/2024
##
## 10_a_Relationsh_drvers_diversity_all_taxa_PD_Faith.R
##
################################################################################


# Define the pipe symbol so I can use it:
`%>%` <- magrittr::`%>%`


# 1 - Load environmental data and PD Faith =====================================


# Load environmental drivers (with no NA for predictors and only cells which
# .. have values for all the studied taxa):
envdriv_full_db <- readRDS(here::here("transformed_data", "env_db",
                                      "env_drivers_final_noNA_db.rds"))

# Load SES PD - Faith:
faith_ses_birds_df <- readRDS(here::here("transformed_data",
                                         "div_values_null_models",
                                         "PD_Faith_null_models_metrics_50km_BIRDS.rds"))
faith_ses_trees_df <- readRDS(here::here("transformed_data",
                                         "div_values_null_models",
                                         "PD_Faith_null_models_metrics_50km_TREES.rds"))
faith_ses_reptiles_df <- readRDS(here::here("transformed_data",
                                            "div_values_null_models",
                                            "PD_Faith_null_models_metrics_50km_REPTILES.rds"))


# 2 - Build a df to study relationships ========================================


# Link environmental db and ses for all taxa:
relationsh_ses_faith_df <- envdriv_full_db %>%
  dplyr::left_join(faith_ses_birds_df[, c("Idgrid", "ses")], by = "Idgrid") %>%
  dplyr::rename("ses_birds" = ses) %>%
  dplyr::left_join(faith_ses_reptiles_df[, c("Idgrid", "ses")], by = "Idgrid") %>%
  dplyr::rename("ses_reptiles" = ses) %>%
  dplyr::left_join(faith_ses_trees_df[, c("Idgrid", "ses")], by = "Idgrid") %>%
  dplyr::rename("ses_trees" = ses)


# 3 - Past climate stability ===================================================


# Visualise the distribution of past climate stablity variables:
hist(relationsh_ses_faith_df$Past_MAT_sd)
hist(relationsh_ses_faith_df$Past_TAP_sd)
hist(relationsh_ses_faith_df$Past_CCVelHolocene_mean.voccMag)
hist(relationsh_ses_faith_df$Past_CCVelLGM_mean.voccMag)
hist(relationsh_ses_faith_df$Past_CCVelShortTerm_mean.voccMag)
hist(relationsh_ses_faith_df$Past_CCVelYoungerDryas_mean.voccMag)











