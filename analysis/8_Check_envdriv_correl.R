################################################################################
##
## Script to check correlation among env drivers and chose a subset of
## ... uncorrelated variables
##
## Camille Magneville
##
## 01/05/2024
##
## 8_Check_envdriv_correl.R
##
################################################################################


# Define the pipe symbol so I can use it:
`%>%` <- magrittr::`%>%`


# 1 - Load the environmental drivers ===========================================


soil_topo_db <- readRDS(here::here("transformed_data",
                                   "env_db",
                                   "soil_topo_final_db.rds"))
disturb_db <- readRDS(here::here("transformed_data",
                                  "env_db",
                                  "fire_herb_final_db.rds"))
past_clim_db <- readRDS(here::here("transformed_data",
                                   "env_db",
                                   "past_veloc_heterog_final_db.rds"))
present_clim_db <- readRDS(here::here("transformed_data",
                                      "env_db",
                                      "present_clim_final_db.rds"))
past_lu_db <- readRDS(here::here("transformed_data",
                                 "env_db",
                                 "past_landuse_final_db.rds"))
present_lu_db <- readRDS(here::here("transformed_data",
                                    "env_db",
                                    "present_landuse_final_db.rds"))
present_pop_db <- readRDS(here::here("transformed_data",
                                     "env_db",
                                     "present_pop_final_db.rds"))


# 2 - Create a data frame for correlation analyses =============================


# Long format for all db:
# Soil:
soil_topo_long_db <- soil_topo_db %>%
  dplyr::mutate("Full_metric_nm" = paste0(FinalVariableCode, sep = "_",
                                         Metric)) %>%

  reshape::cast(Idgrid ~ Full_metric_nm,
                 value = "Value")
# Disturbances:
disturb_long_db <- disturb_db %>%
  dplyr::mutate("Full_metric_nm" = paste0(FinalVariableCode, sep = "_",
                                          Metric)) %>%

  reshape::cast(Idgrid ~ Full_metric_nm,
                value = "Value")
# Past climate:
past_clim_long_db <- past_clim_db %>%
  dplyr::mutate("Full_metric_nm" = paste0("Past", sep = "_",
                                          FinalVariableCode, sep = "_",
                                          Metric)) %>%

  reshape::cast(Idgrid ~ Full_metric_nm,
                value = "Value")
# Present climate:
present_clim_long_db <- present_clim_db %>%
  dplyr::mutate("Full_metric_nm" = paste0("Present", sep = "_",
                                          FinalVariableCode, sep = "_",
                                          Metric)) %>%

  reshape::cast(Idgrid ~ Full_metric_nm,
                value = "Value")
# Past lu:
past_lu_long_db <- past_lu_db %>%
  dplyr::mutate("Full_metric_nm" = paste0("Past", sep = "_",
                                          FinalVariableCode, sep = "_",
                                          Metric)) %>%

  reshape::cast(Idgrid ~ Full_metric_nm,
                value = "Value")
# Present lu:
present_lu_long_db <- present_lu_db %>%
  dplyr::mutate("Full_metric_nm" = paste0("Present", sep = "_",
                                          FinalVariableCode, sep = "_",
                                          Metric)) %>%

  reshape::cast(Idgrid ~ Full_metric_nm,
                value = "Value")
# Present pop:
present_pop_long_db <- present_pop_db %>%
  dplyr::mutate("Full_metric_nm" = paste0(FinalVariableCode, sep = "_",
                                          Metric)) %>%

  reshape::cast(Idgrid ~ Full_metric_nm,
                value = "Value")

# Bind all the drivers together:
envdriv_full_db <- soil_topo_long_db %>%
  dplyr::left_join(disturb_long_db, by = "Idgrid") %>%
  dplyr::left_join(past_clim_long_db, by = "Idgrid") %>%
  dplyr::left_join(present_clim_long_db, by = "Idgrid") %>%
  dplyr::left_join(past_lu_long_db, by = "Idgrid") %>%
  dplyr::left_join(present_lu_long_db, by = "Idgrid") %>%
  dplyr::left_join(present_pop_long_db, by = "Idgrid")


# 3 - Compute pairwise correlation============================================


# Make sure each column is numeric:
envdriv_full_db[, -1] <- apply(envdriv_full_db[, -1], 2, as.numeric)


# Compute correlation matrix between all env drivers:
cor_matrix <- Hmisc::rcorr(as.matrix(envdriv_full_db[, -1]),
                           type = "spearman")
correl_df <- as.data.frame(cor_matrix$r)
pvalues_correl_df <- as.data.frame(cor_matrix$P)

# Put variables names back (check the code, respect var order):
colnames(correl_df) <- colnames(envdriv_full_db)[-1]
rownames(correl_df) <- colnames(envdriv_full_db)[-1]
colnames(pvalues_correl_df) <- colnames(envdriv_full_db)[-1]
rownames(pvalues_correl_df) <- colnames(envdriv_full_db)[-1]

# Format so can link correl and pvalues:
full_correl_df <- reshape2::melt(as.matrix(correl_df)) %>%
  dplyr::rename(Correl = value) %>%
  dplyr::mutate("Comb" = paste0(Var1, sep = "_", Var2))
full_pvalue_df <- reshape2::melt(as.matrix(pvalues_correl_df)) %>%
  dplyr::rename(Pvalue = value) %>%
  dplyr::mutate("Comb" = paste0(Var1, sep = "_", Var2))

# Create a final df with pvalues and spearman correl:
full_correl_pvalue_df <- dplyr::left_join(full_correl_df,
                                          full_pvalue_df[, -c(1, 2)],
                                          by = "Comb") %>%
  dplyr::select(c("Var1", "Var2", "Correl", "Pvalue"))


# Get the most correlated variables: BE CAREFUL 1 pair + 2 rows
correl_70 <- subset(full_correl_pvalue_df, Correl > .70 & Correl != 1)


# 4 - Visualize correlation between envdrivers =================================


# Note: Between variables of a specific type of drivers and then by
# ... pairs of environmental drivers

# Soil, Topo and Present climate : present habitat characteritics:
GGally::ggpairs(envdriv_full_db[, c(2:11, 20:25)],
                upper = list(continuous = GGally::wrap("cor",
                                                       method = "spearman")))


