################################################################################
##
## Script to study the relationships between drivers and PD Faith for all taxa
##
## Camille Magneville
##
## 16/05/2024
##
## 10_a_Relationsh_drivers_diversity_all_taxa_PD_Faith.R
##
################################################################################


# Define the pipe symbol so I can use it:
`%>%` <- magrittr::`%>%`


# 1 - Load environmental data and PD Faith =====================================


# Load environmental drivers - PCA synthetic var (with no NA for predictors and only cells which
# .. have values for all the studied taxa):
driv_db <- readRDS(here::here("transformed_data", "env_db",
                                      "SEM_env_db.rds"))

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
relationsh_ses_faith_df <- driv_db %>%
  dplyr::left_join(faith_ses_birds_df[, c("Idgrid", "ses")], by = "Idgrid") %>%
  dplyr::rename("ses_birds" = ses) %>%
  dplyr::left_join(faith_ses_reptiles_df[, c("Idgrid", "ses")], by = "Idgrid") %>%
  dplyr::rename("ses_reptiles" = ses) %>%
  dplyr::left_join(faith_ses_trees_df[, c("Idgrid", "ses")], by = "Idgrid") %>%
  dplyr::rename("ses_trees" = ses)


# 3 - Correlations between all drivers variables ===============================

cor_matrix <- Hmisc::rcorr(as.matrix(driv_db[, -1]),
                           type = "spearman")
correl_df <- as.data.frame(cor_matrix$r)
pvalues_correl_df <- as.data.frame(cor_matrix$P)

# Put variables names back (checked the code, it respects var order):
colnames(correl_df) <- colnames(driv_db)[-1]
rownames(correl_df) <- colnames(driv_db)[-1]
colnames(pvalues_correl_df) <- colnames(driv_db)[-1]
rownames(pvalues_correl_df) <- colnames(driv_db)[-1]

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


# 4 - Relationship between synthetic variables for past stab and Faith PD ======


# Histograms for drivers var:
hist(driv_db$PastClimStab_dim1)
hist(driv_db$PastClimStab_dim2)

# Histograms for PD Faith:
hist(relationsh_ses_faith_df$ses_birds)
hist(relationsh_ses_faith_df$ses_reptiles)
hist(relationsh_ses_faith_df$ses_trees)

# Test normality:
ggpubr::ggqqplot(relationsh_ses_faith_df$ses_birds)
ggpubr::ggqqplot(relationsh_ses_faith_df$ses_reptiles)
ggpubr::ggqqplot(relationsh_ses_faith_df$ses_trees)
shapiro.test(relationsh_ses_faith_df$ses_birds) #H0: fits normal distribution
shapiro.test(relationsh_ses_faith_df$ses_reptiles)
shapiro.test(relationsh_ses_faith_df$ses_trees)

# BIRDS:
# Test Linear Regression - dim1:
birds_pastclim1_lm <- lm(ses_birds ~ PastClimStab_dim1,
                         data = relationsh_ses_faith_df)
performance::check_model(birds_pastclim1_lm)



# Plot against Faith's PD:
# BIRDS:
plot(relationsh_ses_faith_df$ses_birds,
     relationsh_ses_faith_df$PastClimStab_dim1)
plot(relationsh_ses_faith_df$ses_birds,
     relationsh_ses_faith_df$PastClimStab_dim2)

# REPTILES:
plot(relationsh_ses_faith_df$ses_reptiles,
     relationsh_ses_faith_df$PastClimStab_dim1)
plot(relationsh_ses_faith_df$ses_reptiles,
     relationsh_ses_faith_df$PastClimStab_dim2)

# TREES:
plot(relationsh_ses_faith_df$ses_trees,
     relationsh_ses_faith_df$PastClimStab_dim1)
plot(relationsh_ses_faith_df$ses_trees,
     relationsh_ses_faith_df$PastClimStab_dim2)





