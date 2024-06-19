################################################################################
##
## Script to study the relationships between drivers and PD Faith for all taxa
##
## Camille Magneville
##
## 16/05/2024
##
## 11_a_Relationsh_drivers_diversity_all_taxa_PD_Faith.R
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

# Note shapiro test: as sample sizes grow, increasingly trivial
# ... departures from normality (which are almost always present in real data)
# ... will result in small p-values.
# ... For this reason, visual tests are more useful.

# Note GLM: Assumptions are about the RESIDUALS, not the actual varaibles


# Histograms for drivers var:
hist(driv_db$PastClimStab_dim1)
hist(driv_db$PastClimStab_dim2)

# Histograms for PD Faith:
hist(relationsh_ses_faith_df$ses_birds)
hist(relationsh_ses_faith_df$ses_reptiles)
hist(relationsh_ses_faith_df$ses_trees)

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


# BIRDS - GLM:

# Test Linear Regression - dim1:
birds_pastclim1_lm <- lm(ses_birds ~ PastClimStab_dim1,
                         data = relationsh_ses_faith_df)
# General tests model:
performance::check_model(birds_pastclim1_lm)
# Test normality residuals: OK
shapiro.test(rstandard(birds_pastclim1_lm))
# QQplot:
ggplot2::ggplot() +
  ggplot2::geom_qq(ggplot2::aes(sample = rstandard(birds_pastclim1_lm))) +
  ggplot2::geom_abline(color = "red") +
  ggplot2::coord_fixed()
# Model summary:
summary(birds_pastclim1_lm)
car::Anova(birds_pastclim1_lm)

# Test Linear Regression - dim2:
birds_pastclim2_lm <- lm(ses_birds ~ PastClimStab_dim2,
                         data = relationsh_ses_faith_df)
# General tests model:
performance::check_model(birds_pastclim2_lm)
# Test normality residuals:
shapiro.test(rstandard(birds_pastclim2_lm))
# QQplot:
ggplot2::ggplot() +
  ggplot2::geom_qq(ggplot2::aes(sample = rstandard(birds_pastclim2_lm))) +
  ggplot2::geom_abline(color = "red") +
  ggplot2::coord_fixed()
# Model summary:
summary(birds_pastclim2_lm)
car::Anova(birds_pastclim2_lm)


# REPTILES - GLM:

# Test Linear Regression - dim1:
reptiles_pastclim1_lm <- lm(ses_reptiles ~ PastClimStab_dim1,
                         data = relationsh_ses_faith_df)
# General tests model:
performance::check_model(reptiles_pastclim1_lm)
# Test normality residuals: OK
shapiro.test(rstandard(reptiles_pastclim1_lm))
# QQplot:
ggplot2::ggplot() +
  ggplot2::geom_qq(ggplot2::aes(sample = rstandard(reptiles_pastclim1_lm))) +
  ggplot2::geom_abline(color = "red") +
  ggplot2::coord_fixed()
# Model summary:
summary(reptiles_pastclim1_lm)
car::Anova(reptiles_pastclim1_lm)

# Test Linear Regression - dim2:
reptiles_pastclim2_lm <- lm(ses_reptiles ~ PastClimStab_dim2,
                         data = relationsh_ses_faith_df)
# General tests model:
performance::check_model(reptiles_pastclim2_lm)
# Test normality residuals:
shapiro.test(rstandard(reptiles_pastclim2_lm)) # Not normal but check visual cues
# QQplot:
ggplot2::ggplot() +
  ggplot2::geom_qq(ggplot2::aes(sample = rstandard(reptiles_pastclim2_lm))) +
  ggplot2::geom_abline(color = "red") +
  ggplot2::coord_fixed()
# Model summary:
summary(reptiles_pastclim2_lm)
car::Anova(reptiles_pastclim2_lm)


# TREES - GLM:

# Test Linear Regression - dim1:
trees_pastclim1_lm <- lm(ses_trees ~ PastClimStab_dim1,
                            data = relationsh_ses_faith_df)
# General tests model:
performance::check_model(trees_pastclim1_lm)
# Test normality residuals: OK
shapiro.test(rstandard(trees_pastclim1_lm)) # NOT NORMAL - check visual cues
# QQplot:
ggplot2::ggplot() +
  ggplot2::geom_qq(ggplot2::aes(sample = rstandard(trees_pastclim1_lm))) +
  ggplot2::geom_abline(color = "red") +
  ggplot2::coord_fixed() # STILL NO
# Model summary:
summary(trees_pastclim1_lm)
car::Anova(trees_pastclim1_lm)

# Test Linear Regression - dim2:
trees_pastclim2_lm <- lm(ses_trees ~ PastClimStab_dim2,
                            data = relationsh_ses_faith_df)
# General tests model:
performance::check_model(trees_pastclim2_lm)
# Test normality residuals:
shapiro.test(rstandard(trees_pastclim2_lm)) # NOT NORMAL - check visual cues
# QQplot:
ggplot2::ggplot() +
  ggplot2::geom_qq(ggplot2::aes(sample = rstandard(trees_pastclim2_lm))) +
  ggplot2::geom_abline(color = "red") +
  ggplot2::coord_fixed() # STILL NO
# Model summary:
summary(trees_pastclim2_lm)
car::Anova(trees_pastclim2_lm)


# Conclusion: I can't use glm because models don't respect residuals
#... normality for some of them + when I plot the data, don't look like
# ... linear relationships are ok: Let's try GAMs!


# Note GAMs: model non-linear relationships - model is additive in the sense
# ... that the effects of the predictors are summed: each predictor can have
# ... its own smooth function - GAMs do not assume a specific parametric form
# ... for the relationship between predictors and the response -


# BIRDS - GAMs:

# Test GAMs - dim1:
# s() as modelise non-linear relationships:
birds_pastclim1_gam <- mgcv::gam(ses_birds ~ s(PastClimStab_dim1),
                                 data = relationsh_ses_faith_df)
# Check model:
par(mfrow = c(2, 2))
plot(birds_pastclim1_gam, pages = 1)  # Check smooth functions
mgcv::gam.check(birds_pastclim1_gam)
qqnorm(resid(birds_pastclim1_gam))
qqline(resid(birds_pastclim1_gam))
summary(birds_pastclim1_gam)

# Test GAMs - dim2:
# s() as modelise non-linear relationships:
birds_pastclim2_gam <- mgcv::gam(ses_birds ~ s(PastClimStab_dim2),
                                 data = relationsh_ses_faith_df)
# Check model:
plot(birds_pastclim2_gam, pages = 1)  # Check smooth functions
mgcv::gam.check(birds_pastclim2_gam)
qqnorm(resid(birds_pastclim2_gam))
qqline(resid(birds_pastclim2_gam))


# REPTILES - GAMs:

# Test GAMs - dim1:
# s() as modelise non-linear relationships:
reptiles_pastclim1_gam <- mgcv::gam(ses_reptiles ~ s(PastClimStab_dim1),
                                 data = relationsh_ses_faith_df)
# Check model:
par(mfrow = c(2, 2))
plot(reptiles_pastclim1_gam, pages = 1)  # Check smooth functions
mgcv::gam.check(reptiles_pastclim1_gam)
qqnorm(resid(reptiles_pastclim1_gam))
qqline(resid(reptiles_pastclim1_gam))
summary(reptiles_pastclim1_gam)

# Test GAMs - dim2:
# s() as modelise non-linear relationships:
reptiles_pastclim2_gam <- mgcv::gam(ses_reptiles ~ s(PastClimStab_dim2),
                                 data = relationsh_ses_faith_df)
# Check model:
plot(reptiles_pastclim2_gam, pages = 1)  # Check smooth functions
mgcv::gam.check(reptiles_pastclim2_gam)
qqnorm(resid(reptiles_pastclim2_gam))
qqline(resid(reptiles_pastclim2_gam))


# TREES - GAMs:

# Test GAMs - dim1:
# s() as modelise non-linear relationships:
trees_pastclim1_gam <- mgcv::gam(ses_trees ~ s(PastClimStab_dim1),
                                    data = relationsh_ses_faith_df)
# Check model:
par(mfrow = c(2, 2))
plot(trees_pastclim1_gam, pages = 1)  # Check smooth functions
mgcv::gam.check(trees_pastclim1_gam)
qqnorm(resid(trees_pastclim1_gam))
qqline(resid(trees_pastclim1_gam))
summary(trees_pastclim1_gam)

# Test GAMs - dim2:
# s() as modelise non-linear relationships:
trees_pastclim2_gam <- mgcv::gam(ses_trees ~ s(PastClimStab_dim2),
                                    data = relationsh_ses_faith_df)
# Check model:
plot(trees_pastclim2_gam, pages = 1)  # Check smooth functions
mgcv::gam.check(trees_pastclim2_gam)
qqnorm(resid(trees_pastclim2_gam))
qqline(resid(trees_pastclim2_gam))

