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
shapiro.test(rstandard(birds_pastclim1_lm)) # H0: normal distrib
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

# Test Linear Regression - dim 1 + dim 2:
birds_pastclim_lm <- lm(ses_birds ~ PastClimStab_dim1+
                                    PastClimStab_dim2,
                         data = relationsh_ses_faith_df)
# General tests model:
performance::check_model(birds_pastclim_lm)
# Test normality residuals: OK
shapiro.test(rstandard(birds_pastclim_lm))
# QQplot:
ggplot2::ggplot() +
  ggplot2::geom_qq(ggplot2::aes(sample = rstandard(birds_pastclim_lm))) +
  ggplot2::geom_abline(color = "red") +
  ggplot2::coord_fixed()
# Model summary:
summary(birds_pastclim_lm)
car::Anova(birds_pastclim_lm)


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

# Test Linear Regression - dim 1 + dim 2:
reptiles_pastclim_lm <- lm(ses_reptiles ~ PastClimStab_dim1+
                          PastClimStab_dim2,
                        data = relationsh_ses_faith_df)
# General tests model:
performance::check_model(reptiles_pastclim_lm)
# Test normality residuals: OK
shapiro.test(rstandard(reptiles_pastclim_lm))
# QQplot:
ggplot2::ggplot() +
  ggplot2::geom_qq(ggplot2::aes(sample = rstandard(reptiles_pastclim_lm))) +
  ggplot2::geom_abline(color = "red") +
  ggplot2::coord_fixed()
# Model summary:
summary(reptiles_pastclim_lm)
car::Anova(reptiles_pastclim_lm)


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

# Test Linear Regression - dim 1 + dim 2:
trees_pastclim_lm <- lm(ses_trees ~ PastClimStab_dim1+
                             PastClimStab_dim2,
                           data = relationsh_ses_faith_df)
# General tests model:
performance::check_model(trees_pastclim_lm)
# Test normality residuals: OK
shapiro.test(rstandard(trees_pastclim_lm))
# QQplot:
ggplot2::ggplot() +
  ggplot2::geom_qq(ggplot2::aes(sample = rstandard(trees_pastclim_lm))) +
  ggplot2::geom_abline(color = "red") +
  ggplot2::coord_fixed()
# Model summary:
summary(trees_pastclim_lm)
car::Anova(trees_pastclim_lm)



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

# Test GAMs - dim1 + dim2:
# s() as modelise non-linear relationships:
birds_pastclim_gam <- mgcv::gam(ses_birds ~ s(PastClimStab_dim1)+
                                  s(PastClimStab_dim2),
                                 data = relationsh_ses_faith_df)
# Check model:
plot(birds_pastclim_gam, pages = 1)  # Check smooth functions
mgcv::gam.check(birds_pastclim_gam)
qqnorm(resid(birds_pastclim_gam))
qqline(resid(birds_pastclim_gam))


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


# Test GAMs - dim1 + dim2:
# s() as modelise non-linear relationships:
reptiles_pastclim_gam <- mgcv::gam(ses_reptiles ~ s(PastClimStab_dim1)+
                                  s(PastClimStab_dim2),
                                data = relationsh_ses_faith_df)
# Check model:
plot(reptiles_pastclim_gam, pages = 1)  # Check smooth functions
mgcv::gam.check(reptiles_pastclim_gam)
qqnorm(resid(reptiles_pastclim_gam))
qqline(resid(reptiles_pastclim_gam))


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


# Test GAMs - dim1 + dim2:
# s() as modelise non-linear relationships:
trees_pastclim_gam <- mgcv::gam(ses_trees ~ s(PastClimStab_dim1)+
                                     s(PastClimStab_dim2),
                                   data = relationsh_ses_faith_df)
# Check model:
plot(trees_pastclim_gam, pages = 1)  # Check smooth functions
mgcv::gam.check(trees_pastclim_gam)
qqnorm(resid(trees_pastclim_gam))
qqline(resid(trees_pastclim_gam))


# 5 - Relationship between synthetic variables for present clim mean and Faith PD ======


# Histograms for drivers var:
hist(driv_db$PresentClimMean_dim1)
hist(driv_db$PresentClimMean_dim2)

# Histograms for PD Faith:
hist(relationsh_ses_faith_df$ses_birds)
hist(relationsh_ses_faith_df$ses_reptiles)
hist(relationsh_ses_faith_df$ses_trees)


# Plot against Faith's PD:
# BIRDS:
plot(relationsh_ses_faith_df$ses_birds,
     relationsh_ses_faith_df$PresentClimMean_dim1)
plot(relationsh_ses_faith_df$ses_birds,
     relationsh_ses_faith_df$PresentClimMean_dim2)
# REPTILES:
plot(relationsh_ses_faith_df$ses_reptiles,
     relationsh_ses_faith_df$PresentClimMean_dim1)
plot(relationsh_ses_faith_df$ses_reptiles,
     relationsh_ses_faith_df$PresentClimMean_dim2)
# TREES:
plot(relationsh_ses_faith_df$ses_trees,
     relationsh_ses_faith_df$PresentClimMean_dim1)
plot(relationsh_ses_faith_df$ses_trees,
     relationsh_ses_faith_df$PresentClimMean_dim2)


# BIRDS - GLM:

# Test Linear Regression - dim 1 + dim 2:
birds_presclimmean_lm <- lm(ses_birds ~ PresentClimMean_dim1 +
                             PresentClimMean_dim2,
                        data = relationsh_ses_faith_df)
# General tests model:
performance::check_model(birds_presclimmean_lm)
# Test normality residuals: NO
shapiro.test(rstandard(birds_presclimmean_lm))
# QQplot:
ggplot2::ggplot() +
  ggplot2::geom_qq(ggplot2::aes(sample = rstandard(birds_presclimmean_lm))) +
  ggplot2::geom_abline(color = "red") +
  ggplot2::coord_fixed()
# Model summary:
summary(birds_presclimmean_lm)
car::Anova(birds_presclimmean_lm)


# BIRDS - GAMs:

# Test GAMs - dim1 + dim2:
# s() as modelise non-linear relationships:
birds_presclimmean_gam <- mgcv::gam(ses_birds ~ s(PresentClimMean_dim1)+
                                  s(PresentClimMean_dim2),
                                data = relationsh_ses_faith_df)
# Check model:
plot(birds_presclimmean_gam, pages = 1)  # Check smooth functions
par(mfrow = c(2,2))
mgcv::gam.check(birds_presclimmean_gam)
par(mfrow = c(1,1))
qqnorm(resid(birds_presclimmean_gam))
qqline(resid(birds_presclimmean_gam))



# REPTILES - GLM:

# Test Linear Regression - dim 1 + dim 2:
reptiles_presclimmean_lm <- lm(ses_reptiles ~ PresentClimMean_dim1 +
                              PresentClimMean_dim2,
                            data = relationsh_ses_faith_df)
# General tests model:
performance::check_model(reptiles_presclimmean_lm)
# Test normality residuals: NO
shapiro.test(rstandard(reptiles_presclimmean_lm))
# QQplot:
ggplot2::ggplot() +
  ggplot2::geom_qq(ggplot2::aes(sample = rstandard(reptiles_presclimmean_lm))) +
  ggplot2::geom_abline(color = "red") +
  ggplot2::coord_fixed()
# Model summary:
summary(reptiles_presclimmean_lm)
car::Anova(reptiles_presclimmean_lm)


# REPTILES - GAMs:

# Test GAMs - dim1 + dim2:
# s() as modelise non-linear relationships:
reptiles_presclimmean_gam <- mgcv::gam(ses_reptiles ~ s(PresentClimMean_dim1)+
                                      s(PresentClimMean_dim2),
                                    data = relationsh_ses_faith_df)
# Check model:
plot(reptiles_presclimmean_gam, pages = 1)  # Check smooth functions
par(mfrow = c(2,2))
mgcv::gam.check(reptiles_presclimmean_gam)
par(mfrow = c(1,1))
qqnorm(resid(reptiles_presclimmean_gam))
qqline(resid(reptiles_presclimmean_gam))


# TREES - GLM:

# Test Linear Regression - dim 1 + dim 2:
trees_presclimmean_lm <- lm(ses_trees ~ PresentClimMean_dim1 +
                                 PresentClimMean_dim2,
                               data = relationsh_ses_faith_df)
# General tests model:
performance::check_model(trees_presclimmean_lm)
# Test normality residuals: NO
shapiro.test(rstandard(trees_presclimmean_lm))
# QQplot:
ggplot2::ggplot() +
  ggplot2::geom_qq(ggplot2::aes(sample = rstandard(trees_presclimmean_lm))) +
  ggplot2::geom_abline(color = "red") +
  ggplot2::coord_fixed()
# Model summary:
summary(trees_presclimmean_lm)
car::Anova(trees_presclimmean_lm)


# TREES - GAMs:

# Test GAMs - dim1 + dim2:
# s() as modelise non-linear relationships:
trees_presclimmean_gam <- mgcv::gam(ses_trees ~ s(PresentClimMean_dim1)+
                                         s(PresentClimMean_dim2),
                                       data = relationsh_ses_faith_df)
# Check model:
plot(trees_presclimmean_gam, pages = 1)  # Check smooth functions
par(mfrow = c(2,2))
mgcv::gam.check(trees_presclimmean_gam)
par(mfrow = c(1,1))
qqnorm(resid(trees_presclimmean_gam))
qqline(resid(trees_presclimmean_gam))



# 6 - Relationship between synthetic variables for present clim sd and Faith PD ======


# Histograms for drivers var:
hist(driv_db$PresentClimSd_dim1)
hist(driv_db$PresentClimSd_dim2)

# Histograms for PD Faith:
hist(relationsh_ses_faith_df$ses_birds)
hist(relationsh_ses_faith_df$ses_reptiles)
hist(relationsh_ses_faith_df$ses_trees)


# Plot against Faith's PD:
# BIRDS:
plot(relationsh_ses_faith_df$ses_birds,
     relationsh_ses_faith_df$PresentClimSd_dim1)
plot(relationsh_ses_faith_df$ses_birds,
     relationsh_ses_faith_df$PresentClimSd_dim2)
# REPTILES:
plot(relationsh_ses_faith_df$ses_reptiles,
     relationsh_ses_faith_df$PresentClimSd_dim1)
plot(relationsh_ses_faith_df$ses_reptiles,
     relationsh_ses_faith_df$PresentClimSd_dim2)
# TREES:
plot(relationsh_ses_faith_df$ses_trees,
     relationsh_ses_faith_df$PresentClimSd_dim1)
plot(relationsh_ses_faith_df$ses_trees,
     relationsh_ses_faith_df$PresentClimSd_dim2)


# BIRDS - GLM:

# Test Linear Regression - dim 1 + dim 2:
birds_presclimsd_lm <- lm(ses_birds ~ PresentClimSd_dim1 +
                              PresentClimSd_dim2,
                            data = relationsh_ses_faith_df)
# General tests model:
performance::check_model(birds_presclimsd_lm)
# Test normality residuals: YES
shapiro.test(rstandard(birds_presclimsd_lm))
# QQplot:
ggplot2::ggplot() +
  ggplot2::geom_qq(ggplot2::aes(sample = rstandard(birds_presclimsd_lm))) +
  ggplot2::geom_abline(color = "red") +
  ggplot2::coord_fixed()
# Model summary:
summary(birds_presclimsd_lm)
car::Anova(birds_presclimsd_lm)


# BIRDS - GAMs:

# Test GAMs - dim1 + dim2:
# s() as modelise non-linear relationships:
birds_presclimsd_gam <- mgcv::gam(ses_birds ~ s(PresentClimSd_dim1)+
                                      s(PresentClimSd_dim2),
                                    data = relationsh_ses_faith_df)
# Check model:
plot(birds_presclimsd_gam, pages = 1)  # Check smooth functions
par(mfrow = c(2,2))
mgcv::gam.check(birds_presclimsd_gam)
par(mfrow = c(1,1))
qqnorm(resid(birds_presclimsd_gam))
qqline(resid(birds_presclimsd_gam))



# REPTILES - GLM:

# Test Linear Regression - dim 1 + dim 2:
reptiles_presclimsd_lm <- lm(ses_reptiles ~ PresentClimSd_dim1 +
                                 PresentClimSd_dim2,
                               data = relationsh_ses_faith_df)
# General tests model:
performance::check_model(reptiles_presclimsd_lm)
# Test normality residuals: NO
shapiro.test(rstandard(reptiles_presclimsd_lm))
# QQplot:
ggplot2::ggplot() +
  ggplot2::geom_qq(ggplot2::aes(sample = rstandard(reptiles_presclimsd_lm))) +
  ggplot2::geom_abline(color = "red") +
  ggplot2::coord_fixed()
# Model summary:
summary(reptiles_presclimsd_lm)
car::Anova(reptiles_presclimsd_lm)


# REPTILES - GAMs:

# Test GAMs - dim1 + dim2:
# s() as modelise non-linear relationships:
reptiles_presclimsd_gam <- mgcv::gam(ses_reptiles ~ s(PresentClimSd_dim1)+
                                         s(PresentClimSd_dim2),
                                       data = relationsh_ses_faith_df)
# Check model:
plot(reptiles_presclimsd_gam, pages = 1)  # Check smooth functions
par(mfrow = c(2,2))
mgcv::gam.check(reptiles_presclimsd_gam)
par(mfrow = c(1,1))
qqnorm(resid(reptiles_presclimsd_gam))
qqline(resid(reptiles_presclimsd_gam))


# TREES - GLM:

# Test Linear Regression - dim 1 + dim 2:
trees_presclimsd_lm <- lm(ses_trees ~ PresentClimSd_dim1 +
                              PresentClimSd_dim2,
                            data = relationsh_ses_faith_df)
# General tests model:
performance::check_model(trees_presclimsd_lm)
# Test normality residuals: NO
shapiro.test(rstandard(trees_presclimsd_lm))
# QQplot:
ggplot2::ggplot() +
  ggplot2::geom_qq(ggplot2::aes(sample = rstandard(trees_presclimsd_lm))) +
  ggplot2::geom_abline(color = "red") +
  ggplot2::coord_fixed()
# Model summary:
summary(trees_presclimsd_lm)
car::Anova(trees_presclimsd_lm)


# TREES - GAMs:

# Test GAMs - dim1 + dim2:
# s() as modelise non-linear relationships:
trees_presclimsd_gam <- mgcv::gam(ses_trees ~ s(PresentClimSd_dim1)+
                                      s(PresentClimSd_dim2),
                                    data = relationsh_ses_faith_df)
# Check model:
plot(trees_presclimsd_gam, pages = 1)  # Check smooth functions
par(mfrow = c(2,2))
mgcv::gam.check(trees_presclimsd_gam)
par(mfrow = c(1,1))
qqnorm(resid(trees_presclimsd_gam))
qqline(resid(trees_presclimsd_gam))


# 7 - Relationship between synthetic variables for hab mean and Faith PD ======


# Histograms for drivers var:
hist(driv_db$PresentHabMean_dim1)
hist(driv_db$PresentHabMean_dim2)
hist(driv_db$PresentHabMean_dim3)

# Histograms for PD Faith:
hist(relationsh_ses_faith_df$ses_birds)
hist(relationsh_ses_faith_df$ses_reptiles)
hist(relationsh_ses_faith_df$ses_trees)


# Plot against Faith's PD:
# BIRDS:
plot(relationsh_ses_faith_df$ses_birds,
     relationsh_ses_faith_df$PresentHabMean_dim1)
plot(relationsh_ses_faith_df$ses_birds,
     relationsh_ses_faith_df$PresentHabMean_dim2)
plot(relationsh_ses_faith_df$ses_birds,
     relationsh_ses_faith_df$PresentHabMean_dim3)
# REPTILES:
plot(relationsh_ses_faith_df$ses_reptiles,
     relationsh_ses_faith_df$PresentHabMean_dim1)
plot(relationsh_ses_faith_df$ses_reptiles,
     relationsh_ses_faith_df$PresentHabMean_dim2)
plot(relationsh_ses_faith_df$ses_reptiles,
     relationsh_ses_faith_df$PresentHabMean_dim3)
# TREES:
plot(relationsh_ses_faith_df$ses_trees,
     relationsh_ses_faith_df$PresentHabMean_dim1)
plot(relationsh_ses_faith_df$ses_trees,
     relationsh_ses_faith_df$PresentHabMean_dim2)
plot(relationsh_ses_faith_df$ses_trees,
     relationsh_ses_faith_df$PresentHabMean_dim3)


# BIRDS - GLM:

# Test Linear Regression - dim 1 + dim 2 + dim 3:
birds_PresentHabMean_lm <- lm(ses_birds ~ PresentHabMean_dim1 +
                                PresentHabMean_dim2 +
                                PresentHabMean_dim3,
                            data = relationsh_ses_faith_df)
# General tests model:
performance::check_model(birds_PresentHabMean_lm)
# Test normality residuals: NO
shapiro.test(rstandard(birds_PresentHabMean_lm))
# QQplot:
ggplot2::ggplot() +
  ggplot2::geom_qq(ggplot2::aes(sample = rstandard(birds_PresentHabMean_lm))) +
  ggplot2::geom_abline(color = "red") +
  ggplot2::coord_fixed()
# Model summary:
summary(birds_PresentHabMean_lm)
car::Anova(birds_PresentHabMean_lm)


# BIRDS - GAMs:

# Test GAMs - dim1 + dim2 + dim3:
# s() as modelise non-linear relationships:
birds_PresentHabMean_gam <- mgcv::gam(ses_birds ~ s(PresentHabMean_dim1)+
                                        s(PresentHabMean_dim2) +
                                        s(PresentHabMean_dim3),
                                    data = relationsh_ses_faith_df)
# Check model:
plot(birds_PresentHabMean_gam, pages = 1)  # Check smooth functions
par(mfrow = c(2,2))
mgcv::gam.check(birds_PresentHabMean_gam)
par(mfrow = c(1,1))
qqnorm(resid(birds_PresentHabMean_gam))
qqline(resid(birds_PresentHabMean_gam))



# REPTILES - GLM:

# Test Linear Regression - dim 1 + dim 2 + dim 3:
reptiles_PresentHabMean_lm <- lm(ses_reptiles ~ PresentHabMean_dim1 +
                                  PresentHabMean_dim2 +
                                  PresentHabMean_dim3,
                               data = relationsh_ses_faith_df)
# General tests model:
performance::check_model(reptiles_PresentHabMean_lm)
# Test normality residuals: YES
shapiro.test(rstandard(reptiles_PresentHabMean_lm))
# QQplot:
ggplot2::ggplot() +
  ggplot2::geom_qq(ggplot2::aes(sample = rstandard(reptiles_PresentHabMean_lm))) +
  ggplot2::geom_abline(color = "red") +
  ggplot2::coord_fixed()
# Model summary:
summary(reptiles_PresentHabMean_lm)
car::Anova(reptiles_PresentHabMean_lm)


# REPTILES - GAMs:

# Test GAMs - dim1 + dim2 + dim3:
# s() as modelise non-linear relationships:
reptiles_PresentHabMean_gam <- mgcv::gam(ses_reptiles ~ s(PresentHabMean_dim1)+
                                           s(PresentHabMean_dim2)+
                                           s(PresentHabMean_dim3),
                                       data = relationsh_ses_faith_df)
# Check model:
plot(reptiles_PresentHabMean_gam, pages = 1)  # Check smooth functions
par(mfrow = c(2,2))
mgcv::gam.check(reptiles_PresentHabMean_gam)
par(mfrow = c(1,1))
qqnorm(resid(reptiles_PresentHabMean_gam))
qqline(resid(reptiles_PresentHabMean_gam))


# TREES - GLM:

# Test Linear Regression - dim 1 + dim 2 + dim 3:
trees_PresentHabMean_lm <- lm(ses_trees ~ PresentHabMean_dim1 +
                              PresentHabMean_dim2 +
                              PresentHabMean_dim3,
                            data = relationsh_ses_faith_df)
# General tests model:
performance::check_model(trees_PresentHabMean_lm)
# Test normality residuals: NO
shapiro.test(rstandard(trees_PresentHabMean_lm))
# QQplot:
ggplot2::ggplot() +
  ggplot2::geom_qq(ggplot2::aes(sample = rstandard(trees_PresentHabMean_lm))) +
  ggplot2::geom_abline(color = "red") +
  ggplot2::coord_fixed()
# Model summary:
summary(trees_PresentHabMean_lm)
car::Anova(trees_PresentHabMean_lm)


# TREES - GAMs:

# Test GAMs - dim1 + dim2 + dim3:
# s() as modelise non-linear relationships:
trees_PresentHabMean_gam <- mgcv::gam(ses_trees ~ s(PresentHabMean_dim1)+
                                        s(PresentHabMean_dim2) +
                                        s(PresentHabMean_dim3),
                                    data = relationsh_ses_faith_df)
# Check model:
plot(trees_PresentHabMean_gam, pages = 1)  # Check smooth functions
par(mfrow = c(2,2))
mgcv::gam.check(trees_PresentHabMean_gam)
par(mfrow = c(1,1))
qqnorm(resid(trees_PresentHabMean_gam))
qqline(resid(trees_PresentHabMean_gam))


# 8 - Relationship between synthetic variables for hab sd and Faith PD ======


# Histograms for drivers var:
hist(driv_db$PresentHabSd_dim1)
hist(driv_db$PresentHabSd_dim2)
hist(driv_db$PresentHabSd_dim3)

# Histograms for PD Faith:
hist(relationsh_ses_faith_df$ses_birds)
hist(relationsh_ses_faith_df$ses_reptiles)
hist(relationsh_ses_faith_df$ses_trees)


# Plot against Faith's PD:
# BIRDS:
plot(relationsh_ses_faith_df$ses_birds,
     relationsh_ses_faith_df$PresentHabSd_dim1)
plot(relationsh_ses_faith_df$ses_birds,
     relationsh_ses_faith_df$PresentHabSd_dim2)
plot(relationsh_ses_faith_df$ses_birds,
     relationsh_ses_faith_df$PresentHabSd_dim3)
# REPTILES:
plot(relationsh_ses_faith_df$ses_reptiles,
     relationsh_ses_faith_df$PresentHabSd_dim1)
plot(relationsh_ses_faith_df$ses_reptiles,
     relationsh_ses_faith_df$PresentHabSd_dim2)
plot(relationsh_ses_faith_df$ses_reptiles,
     relationsh_ses_faith_df$PresentHabSd_dim3)
# TREES:
plot(relationsh_ses_faith_df$ses_trees,
     relationsh_ses_faith_df$PresentHabSd_dim1)
plot(relationsh_ses_faith_df$ses_trees,
     relationsh_ses_faith_df$PresentHabSd_dim2)
plot(relationsh_ses_faith_df$ses_trees,
     relationsh_ses_faith_df$PresentHabSd_dim3)


# BIRDS - GLM:

# Test Linear Regression - dim 1 + dim 2 + dim 3:
birds_PresentHabSd_lm <- lm(ses_birds ~ PresentHabSd_dim1 +
                                PresentHabSd_dim2 +
                                PresentHabSd_dim3,
                              data = relationsh_ses_faith_df)
# General tests model:
performance::check_model(birds_PresentHabSd_lm)
# Test normality residuals: YES
shapiro.test(rstandard(birds_PresentHabSd_lm))
# QQplot:
ggplot2::ggplot() +
  ggplot2::geom_qq(ggplot2::aes(sample = rstandard(birds_PresentHabSd_lm))) +
  ggplot2::geom_abline(color = "red") +
  ggplot2::coord_fixed()
# Model summary:
summary(birds_PresentHabSd_lm)
car::Anova(birds_PresentHabSd_lm)


# BIRDS - GAMs:

# Test GAMs - dim1 + dim2 + dim3:
# s() as modelise non-linear relationships:
birds_PresentHabSd_gam <- mgcv::gam(ses_birds ~ s(PresentHabSd_dim1)+
                                        s(PresentHabSd_dim2) +
                                        s(PresentHabSd_dim3),
                                      data = relationsh_ses_faith_df)
# Check model:
plot(birds_PresentHabSd_gam, pages = 1)  # Check smooth functions
par(mfrow = c(2,2))
mgcv::gam.check(birds_PresentHabSd_gam)
par(mfrow = c(1,1))
qqnorm(resid(birds_PresentHabSd_gam))
qqline(resid(birds_PresentHabSd_gam))



# REPTILES - GLM:

# Test Linear Regression - dim 1 + dim 2 + dim 3:
reptiles_PresentHabSd_lm <- lm(ses_reptiles ~ PresentHabSd_dim1 +
                                   PresentHabSd_dim2 +
                                   PresentHabSd_dim3,
                                 data = relationsh_ses_faith_df)
# General tests model:
performance::check_model(reptiles_PresentHabSd_lm)
# Test normality residuals: YES
shapiro.test(rstandard(reptiles_PresentHabSd_lm))
# QQplot:
ggplot2::ggplot() +
  ggplot2::geom_qq(ggplot2::aes(sample = rstandard(reptiles_PresentHabSd_lm))) +
  ggplot2::geom_abline(color = "red") +
  ggplot2::coord_fixed()
# Model summary:
summary(reptiles_PresentHabSd_lm)
car::Anova(reptiles_PresentHabSd_lm)


# REPTILES - GAMs:

# Test GAMs - dim1 + dim2 + dim3:
# s() as modelise non-linear relationships:
reptiles_PresentHabSd_gam <- mgcv::gam(ses_reptiles ~ s(PresentHabSd_dim1)+
                                           s(PresentHabSd_dim2)+
                                           s(PresentHabSd_dim3),
                                         data = relationsh_ses_faith_df)
# Check model:
plot(reptiles_PresentHabSd_gam, pages = 1)  # Check smooth functions
par(mfrow = c(2,2))
mgcv::gam.check(reptiles_PresentHabSd_gam)
par(mfrow = c(1,1))
qqnorm(resid(reptiles_PresentHabSd_gam))
qqline(resid(reptiles_PresentHabSd_gam))


# TREES - GLM:

# Test Linear Regression - dim 1 + dim 2 + dim 3:
trees_PresentHabSd_lm <- lm(ses_trees ~ PresentHabSd_dim1 +
                                PresentHabSd_dim2 +
                                PresentHabSd_dim3,
                              data = relationsh_ses_faith_df)
# General tests model:
performance::check_model(trees_PresentHabSd_lm)
# Test normality residuals: NO
shapiro.test(rstandard(trees_PresentHabSd_lm))
# QQplot:
ggplot2::ggplot() +
  ggplot2::geom_qq(ggplot2::aes(sample = rstandard(trees_PresentHabSd_lm))) +
  ggplot2::geom_abline(color = "red") +
  ggplot2::coord_fixed()
# Model summary:
summary(trees_PresentHabSd_lm)
car::Anova(trees_PresentHabSd_lm)


# TREES - GAMs:

# Test GAMs - dim1 + dim2 + dim3:
# s() as modelise non-linear relationships:
trees_PresentHabSd_gam <- mgcv::gam(ses_trees ~ s(PresentHabSd_dim1)+
                                        s(PresentHabSd_dim2) +
                                        s(PresentHabSd_dim3),
                                      data = relationsh_ses_faith_df)
# Check model:
plot(trees_PresentHabSd_gam, pages = 1)  # Check smooth functions
par(mfrow = c(2,2))
mgcv::gam.check(trees_PresentHabSd_gam)
par(mfrow = c(1,1))
qqnorm(resid(trees_PresentHabSd_gam))
qqline(resid(trees_PresentHabSd_gam))


# 9 - Relationship between synthetic variables for fire and Faith PD ======


# Histograms for drivers var:
hist(driv_db$Fire_dim1)
hist(driv_db$Fire_dim2)

# Histograms for PD Faith:
hist(relationsh_ses_faith_df$ses_birds)
hist(relationsh_ses_faith_df$ses_reptiles)
hist(relationsh_ses_faith_df$ses_trees)


# Plot against Faith's PD:
# BIRDS:
plot(relationsh_ses_faith_df$ses_birds,
     relationsh_ses_faith_df$Fire_dim1)
plot(relationsh_ses_faith_df$ses_birds,
     relationsh_ses_faith_df$Fire_dim2)
# REPTILES:
plot(relationsh_ses_faith_df$ses_reptiles,
     relationsh_ses_faith_df$Fire_dim1)
plot(relationsh_ses_faith_df$ses_reptiles,
     relationsh_ses_faith_df$Fire_dim2)
# TREES:
plot(relationsh_ses_faith_df$ses_trees,
     relationsh_ses_faith_df$Fire_dim1)
plot(relationsh_ses_faith_df$ses_trees,
     relationsh_ses_faith_df$Fire_dim2)


# BIRDS - GLM:

# Test Linear Regression - dim 1 + dim 2:
birds_fire_lm <- lm(ses_birds ~ Fire_dim1 +
                            Fire_dim2,
                          data = relationsh_ses_faith_df)
# General tests model:
performance::check_model(birds_fire_lm)
# Test normality residuals: NO
shapiro.test(rstandard(birds_fire_lm))
# QQplot:
ggplot2::ggplot() +
  ggplot2::geom_qq(ggplot2::aes(sample = rstandard(birds_fire_lm))) +
  ggplot2::geom_abline(color = "red") +
  ggplot2::coord_fixed()
# Model summary:
summary(birds_fire_lm)
car::Anova(birds_fire_lm)


# BIRDS - GAMs:

# Test GAMs - dim1 + dim2:
# s() as modelise non-linear relationships:
birds_fire_gam <- mgcv::gam(ses_birds ~ s(Fire_dim1)+
                                    s(Fire_dim2),
                                  data = relationsh_ses_faith_df)
# Check model:
plot(birds_fire_gam, pages = 1)  # Check smooth functions
par(mfrow = c(2,2))
mgcv::gam.check(birds_fire_gam)
par(mfrow = c(1,1))
qqnorm(resid(birds_fire_gam))
qqline(resid(birds_fire_gam))



# REPTILES - GLM:

# Test Linear Regression - dim 1 + dim 2:
reptiles_fire_lm <- lm(ses_reptiles ~ Fire_dim1 +
                               Fire_dim2,
                             data = relationsh_ses_faith_df)
# General tests model:
performance::check_model(reptiles_fire_lm)
# Test normality residuals: NO
shapiro.test(rstandard(reptiles_fire_lm))
# QQplot:
ggplot2::ggplot() +
  ggplot2::geom_qq(ggplot2::aes(sample = rstandard(reptiles_fire_lm))) +
  ggplot2::geom_abline(color = "red") +
  ggplot2::coord_fixed()
# Model summary:
summary(reptiles_fire_lm)
car::Anova(reptiles_fire_lm)


# REPTILES - GAMs:

# Test GAMs - dim1 + dim2:
# s() as modelise non-linear relationships:
reptiles_fire_gam <- mgcv::gam(ses_reptiles ~ s(Fire_dim1)+
                                       s(Fire_dim2),
                                     data = relationsh_ses_faith_df)
# Check model:
plot(reptiles_fire_gam, pages = 1)  # Check smooth functions
par(mfrow = c(2,2))
mgcv::gam.check(reptiles_fire_gam)
par(mfrow = c(1,1))
qqnorm(resid(reptiles_fire_gam))
qqline(resid(reptiles_fire_gam))


# TREES - GLM:

# Test Linear Regression - dim 1 + dim 2:
trees_fire_lm <- lm(ses_trees ~ Fire_dim1 +
                            Fire_dim2,
                          data = relationsh_ses_faith_df)
# General tests model:
performance::check_model(trees_fire_lm)
# Test normality residuals: NO
shapiro.test(rstandard(trees_fire_lm))
# QQplot:
ggplot2::ggplot() +
  ggplot2::geom_qq(ggplot2::aes(sample = rstandard(trees_fire_lm))) +
  ggplot2::geom_abline(color = "red") +
  ggplot2::coord_fixed()
# Model summary:
summary(trees_fire_lm)
car::Anova(trees_fire_lm)


# TREES - GAMs:

# Test GAMs - dim1 + dim2:
# s() as modelise non-linear relationships:
trees_fire_gam <- mgcv::gam(ses_trees ~ s(Fire_dim1)+
                                    s(Fire_dim2),
                                  data = relationsh_ses_faith_df)
# Check model:
plot(trees_fire_gam, pages = 1)  # Check smooth functions
par(mfrow = c(2,2))
mgcv::gam.check(trees_fire_gam)
par(mfrow = c(1,1))
qqnorm(resid(trees_fire_gam))
qqline(resid(trees_fire_gam))



# 10 - Relationship between synthetic variables for present lu and Faith PD ======


# Histograms for drivers var:
hist(driv_db$PresentLandUse_dim1)
hist(driv_db$PresentLandUse_dim2)
hist(driv_db$PresentLandUse_dim3)
hist(driv_db$PresentLandUse_dim4)


# Histograms for PD Faith:
hist(relationsh_ses_faith_df$ses_birds)
hist(relationsh_ses_faith_df$ses_reptiles)
hist(relationsh_ses_faith_df$ses_trees)


# Plot against Faith's PD:
# BIRDS:
plot(relationsh_ses_faith_df$ses_birds,
     relationsh_ses_faith_df$PresentLandUse_dim1)
plot(relationsh_ses_faith_df$ses_birds,
     relationsh_ses_faith_df$PresentLandUse_dim2)
plot(relationsh_ses_faith_df$ses_birds,
     relationsh_ses_faith_df$PresentLandUse_dim3)
plot(relationsh_ses_faith_df$ses_birds,
     relationsh_ses_faith_df$PresentLandUse_dim4)
# REPTILES:
plot(relationsh_ses_faith_df$ses_reptiles,
     relationsh_ses_faith_df$PresentLandUse_dim1)
plot(relationsh_ses_faith_df$ses_reptiles,
     relationsh_ses_faith_df$PresentLandUse_dim2)
plot(relationsh_ses_faith_df$ses_reptiles,
     relationsh_ses_faith_df$PresentLandUse_dim3)
plot(relationsh_ses_faith_df$ses_reptiles,
     relationsh_ses_faith_df$PresentLandUse_dim4)
# TREES:
plot(relationsh_ses_faith_df$ses_trees,
     relationsh_ses_faith_df$PresentLandUse_dim1)
plot(relationsh_ses_faith_df$ses_trees,
     relationsh_ses_faith_df$PresentLandUse_dim2)
plot(relationsh_ses_faith_df$ses_trees,
     relationsh_ses_faith_df$PresentLandUse_dim3)
plot(relationsh_ses_faith_df$ses_trees,
     relationsh_ses_faith_df$PresentLandUse_dim4)


# BIRDS - GLM:

# Test Linear Regression - dim 1 + dim 2 + dim 3 + dim 4:
birds_PresentLandUse_lm <- lm(ses_birds ~ PresentLandUse_dim1 +
                              PresentLandUse_dim2 +
                              PresentLandUse_dim3 +
                              PresentLandUse_dim4,
                            data = relationsh_ses_faith_df)
# General tests model:
performance::check_model(birds_PresentLandUse_lm)
# Test normality residuals: YES
shapiro.test(rstandard(birds_PresentLandUse_lm))
# QQplot:
ggplot2::ggplot() +
  ggplot2::geom_qq(ggplot2::aes(sample = rstandard(birds_PresentLandUse_lm))) +
  ggplot2::geom_abline(color = "red") +
  ggplot2::coord_fixed()
# Model summary:
summary(birds_PresentLandUse_lm)
car::Anova(birds_PresentLandUse_lm)


# BIRDS - GAMs:

# Test GAMs - dim1 + dim2 + dim3 + dim4:
# s() as modelise non-linear relationships:
birds_PresentLandUse_gam <- mgcv::gam(ses_birds ~ s(PresentLandUse_dim1)+
                                      s(PresentLandUse_dim2) +
                                      s(PresentLandUse_dim3) +
                                      s(PresentLandUse_dim4),
                                    data = relationsh_ses_faith_df)
# Check model:
plot(birds_PresentLandUse_gam, pages = 1)  # Check smooth functions
par(mfrow = c(2,2))
mgcv::gam.check(birds_PresentLandUse_gam)
par(mfrow = c(1,1))
qqnorm(resid(birds_PresentLandUse_gam))
qqline(resid(birds_PresentLandUse_gam))



# REPTILES - GLM:

# Test Linear Regression - dim 1 + dim 2 + dim 3:
reptiles_PresentLandUse_lm <- lm(ses_reptiles ~ PresentLandUse_dim1 +
                                 PresentLandUse_dim2 +
                                 PresentLandUse_dim3 +
                                 PresentLandUse_dim4,
                               data = relationsh_ses_faith_df)
# General tests model:
performance::check_model(reptiles_PresentLandUse_lm)
# Test normality residuals: YES
shapiro.test(rstandard(reptiles_PresentLandUse_lm))
# QQplot:
ggplot2::ggplot() +
  ggplot2::geom_qq(ggplot2::aes(sample = rstandard(reptiles_PresentLandUse_lm))) +
  ggplot2::geom_abline(color = "red") +
  ggplot2::coord_fixed()
# Model summary:
summary(reptiles_PresentLandUse_lm)
car::Anova(reptiles_PresentLandUse_lm)


# REPTILES - GAMs:

# Test GAMs - dim1 + dim2 + dim3 + dim4:
# s() as modelise non-linear relationships:
reptiles_PresentLandUse_gam <- mgcv::gam(ses_reptiles ~ s(PresentLandUse_dim1)+
                                         s(PresentLandUse_dim2)+
                                         s(PresentLandUse_dim3)+
                                         s(PresentLandUse_dim4),
                                       data = relationsh_ses_faith_df)
# Check model:
plot(reptiles_PresentLandUse_gam, pages = 1)  # Check smooth functions
par(mfrow = c(2,2))
mgcv::gam.check(reptiles_PresentLandUse_gam)
par(mfrow = c(1,1))
qqnorm(resid(reptiles_PresentLandUse_gam))
qqline(resid(reptiles_PresentLandUse_gam))


# TREES - GLM:

# Test Linear Regression - dim 1 + dim 2 + dim 3 + dim 4:
trees_PresentLandUse_lm <- lm(ses_trees ~ PresentLandUse_dim1 +
                              PresentLandUse_dim2 +
                              PresentLandUse_dim3 +
                              PresentLandUse_dim4,
                            data = relationsh_ses_faith_df)
# General tests model:
performance::check_model(trees_PresentLandUse_lm)
# Test normality residuals: NO
shapiro.test(rstandard(trees_PresentLandUse_lm))
# QQplot:
ggplot2::ggplot() +
  ggplot2::geom_qq(ggplot2::aes(sample = rstandard(trees_PresentLandUse_lm))) +
  ggplot2::geom_abline(color = "red") +
  ggplot2::coord_fixed()
# Model summary:
summary(trees_PresentLandUse_lm)
car::Anova(trees_PresentLandUse_lm)


# TREES - GAMs:

# Test GAMs - dim1 + dim2 + dim3 + dim4:
# s() as modelise non-linear relationships:
trees_PresentLandUse_gam <- mgcv::gam(ses_trees ~ s(PresentLandUse_dim1)+
                                      s(PresentLandUse_dim2) +
                                      s(PresentLandUse_dim3) +
                                      s(PresentLandUse_dim4),
                                    data = relationsh_ses_faith_df)
# Check model:
plot(trees_PresentLandUse_gam, pages = 1)  # Check smooth functions
par(mfrow = c(2,2))
mgcv::gam.check(trees_PresentLandUse_gam)
par(mfrow = c(1,1))
qqnorm(resid(trees_PresentLandUse_gam))
qqline(resid(trees_PresentLandUse_gam))


# 11 - Relationship between synthetic variables for past lu and Faith PD ======


# Histograms for drivers var:
hist(driv_db$PastLandUse_dim1)
hist(driv_db$PastLandUse_dim2)
hist(driv_db$PastLandUse_dim3)
hist(driv_db$PastLandUse_dim4)


# Histograms for PD Faith:
hist(relationsh_ses_faith_df$ses_birds)
hist(relationsh_ses_faith_df$ses_reptiles)
hist(relationsh_ses_faith_df$ses_trees)


# Plot against Faith's PD:
# BIRDS:
plot(relationsh_ses_faith_df$ses_birds,
     relationsh_ses_faith_df$PastLandUse_dim1)
plot(relationsh_ses_faith_df$ses_birds,
     relationsh_ses_faith_df$PastLandUse_dim2)
plot(relationsh_ses_faith_df$ses_birds,
     relationsh_ses_faith_df$PastLandUse_dim3)
plot(relationsh_ses_faith_df$ses_birds,
     relationsh_ses_faith_df$PastLandUse_dim4)
# REPTILES:
plot(relationsh_ses_faith_df$ses_reptiles,
     relationsh_ses_faith_df$PastLandUse_dim1)
plot(relationsh_ses_faith_df$ses_reptiles,
     relationsh_ses_faith_df$PastLandUse_dim2)
plot(relationsh_ses_faith_df$ses_reptiles,
     relationsh_ses_faith_df$PastLandUse_dim3)
plot(relationsh_ses_faith_df$ses_reptiles,
     relationsh_ses_faith_df$PastLandUse_dim4)
# TREES:
plot(relationsh_ses_faith_df$ses_trees,
     relationsh_ses_faith_df$PastLandUse_dim1)
plot(relationsh_ses_faith_df$ses_trees,
     relationsh_ses_faith_df$PastLandUse_dim2)
plot(relationsh_ses_faith_df$ses_trees,
     relationsh_ses_faith_df$PastLandUse_dim3)
plot(relationsh_ses_faith_df$ses_trees,
     relationsh_ses_faith_df$PastLandUse_dim4)


# BIRDS - GLM:

# Test Linear Regression - dim 1 + dim 2 + dim 3 + dim 4:
birds_PastLandUse_lm <- lm(ses_birds ~ PastLandUse_dim1 +
                                PastLandUse_dim2 +
                                PastLandUse_dim3 +
                                PastLandUse_dim4,
                              data = relationsh_ses_faith_df)
# General tests model:
performance::check_model(birds_PastLandUse_lm)
# Test normality residuals: YES
shapiro.test(rstandard(birds_PastLandUse_lm))
# QQplot:
ggplot2::ggplot() +
  ggplot2::geom_qq(ggplot2::aes(sample = rstandard(birds_PastLandUse_lm))) +
  ggplot2::geom_abline(color = "red") +
  ggplot2::coord_fixed()
# Model summary:
summary(birds_PastLandUse_lm)
car::Anova(birds_PastLandUse_lm)


# BIRDS - GAMs:

# Test GAMs - dim1 + dim2 + dim3 + dim4:
# s() as modelise non-linear relationships:
birds_PastLandUse_gam <- mgcv::gam(ses_birds ~ s(PastLandUse_dim1)+
                                        s(PastLandUse_dim2) +
                                        s(PastLandUse_dim3) +
                                        s(PastLandUse_dim4),
                                      data = relationsh_ses_faith_df)
# Check model:
plot(birds_PastLandUse_gam, pages = 1)  # Check smooth functions
par(mfrow = c(2,2))
mgcv::gam.check(birds_PastLandUse_gam)
par(mfrow = c(1,1))
qqnorm(resid(birds_PastLandUse_gam))
qqline(resid(birds_PastLandUse_gam))



# REPTILES - GLM:

# Test Linear Regression - dim 1 + dim 2 + dim 3:
reptiles_PastLandUse_lm <- lm(ses_reptiles ~ PastLandUse_dim1 +
                                   PastLandUse_dim2 +
                                   PastLandUse_dim3 +
                                   PastLandUse_dim4,
                                 data = relationsh_ses_faith_df)
# General tests model:
performance::check_model(reptiles_PastLandUse_lm)
# Test normality residuals: YES
shapiro.test(rstandard(reptiles_PastLandUse_lm))
# QQplot:
ggplot2::ggplot() +
  ggplot2::geom_qq(ggplot2::aes(sample = rstandard(reptiles_PastLandUse_lm))) +
  ggplot2::geom_abline(color = "red") +
  ggplot2::coord_fixed()
# Model summary:
summary(reptiles_PastLandUse_lm)
car::Anova(reptiles_PastLandUse_lm)


# REPTILES - GAMs:

# Test GAMs - dim1 + dim2 + dim3 + dim4:
# s() as modelise non-linear relationships:
reptiles_PastLandUse_gam <- mgcv::gam(ses_reptiles ~ s(PastLandUse_dim1)+
                                           s(PastLandUse_dim2)+
                                           s(PastLandUse_dim3)+
                                           s(PastLandUse_dim4),
                                         data = relationsh_ses_faith_df)
# Check model:
plot(reptiles_PastLandUse_gam, pages = 1)  # Check smooth functions
par(mfrow = c(2,2))
mgcv::gam.check(reptiles_PastLandUse_gam)
par(mfrow = c(1,1))
qqnorm(resid(reptiles_PastLandUse_gam))
qqline(resid(reptiles_PastLandUse_gam))


# TREES - GLM:

# Test Linear Regression - dim 1 + dim 2 + dim 3 + dim 4:
trees_PastLandUse_lm <- lm(ses_trees ~ PastLandUse_dim1 +
                                PastLandUse_dim2 +
                                PastLandUse_dim3 +
                                PastLandUse_dim4,
                              data = relationsh_ses_faith_df)
# General tests model:
performance::check_model(trees_PastLandUse_lm)
# Test normality residuals: NO
shapiro.test(rstandard(trees_PastLandUse_lm))
# QQplot:
ggplot2::ggplot() +
  ggplot2::geom_qq(ggplot2::aes(sample = rstandard(trees_PastLandUse_lm))) +
  ggplot2::geom_abline(color = "red") +
  ggplot2::coord_fixed()
# Model summary:
summary(trees_PastLandUse_lm)
car::Anova(trees_PastLandUse_lm)


# TREES - GAMs:

# Test GAMs - dim1 + dim2 + dim3 + dim4:
# s() as modelise non-linear relationships:
trees_PastLandUse_gam <- mgcv::gam(ses_trees ~ s(PastLandUse_dim1)+
                                        s(PastLandUse_dim2) +
                                        s(PastLandUse_dim3) +
                                        s(PastLandUse_dim4),
                                      data = relationsh_ses_faith_df)
# Check model:
plot(trees_PastLandUse_gam, pages = 1)  # Check smooth functions
par(mfrow = c(2,2))
mgcv::gam.check(trees_PastLandUse_gam)
par(mfrow = c(1,1))
qqnorm(resid(trees_PastLandUse_gam))
qqline(resid(trees_PastLandUse_gam))


# 12 - Relationship between synthetic variables for pop and Faith PD ======


# Histograms for drivers var:
hist(driv_db$Pr_Pop_2020_mean)
hist(driv_db$Pr_RatePop_2020_mean)

# Histograms for PD Faith:
hist(relationsh_ses_faith_df$ses_birds)
hist(relationsh_ses_faith_df$ses_reptiles)
hist(relationsh_ses_faith_df$ses_trees)


# Plot against Faith's PD:
# BIRDS:
plot(relationsh_ses_faith_df$ses_birds,
     relationsh_ses_faith_df$Pr_Pop_2020_mean)
plot(relationsh_ses_faith_df$ses_birds,
     relationsh_ses_faith_df$Pr_RatePop_2020_mean)
# REPTILES:
plot(relationsh_ses_faith_df$ses_reptiles,
     relationsh_ses_faith_df$Pr_Pop_2020_mean)
plot(relationsh_ses_faith_df$ses_reptiles,
     relationsh_ses_faith_df$Pr_RatePop_2020_mean)
# TREES:
plot(relationsh_ses_faith_df$ses_trees,
     relationsh_ses_faith_df$Pr_Pop_2020_mean)
plot(relationsh_ses_faith_df$ses_trees,
     relationsh_ses_faith_df$Pr_RatePop_2020_mean)


# BIRDS - GLM:

# Test Linear Regression - dim 1 + dim 2:
birds_fire_lm <- lm(ses_birds ~ Pr_Pop_2020_mean +
                      Pr_RatePop_2020_mean,
                    data = relationsh_ses_faith_df)
# General tests model:
performance::check_model(birds_fire_lm)
# Test normality residuals: YES
shapiro.test(rstandard(birds_fire_lm))
# QQplot:
ggplot2::ggplot() +
  ggplot2::geom_qq(ggplot2::aes(sample = rstandard(birds_fire_lm))) +
  ggplot2::geom_abline(color = "red") +
  ggplot2::coord_fixed()
# Model summary:
summary(birds_fire_lm)
car::Anova(birds_fire_lm)


# BIRDS - GAMs:

# Test GAMs - dim1 + dim2:
# s() as modelise non-linear relationships:
birds_fire_gam <- mgcv::gam(ses_birds ~ s(Pr_Pop_2020_mean)+
                              s(Pr_RatePop_2020_mean),
                            data = relationsh_ses_faith_df)
# Check model:
plot(birds_fire_gam, pages = 1)  # Check smooth functions
par(mfrow = c(2,2))
mgcv::gam.check(birds_fire_gam)
par(mfrow = c(1,1))
qqnorm(resid(birds_fire_gam))
qqline(resid(birds_fire_gam))



# REPTILES - GLM:

# Test Linear Regression - dim 1 + dim 2:
reptiles_fire_lm <- lm(ses_reptiles ~ Pr_Pop_2020_mean +
                         Pr_RatePop_2020_mean,
                       data = relationsh_ses_faith_df)
# General tests model:
performance::check_model(reptiles_fire_lm)
# Test normality residuals: NO
shapiro.test(rstandard(reptiles_fire_lm))
# QQplot:
ggplot2::ggplot() +
  ggplot2::geom_qq(ggplot2::aes(sample = rstandard(reptiles_fire_lm))) +
  ggplot2::geom_abline(color = "red") +
  ggplot2::coord_fixed()
# Model summary:
summary(reptiles_fire_lm)
car::Anova(reptiles_fire_lm)


# REPTILES - GAMs:

# Test GAMs - dim1 + dim2:
# s() as modelise non-linear relationships:
reptiles_fire_gam <- mgcv::gam(ses_reptiles ~ s(Pr_Pop_2020_mean)+
                                 s(Pr_RatePop_2020_mean),
                               data = relationsh_ses_faith_df)
# Check model:
plot(reptiles_fire_gam, pages = 1)  # Check smooth functions
par(mfrow = c(2,2))
mgcv::gam.check(reptiles_fire_gam)
par(mfrow = c(1,1))
qqnorm(resid(reptiles_fire_gam))
qqline(resid(reptiles_fire_gam))


# TREES - GLM:

# Test Linear Regression - dim 1 + dim 2:
trees_fire_lm <- lm(ses_trees ~ Pr_Pop_2020_mean +
                      Pr_RatePop_2020_mean,
                    data = relationsh_ses_faith_df)
# General tests model:
performance::check_model(trees_fire_lm)
# Test normality residuals: NO
shapiro.test(rstandard(trees_fire_lm))
# QQplot:
ggplot2::ggplot() +
  ggplot2::geom_qq(ggplot2::aes(sample = rstandard(trees_fire_lm))) +
  ggplot2::geom_abline(color = "red") +
  ggplot2::coord_fixed()
# Model summary:
summary(trees_fire_lm)
car::Anova(trees_fire_lm)


# TREES - GAMs:

# Test GAMs - dim1 + dim2:
# s() as modelise non-linear relationships:
trees_fire_gam <- mgcv::gam(ses_trees ~ s(Pr_Pop_2020_mean)+
                              s(Pr_RatePop_2020_mean),
                            data = relationsh_ses_faith_df)
# Check model:
plot(trees_fire_gam, pages = 1)  # Check smooth functions
par(mfrow = c(2,2))
mgcv::gam.check(trees_fire_gam)
par(mfrow = c(1,1))
qqnorm(resid(trees_fire_gam))
qqline(resid(trees_fire_gam))

