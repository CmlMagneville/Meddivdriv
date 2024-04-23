################################################################################
##
## Script to impute traits based on missForests - for TREES
##
## Camille Magneville
##
## 12/04/2024
##
## 4_a_Impute_traits_TREES.R
##
################################################################################


# Define the pipe symbol so I can use it:
`%>%` <- magrittr::`%>%`


# 1 - Load data ========================================================


sp_tr_TREES <- readRDS(here::here("transformed_data",
                                  "raw_traits_TREES.rds"))


# 2 - Traits and table in the right format ==============================


sp_tr_TREES$LA <- as.numeric(sp_tr_TREES$LA)
sp_tr_TREES$SeedMass <- as.numeric(sp_tr_TREES$SeedMass)
sp_tr_TREES$SLA <- as.numeric(sp_tr_TREES$SLA)
sp_tr_TREES$StemSpecDens <- as.numeric(sp_tr_TREES$StemSpecDens)
sp_tr_TREES$HeightMax <- as.numeric(sp_tr_TREES$HeightMax)

# Species as rownames:
sp_tr_TREES <- tibble::column_to_rownames(sp_tr_TREES, "Species")


# 3 - Test if ok to impute traits - cross validation ===========================

cross_val_results <- test.mf(raw_sp_tr = sp_tr_TREES)


# 4 - Impute traits based on Random Forest approach =====================

set.seed(42)

# Impute traits and check NRMSE - with missForest R pkge:
imputed_traits_TREES_test <- missForest::missForest(sp_tr_TREES,
                                                    maxiter = 100,
                                                    ntree = 200,
                                                    variablewise = TRUE)
try <- imputed_traits_TREES_test$ximp
error <- imputed_traits_TREES_test$OOBerror
# get global OOB error:
imputed_traits_TREES_test <- missForest::missForest(sp_tr_TREES,
                                                    maxiter = 100,
                                                    ntree = 200)
try <- imputed_traits_TREES_test$ximp
error <- imputed_traits_TREES_test$OOBerror


# Impute traits and check quality - with mice pkge:
## Check missing traits:
mice::md.pattern(sp_tr_TREES)
## Compute missing data m times:
init_test <- mice::mice(sp_tr_TREES, ntree = 100,
                        m = 1,
                        meth = 'rf', seed = 42)
summary(init_test)

## Compare the distributions - test for each variable - should have a similar distrib:
mice::densityplot(init_test, ~ LA, lwd = 2)
mice::densityplot(init_test, ~ SLA, lwd = 2)
mice::densityplot(init_test, ~ SeedMass, lwd = 2)
mice::densityplot(init_test, ~ StemSpecDens, lwd = 2)
# prefer for the imputed data to be plausible values, i.e. values that could
# ... have been observed if they had not been missing.
mice::stripplot(init_test, LA ~ .imp, pch = 20, cex = 2)
mice::stripplot(init_test, SLA ~ .imp, pch = 20, cex = 2)
mice::stripplot(init_test, SeedMass ~ .imp, pch = 20, cex = 2)
mice::stripplot(init_test, StemSpecDens ~ .imp, pch = 20, cex = 2)
mice::stripplot(init_test)

# Look at the predictor matrix:
init_test$predictorMatrix


# Get the imputed values for each iteration?
# If the objective of the imputation is to produce estimates of missing
# values (for example, to fill gaps in a dataset), single imputation is
# considered most effective, because the stochastic draws in multiple
# imputation add error (Van Buuren, 2012)
iter_data <- mice::complete(init_test)

# Check whether the mean for each traits has changed:
colMeans(sp_tr_TREES, na.rm = TRUE)
colMeans(iter_data)

# Save imputed traits:
saveRDS(iter_data, here::here("transformed_data",
                                         "final_traits_TREES.rds"))



