################################################################################
##
## Script to impute traits based on missForests - for TREES
##
## Camille Magneville
##
## 12/04/2024 - 09/2024
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

# Check traits in the right format:
class(sp_tr_TREES$BloomBreadth)
class(sp_tr_TREES$DispDist)
class(sp_tr_TREES$HeightMax)
class(sp_tr_TREES$LA)
class(sp_tr_TREES$LeafOutline)
class(sp_tr_TREES$LeafPhenology)
class(sp_tr_TREES$Pollination)
class(sp_tr_TREES$SLA)
class(sp_tr_TREES$SeedMass)
class(sp_tr_TREES$SexualSystem)
class(sp_tr_TREES$StemSpecDens)

# See missing values:
# Change first column name for funbiogeo use:
funbiogeo_df <- dplyr::rename(sp_tr_TREES, species = Species)

# Get the percentage of completedness per traits:
funbiogeo::fb_plot_number_species_by_trait(funbiogeo_df)

# Get the percentage of species with for than 1, 2, 3 etc traits:
funbiogeo::fb_plot_number_traits_by_species(funbiogeo_df)

# Species as rownames:
sp_tr_TREES <- tibble::column_to_rownames(sp_tr_TREES,
                                          "Species")

# 3 - Impute traits based on Random Forest approach =====================


set.seed(42)

# Impute traits and check quality - with mice pkge:
## Check missing traits:
mice::md.pattern(sp_tr_TREES)
## Compute missing data:
init_test <- mice::mice(sp_tr_TREES, ntree = 300,
                        m = 5,
                        meth = 'rf', seed = 42)
summary(init_test)
plot(init_test)

# How are the 5 imputation similar or dissimilar?
# Have a look at which dataset is closer to the mean value of each variable?
summary(sp_tr_TREES)
init_test$imp$StemSpecDens
init_test$imp$BloomBreadth
init_test$imp$SLA
init_test$imp$SeedMass
init_test$imp$LA


# prefer for the imputed data to be plausible values, i.e. values that could
# ... have been observed if they had not been missing.
mice::stripplot(init_test, StemSpecDens ~ .imp, pch = 20, cex = 2)
mice::stripplot(init_test, SLA ~ .imp, pch = 20, cex = 2)
mice::stripplot(init_test, SeedMass ~ .imp, pch = 20, cex = 2)
mice::stripplot(init_test, LA ~ .imp, pch = 20, cex = 2)
mice::stripplot(init_test, BloomBreadth ~ .imp, pch = 20, cex = 2)

# Can also check that relatioships between variables are not changed:
# 1 plot only raw data and other plot, repetitions of imputated (red) and raw (blue)
mice::xyplot(init_test, StemSpecDens ~ SLA | .imp,
             pch = 20, cex = 2)
mice::xyplot(init_test, StemSpecDens ~ SeedMass | .imp,
             pch = 20, cex = 2)
mice::xyplot(init_test, StemSpecDens ~ LA | .imp,
             pch = 20, cex = 2)
mice::xyplot(init_test, StemSpecDens ~ BloomBreadth | .imp,
             pch = 20, cex = 2)
mice::xyplot(init_test, SLA ~ SeedMass | .imp,
             pch = 20, cex = 2)
mice::xyplot(init_test, SLA ~ LA | .imp,
             pch = 20, cex = 2)
mice::xyplot(init_test, SLA ~ BloomBreadth | .imp,
             pch = 20, cex = 2)
mice::xyplot(init_test, SeedMass ~ LA | .imp,
             pch = 20, cex = 2)
mice::xyplot(init_test, SeedMass ~ BloomBreadth | .imp,
             pch = 20, cex = 2)


# Get the imputed values for each iteration?
# If the objective of the imputation is to produce estimates of missing
# values (for example, to fill gaps in a dataset), single imputation is
# considered most effective, because the stochastic draws in multiple
# imputation add error (Van Buuren, 2012)
iter_data <- mice::complete(init_test, action = "long")
complete_data <- mice::complete(init_test, 2)


# Save imputed traits:
saveRDS(complete_data, here::here("transformed_data",
                                  "final_traits_TREES.rds"))

