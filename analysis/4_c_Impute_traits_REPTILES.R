################################################################################
##
## Script to impute traits based on missForests - for REPTILES
##
## Camille Magneville
##
## 18/04/2024 - 09/2024
##
## 4_a_Impute_traits_REPTILES.R
##
################################################################################


# Define the pipe symbol so I can use it:
`%>%` <- magrittr::`%>%`


# 1 - Load data ========================================================


sp_tr_REPTILES <- readRDS(here::here("transformed_data",
                                  "raw_traits_REPTILES.rds"))


# 2 - Traits and table in the right format ==============================


# Check that traits are in the right format:
str(sp_tr_REPTILES)

# See missing values:
# Change first column name for funbiogeo use:
funbiogeo_df <- dplyr::rename(sp_tr_REPTILES, species = Species)

# Get the percentage of completedness per traits:
funbiogeo::fb_plot_number_species_by_trait(funbiogeo_df)

# Get the percentage of species with for than 1, 2, 3 etc traits:
funbiogeo::fb_plot_number_traits_by_species(funbiogeo_df)

# Species as rownames:
sp_tr_REPTILES <- tibble::column_to_rownames(sp_tr_REPTILES,
                                          "Species")



# 3 - Impute traits based on Random Forest approach - non fuzzy ones ===========


set.seed(42)

# Sp tr df with no fuzzy traits:
sp_tr_no_fuzzy_REPTILES <- sp_tr_REPTILES[, c(1:10)]

# Impute traits and check quality - with mice pkge:
## Check missing traits:
mice::md.pattern(sp_tr_no_fuzzy_REPTILES)
## Compute missing data:
init_test <- mice::mice(sp_tr_no_fuzzy_REPTILES, ntree = 300,
                        m = 5,
                        meth = 'rf', seed = 42)
summary(init_test)

# How are the 5 imputation similar or dissimilar?
# Have a look at which dataset is closer to the mean value of each variable?
summary(sp_tr_no_fuzzy_REPTILES)
init_test$imp$ActivitySeasonLength
init_test$imp$FirstBreedingAge
init_test$imp$BodyTemperature
init_test$imp$ReproPerYear
init_test$imp$LongevityMax
init_test$imp$SVLMax
init_test$imp$OffspringPerRepro


# Prefer for the imputed data to be plausible values, i.e. values that could
# ... have been observed if they had not been missing.
mice::stripplot(init_test, LongevityMax ~ .imp, pch = 20, cex = 2)
mice::stripplot(init_test, FirstBreedingAge ~ .imp, pch = 20, cex = 2)
mice::stripplot(init_test, ReproPerYear ~ .imp, pch = 20, cex = 2)
mice::stripplot(init_test, SVLMax ~ .imp, pch = 20, cex = 2)
mice::stripplot(init_test, OffspringPerRepro ~ .imp, pch = 20, cex = 2)
mice::stripplot(init_test, ActivitySeasonLength ~ .imp, pch = 20, cex = 2)
mice::stripplot(init_test, BodyTemperature ~ .imp, pch = 20, cex = 2)

# Can also check that relatioships between variables are not changed:
# 1 plot only raw data and other plot, repetitions of imputated (red) and raw (blue)
mice::xyplot(init_test, LongevityMax ~ FirstBreedingAge | .imp,
             pch = 20, cex = 2)
mice::xyplot(init_test, LongevityMax ~ ReproPerYear | .imp,
             pch = 20, cex = 2)
mice::xyplot(init_test, LongevityMax ~ SVLMax | .imp,
             pch = 20, cex = 2)
mice::xyplot(init_test, LongevityMax ~ OffspringPerRepro | .imp,
             pch = 20, cex = 2)
mice::xyplot(init_test, LongevityMax ~ ActivitySeasonLength | .imp,
             pch = 20, cex = 2)
mice::xyplot(init_test, LongevityMax ~ BodyTemperature | .imp,
             pch = 20, cex = 2)
mice::xyplot(init_test, FirstBreedingAge ~ ReproPerYear | .imp,
             pch = 20, cex = 2)
mice::xyplot(init_test, FirstBreedingAge ~ SVLMax | .imp,
             pch = 20, cex = 2)
mice::xyplot(init_test, FirstBreedingAge ~ OffspringPerRepro | .imp,
             pch = 20, cex = 2)
mice::xyplot(init_test, FirstBreedingAge ~ ActivitySeasonLength | .imp,
             pch = 20, cex = 2)
mice::xyplot(init_test, FirstBreedingAge ~ BodyTemperature | .imp,
             pch = 20, cex = 2)
mice::xyplot(init_test, ReproPerYear ~ SVLMax | .imp,
             pch = 20, cex = 2)
mice::xyplot(init_test, ReproPerYear ~ OffspringPerRepro  | .imp,
             pch = 20, cex = 2)
mice::xyplot(init_test, ReproPerYear ~ ActivitySeasonLength  | .imp,
             pch = 20, cex = 2)
mice::xyplot(init_test, ReproPerYear ~ BodyTemperature  | .imp,
             pch = 20, cex = 2)
mice::xyplot(init_test, SVLMax ~ OffspringPerRepro  | .imp,
             pch = 20, cex = 2)
mice::xyplot(init_test, SVLMax ~ ActivitySeasonLength  | .imp,
             pch = 20, cex = 2)
mice::xyplot(init_test, SVLMax ~ BodyTemperature | .imp,
             pch = 20, cex = 2)
mice::xyplot(init_test, OffspringPerRepro ~ BodyTemperature | .imp,
             pch = 20, cex = 2)
mice::xyplot(init_test, OffspringPerRepro ~ ActivitySeasonLength | .imp,
             pch = 20, cex = 2)
mice::xyplot(init_test, BodyTemperature ~ ActivitySeasonLength | .imp,
             pch = 20, cex = 2)

# Get the imputed values for each iteration?
# If the objective of the imputation is to produce estimates of missing
# values (for example, to fill gaps in a dataset), single imputation is
# considered most effective, because the stochastic draws in multiple
# imputation add error (Van Buuren, 2012)
iter_data <- mice::complete(init_test, action = "long")
# Regarding the plots before, the "best" imputation looks like 4 or 5:
complete_data <- mice::complete(init_test, 5)

# Link with the fuzzy traits:
complete_data_REPTILES <- complete_data %>%
  tibble::rownames_to_column(var = "Species")
sp_tr_REPTILES <- sp_tr_REPTILES %>%
  tibble::rownames_to_column(var = "Species")
complete_data_REPTILES <- dplyr::left_join(complete_data_REPTILES,
                                           sp_tr_REPTILES[, c(1, 12:ncol(sp_tr_REPTILES))],
                                           by = "Species")

complete_data_REPTILES <- tibble::column_to_rownames(complete_data_REPTILES,
                                          "Species")

# Put fuzzy traits as numeric:
complete_data_REPTILES[, c(11:ncol(complete_data_REPTILES))] <- apply(complete_data_REPTILES[, c(11:ncol(complete_data_REPTILES))],
                                                                      2,
                                                                      as.numeric)

# Save imputed traits:
saveRDS(complete_data_REPTILES, here::here("transformed_data",
                                  "final_traits_REPTILES.rds"))



