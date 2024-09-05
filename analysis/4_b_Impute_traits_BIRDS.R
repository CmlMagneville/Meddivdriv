################################################################################
##
## Script to impute traits based on missForests - for BIRDS
##
## Camille Magneville
##
## 15/04/2024 - 09/2024
##
## 4_a_Impute_traits_BIRDS.R
##
################################################################################


# Define the pipe symbol so I can use it:
`%>%` <- magrittr::`%>%`


# 1 - Load data ========================================================


sp_tr_BIRDS <- readRDS(here::here("transformed_data",
                                  "raw_traits_BIRDS.rds"))


# 2 - Traits and table in the right format ==============================


# Check that traits are in the right format:
str(sp_tr_BIRDS)

# See missing values:
# Change first column name for funbiogeo use:
funbiogeo_df <- dplyr::rename(sp_tr_BIRDS, species = Species)

# Get the percentage of completedness per traits:
funbiogeo::fb_plot_number_species_by_trait(funbiogeo_df)

# Get the percentage of species with for than 1, 2, 3 etc traits:
funbiogeo::fb_plot_number_traits_by_species(funbiogeo_df)

# Species as rownames:
sp_tr_BIRDS <- tibble::column_to_rownames(sp_tr_BIRDS,
                                          "Species")




# 3 - Impute traits based on Random Forest approach =====================


set.seed(42)

# Impute traits and check quality - with mice pkge:
## Check missing traits:
mice::md.pattern(sp_tr_BIRDS)
## Compute missing data:
init_test <- mice::mice(sp_tr_BIRDS, ntree = 500,
                        m = 6,
                        meth = 'rf', seed = 42)
summary(init_test)
complete_data <- mice::complete(init_test, 1)
plot(init_test)

# Prefer for the imputed data to be plausible values, i.e. values that could
# ... have been observed if they had not been missing.
mice::stripplot(init_test, LongevityMax ~ .imp, pch = 20, cex = 2)
mice::stripplot(init_test, FirstBreedingAge ~ .imp, pch = 20, cex = 2)
mice::stripplot(init_test, ReproPerYear ~ .imp, pch = 20, cex = 2)
mice::stripplot(init_test, FledgingPeriod ~ .imp, pch = 20, cex = 2)
mice::stripplot(init_test, OffspringPerRepro ~ .imp, pch = 20, cex = 2)
mice::stripplot(init_test)

# Get the imputed values for each iteration?
# If the objective of the imputation is to produce estimates of missing
# values (for example, to fill gaps in a dataset), single imputation is
# considered most effective, because the stochastic draws in multiple
# imputation add error (Van Buuren, 2012)
iter_data <- mice::complete(init_test, action = "long")


# Save imputed traits:
saveRDS(complete_data, here::here("transformed_data",
                                  "final_traits_BIRDS.rds"))



