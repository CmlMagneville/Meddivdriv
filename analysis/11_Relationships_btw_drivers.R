################################################################################
##
## Script to study the relationships between drivers
##
## Camille Magneville
##
## 04/07/2024
##
## 11_Relationships_btw_drivers.R
##
################################################################################


# Define the pipe symbol so I can use it:
`%>%` <- magrittr::`%>%`


# 1 - Load environmental data ==================================================


# Load environmental drivers - PCA synthetic var (with no NA for predictors and only cells which
# .. have values for all the studied taxa):
driv_db <- readRDS(here::here("transformed_data", "env_db",
                              "SEM_env_db.rds"))


# 2 -
