################################################################################
##
## Script to run all the analysis from data collection and cleaning to ...
## ... models and graphs
##
## Camille Magneville
##
## 03/04/2024
##
## make.R
##
################################################################################


# Clean the environnement:
rm(list = ls(all.names = TRUE), envir = .GlobalEnv)

# Install dependencies:
devtools::install_deps()

# Load the functions so make them available for use:
devtools::load_all()

# Define the pipe symbol so I can use it:
`%>%` <- magrittr::`%>%`

# Install from local the sobol-MDA package for random forests:
remotes::install_gitlab("DRTI/sobolMDA")
install.packages(path_to_file = here::here("sobolmda-master.tar.gz"),
                            repos = NULL,
                            type = "source")
install.packages(here::here("sobolmda-master.zip"),
                  repos = NULL,
                  type = "source")
devtools::install_gitlab(repo = "drti/sobolmda")
