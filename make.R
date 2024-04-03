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


