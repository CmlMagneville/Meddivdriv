################################################################################
##
## Script open the phylogeny from the integradiv database and check for
## ... potential issues - for TREES
##
## Camille Magneville
##
## 03/04/2024
##
## 1_a_Open_PD_data_TREES.R
##
################################################################################


# Define the pipe symbol so I can use it:
`%>%` <- magrittr::`%>%`


# 1 - Load INTEGRADIV occurrence data ================================


INTEGRADIV_occ_db <- read.csv(here::here("integradiv_db",
                                         "INTEGRADIV_occurrences_v2.csv"))

# Only keep trees data:
INTEGRADIV_trees_occ_df <- dplyr::filter(INTEGRADIV_occ_db,
                                         Taxon == "Trees")

# Number of species:
length(unique(INTEGRADIV_trees_occ_df$Species))


# 2 - Load and clean phylogenetic data ================================


# Load phylogeny data:
INTEGRADIV_phylogenies <- ape::read.tree(file = here::here("integradiv_db",
                                                           "INTEGRADIV_phylogenies_v2.tree"))

# Keep only the trees phylogeny:
trees_phylogeny <- INTEGRADIV_phylogenies[[1]]
plot(trees_phylogeny)

# Remove "_" between genus and species and add " " (same tr and occ data):
trees_phylogeny$tip.label <- gsub("_", " ", trees_phylogeny$tip.label)


# Check if all species in the occurrence df are in the phylogeny: YES :)
setdiff(unique(INTEGRADIV_trees_occ_df$Species),
        trees_phylogeny$tip.label)

# Check phylogeny only contains species in the occurrence df: NO :/
setdiff(trees_phylogeny$tip.label,
        unique(INTEGRADIV_trees_occ_df$Species))

# Remove the 2 species present in the phylogeny but not in our occ data:
trees_phylogeny_corrected <- ape::drop.tip(trees_phylogeny,
                                           c("Pyrus syriaca",
                                             "Tamarix passerinoides"),
                                           trim.internal = TRUE)
setdiff(trees_phylogeny_corrected$tip.label,
        unique(INTEGRADIV_trees_occ_df$Species))


# Save the trees phylogeny:
ape::write.tree(trees_phylogeny_corrected,
                file = here::here("transformed_data",
                                  "phylogeny_TREES.tree"))

