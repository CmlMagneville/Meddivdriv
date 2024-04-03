################################################################################
##
## Script to compute PD null models for Faith's PD, MPD, MNPD - for TREES
##
## Camille Magneville
##
## 03/04/2024
##
## 2_a_PD_Null_models_TREES.R
##
################################################################################


# !!! NOTE: DONE AT 50*50km SCALE !!!


# Define the pipe symbol so I can use it:
`%>%` <- magrittr::`%>%`


# 1 - Load data ========================================================


# Load occ data:
trees_occ_df <- readRDS(here::here("transformed_data",
                                   "sp_asb_50km_TREES.rds"))

# Load trees phylogeny:
trees_phylogeny <- ape::read.tree(file = here::here("transformed_data",
                                                    "phylogeny_TREES.tree"))



# 2 - Compute PD metrics for each grid cell - observed data =============


# Compute Faith's PD for 50km cells:
trees_PD_Faith_50km <- compute.PD.per.cell(sp_asb_df = trees_occ_df,
                                           phylo = trees_phylogeny,
                                           metric_nm = "Faith_index",
                                           grid = "50x50",
                                           taxon = "Trees")

# Compute Mean Pairwise Distance (MPD) for 50km cells:
trees_PD_MPD_50km <- compute.PD.per.cell(sp_asb_df = trees_occ_df,
                                         phylo = trees_phylogeny,
                                         metric_nm = "MPD",
                                         grid = "50x50",
                                         taxon = "Trees")

# Compute MNTD for 50km cells:
trees_PD_MNTD_50km <- compute.PD.per.cell(sp_asb_df = trees_occ_df,
                                         phylo = trees_phylogeny,
                                         metric_nm = "MNTD",
                                         grid = "50x50",
                                         taxon = "Trees")


# 3 - Compute PD null models ============================================


# Note: For each grid cell, same species richness but different species ...
# ... compositions: as many null asb as wanted through the `nb_asb_rep` input:


PD_null_asb_list <- compute.null.model.PD(phylo_tree = trees_phylogeny,
                                          sp_asb_df = trees_occ_df,
                                          nb_asb_rep = 1000)

faith_null_models <- PD_null_asb_list$faith
mpd_null_models <- PD_null_asb_list$mpd
mntd_null_models <- PD_null_asb_list$mntd

saveRDS(faith_null_models, here::here("transformed_data",
                                      "PD_Faith_null_models_50km_TREES.rds"))
saveRDS(mpd_null_models, here::here("transformed_data",
                                    "PD_MPD_null_models_50km_TREES.rds"))
saveRDS(mntd_null_models, here::here("transformed_data",
                                    "PD_MNTD_null_models_50km_TREES.rds"))



