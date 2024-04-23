#### CODE FOR PREDICTING POLLINATION INTERACTIONS
#### Created by Emma-Liina Marjakangas, revised and cleaned 25th March 2024

#### PREPARATIONS ####

rm(list=ls());gc()

setwd("C:/Fricke_analyses")

library(tidyverse)
library(Rphylopars)
library(ape)
library(TreeTools)
library(phytools)

# devtools::install_github("jinyizju/U.PhyloMaker")
# library(U.PhyloMaker) # not available for this R version

# Read in the phylogeny

trial <- readRDS(file = "C:/Fricke_analyses/Pruned_Filled_plant_tree.RDS")
trial_phylo <- trial$phylo

# Read in the plant trait data

load("plant_traits_to_impute_231208.RData")
plant_df_new <- all_plant_traits_to_impute

# Make a different version of plant names to match with the phylotree object names

plant_df_new$Plant_species_2 <- gsub(" ", "_", plant_df_new$Plant_species, fixed = TRUE)

# Deal with the duplicated species that is due to the wrong genus name

plant_df_new <- plant_df_new[plant_df_new$Plant_genus != "Pilosocerus", ]
colnames(plant_df_new)[19] <- "species"

# Add new flower traits

load("flower_traits_for_imputation_231215.RData")
flower_traits <- flower_traits[, c(2,4,6)]
plant_df_new <- as.data.frame(merge(plant_df_new, flower_traits, by = "Plant_species"))

# Test trait imputation for the species that are included in the phylogeny
# Estimation of phylogenetic and phenotypic covariance parameters

# TODO Test the imputation performance by removing a couple of species with measured trait values and 
# check how well the trait imputation model manages to predict their values

plant_df_sub <- plant_df_new[, c(19,3,2,1,4)]
plant_df <- as.data.frame(plant_df_sub[, c(1,5)])
trial_phylo$sp.list1 <- plant_df

plant_df_sub <- plant_df_new[, c(19,3,2,1,6)]
plant_df <- as.data.frame(plant_df_sub[, c(1,5)])
trial_phylo$sp.list2 <- plant_df

plant_df_sub <- plant_df_new[, c(19,3,2,1,7)]
plant_df <- as.data.frame(plant_df_sub[, c(1,5)])
trial_phylo$sp.list3 <- plant_df

plant_df_sub <- plant_df_new[, c(19,3,2,1,9)]
plant_df <- as.data.frame(plant_df_sub[, c(1,5)])
trial_phylo$sp.list4 <- plant_df

plant_df_sub <- plant_df_new[, c(19,3,2,1,10)]
plant_df <- as.data.frame(plant_df_sub[, c(1,5)])
trial_phylo$sp.list5 <- plant_df

plant_df_sub <- plant_df_new[, c(19,3,2,1,11)]
plant_df <- as.data.frame(plant_df_sub[, c(1,5)])
trial_phylo$sp.list6 <- plant_df

plant_df_sub <- plant_df_new[, c(19,3,2,1,12)]
plant_df <- as.data.frame(plant_df_sub[, c(1,5)])
trial_phylo$sp.list7 <- plant_df

plant_df_sub <- plant_df_new[, c(19,3,2,1,20)]
plant_df <- as.data.frame(plant_df_sub[, c(1,5)])
trial_phylo$sp.list8 <- plant_df

plant_df_sub <- plant_df_new[, c(19,3,2,1,21)]
plant_df <- as.data.frame(plant_df_sub[, c(1,5)])
trial_phylo$sp.list9 <- plant_df

# Impute corolla length with 10 trees
# Save the values to be used in the subsequent analyses

PPE1 <- phylopars(trait_data = trial_phylo$sp.list1, tree = trial_phylo$run.1)
PPE2 <- phylopars(trait_data = trial_phylo$sp.list1, tree = trial_phylo$run.2)
PPE3 <- phylopars(trait_data = trial_phylo$sp.list1, tree = trial_phylo$run.3)
PPE4 <- phylopars(trait_data = trial_phylo$sp.list1, tree = trial_phylo$run.4)
PPE5 <- phylopars(trait_data = trial_phylo$sp.list1, tree = trial_phylo$run.5)
PPE6 <- phylopars(trait_data = trial_phylo$sp.list1, tree = trial_phylo$run.6)
PPE7 <- phylopars(trait_data = trial_phylo$sp.list1, tree = trial_phylo$run.7)
PPE8 <- phylopars(trait_data = trial_phylo$sp.list1, tree = trial_phylo$run.8)
PPE9 <- phylopars(trait_data = trial_phylo$sp.list1, tree = trial_phylo$run.9)
PPE10 <- phylopars(trait_data = trial_phylo$sp.list1, tree = trial_phylo$run.10)

run1_corolla <- as.data.frame(PPE1$anc_recon)
run2_corolla <- as.data.frame(PPE2$anc_recon)
run3_corolla <- as.data.frame(PPE3$anc_recon)
run4_corolla <- as.data.frame(PPE4$anc_recon)
run5_corolla <- as.data.frame(PPE5$anc_recon)
run6_corolla <- as.data.frame(PPE6$anc_recon)
run7_corolla <- as.data.frame(PPE7$anc_recon)
run8_corolla <- as.data.frame(PPE8$anc_recon)
run9_corolla <- as.data.frame(PPE9$anc_recon)
run10_corolla <- as.data.frame(PPE10$anc_recon)

run1_corolla$species <- rownames(run1_corolla)
run2_corolla$species <- rownames(run2_corolla)
run3_corolla$species <- rownames(run3_corolla)
run4_corolla$species <- rownames(run4_corolla)
run5_corolla$species <- rownames(run5_corolla)
run6_corolla$species <- rownames(run6_corolla)
run7_corolla$species <- rownames(run7_corolla)
run8_corolla$species <- rownames(run8_corolla)
run9_corolla$species <- rownames(run9_corolla)
run10_corolla$species <- rownames(run10_corolla)

# Impute max height 10 trees
# Save the values to be used in the subsequent analyses

PPE1 <- phylopars(trait_data = trial_phylo$sp.list2, tree = trial_phylo$run.1)
PPE2 <- phylopars(trait_data = trial_phylo$sp.list2, tree = trial_phylo$run.2)
PPE3 <- phylopars(trait_data = trial_phylo$sp.list2, tree = trial_phylo$run.3)
PPE4 <- phylopars(trait_data = trial_phylo$sp.list2, tree = trial_phylo$run.4)
PPE5 <- phylopars(trait_data = trial_phylo$sp.list2, tree = trial_phylo$run.5)
PPE6 <- phylopars(trait_data = trial_phylo$sp.list2, tree = trial_phylo$run.6)
PPE7 <- phylopars(trait_data = trial_phylo$sp.list2, tree = trial_phylo$run.7)
PPE8 <- phylopars(trait_data = trial_phylo$sp.list2, tree = trial_phylo$run.8)
PPE9 <- phylopars(trait_data = trial_phylo$sp.list2, tree = trial_phylo$run.9)
PPE10 <- phylopars(trait_data = trial_phylo$sp.list2, tree = trial_phylo$run.10)

run1_height_max <- as.data.frame(PPE1$anc_recon)
run2_height_max <- as.data.frame(PPE2$anc_recon)
run3_height_max <- as.data.frame(PPE3$anc_recon)
run4_height_max <- as.data.frame(PPE4$anc_recon)
run5_height_max <- as.data.frame(PPE5$anc_recon)
run6_height_max <- as.data.frame(PPE6$anc_recon)
run7_height_max <- as.data.frame(PPE7$anc_recon)
run8_height_max <- as.data.frame(PPE8$anc_recon)
run9_height_max <- as.data.frame(PPE9$anc_recon)
run10_height_max <- as.data.frame(PPE10$anc_recon)

run1_height_max$species <- rownames(run1_height_max)
run2_height_max$species <- rownames(run2_height_max)
run3_height_max$species <- rownames(run3_height_max)
run4_height_max$species <- rownames(run4_height_max)
run5_height_max$species <- rownames(run5_height_max)
run6_height_max$species <- rownames(run6_height_max)
run7_height_max$species <- rownames(run7_height_max)
run8_height_max$species <- rownames(run8_height_max)
run9_height_max$species <- rownames(run9_height_max)
run10_height_max$species <- rownames(run10_height_max)

# Impute mean height 10 trees
# Save the values to be used in the subsequent analyses

PPE1 <- phylopars(trait_data = trial_phylo$sp.list3, tree = trial_phylo$run.1)
PPE2 <- phylopars(trait_data = trial_phylo$sp.list3, tree = trial_phylo$run.2)
PPE3 <- phylopars(trait_data = trial_phylo$sp.list3, tree = trial_phylo$run.3)
PPE4 <- phylopars(trait_data = trial_phylo$sp.list3, tree = trial_phylo$run.4)
PPE5 <- phylopars(trait_data = trial_phylo$sp.list3, tree = trial_phylo$run.5)
PPE6 <- phylopars(trait_data = trial_phylo$sp.list3, tree = trial_phylo$run.6)
PPE7 <- phylopars(trait_data = trial_phylo$sp.list3, tree = trial_phylo$run.7)
PPE8 <- phylopars(trait_data = trial_phylo$sp.list3, tree = trial_phylo$run.8)
PPE9 <- phylopars(trait_data = trial_phylo$sp.list3, tree = trial_phylo$run.9)
PPE10 <- phylopars(trait_data = trial_phylo$sp.list3, tree = trial_phylo$run.10)

run1_height_mean <- as.data.frame(PPE1$anc_recon)
run2_height_mean <- as.data.frame(PPE2$anc_recon)
run3_height_mean <- as.data.frame(PPE3$anc_recon)
run4_height_mean <- as.data.frame(PPE4$anc_recon)
run5_height_mean <- as.data.frame(PPE5$anc_recon)
run6_height_mean <- as.data.frame(PPE6$anc_recon)
run7_height_mean <- as.data.frame(PPE7$anc_recon)
run8_height_mean <- as.data.frame(PPE8$anc_recon)
run9_height_mean <- as.data.frame(PPE9$anc_recon)
run10_height_mean <- as.data.frame(PPE10$anc_recon)

run1_height_mean$species <- rownames(run1_height_mean)
run2_height_mean$species <- rownames(run2_height_mean)
run3_height_mean$species <- rownames(run3_height_mean)
run4_height_mean$species <- rownames(run4_height_mean)
run5_height_mean$species <- rownames(run5_height_mean)
run6_height_mean$species <- rownames(run6_height_mean)
run7_height_mean$species <- rownames(run7_height_mean)
run8_height_mean$species <- rownames(run8_height_mean)
run9_height_mean$species <- rownames(run9_height_mean)
run10_height_mean$species <- rownames(run10_height_mean)

# Impute max seed length 10 trees
# Save the values to be used in the subsequent analyses

PPE1 <- phylopars(trait_data = trial_phylo$sp.list4, tree = trial_phylo$run.1)
PPE2 <- phylopars(trait_data = trial_phylo$sp.list4, tree = trial_phylo$run.2)
PPE3 <- phylopars(trait_data = trial_phylo$sp.list4, tree = trial_phylo$run.3)
PPE4 <- phylopars(trait_data = trial_phylo$sp.list4, tree = trial_phylo$run.4)
PPE5 <- phylopars(trait_data = trial_phylo$sp.list4, tree = trial_phylo$run.5)
PPE6 <- phylopars(trait_data = trial_phylo$sp.list4, tree = trial_phylo$run.6)
PPE7 <- phylopars(trait_data = trial_phylo$sp.list4, tree = trial_phylo$run.7)
PPE8 <- phylopars(trait_data = trial_phylo$sp.list4, tree = trial_phylo$run.8)
PPE9 <- phylopars(trait_data = trial_phylo$sp.list4, tree = trial_phylo$run.9)
PPE10 <- phylopars(trait_data = trial_phylo$sp.list4, tree = trial_phylo$run.10)

run1_length_max <- as.data.frame(PPE1$anc_recon)
run2_length_max <- as.data.frame(PPE2$anc_recon)
run3_length_max <- as.data.frame(PPE3$anc_recon)
run4_length_max <- as.data.frame(PPE4$anc_recon)
run5_length_max <- as.data.frame(PPE5$anc_recon)
run6_length_max <- as.data.frame(PPE6$anc_recon)
run7_length_max <- as.data.frame(PPE7$anc_recon)
run8_length_max <- as.data.frame(PPE8$anc_recon)
run9_length_max <- as.data.frame(PPE9$anc_recon)
run10_length_max <- as.data.frame(PPE10$anc_recon)

run1_length_max$species <- rownames(run1_length_max)
run2_length_max$species <- rownames(run2_length_max)
run3_length_max$species <- rownames(run3_length_max)
run4_length_max$species <- rownames(run4_length_max)
run5_length_max$species <- rownames(run5_length_max)
run6_length_max$species <- rownames(run6_length_max)
run7_length_max$species <- rownames(run7_length_max)
run8_length_max$species <- rownames(run8_length_max)
run9_length_max$species <- rownames(run9_length_max)
run10_length_max$species <- rownames(run10_length_max)

# Impute mean seed length 10 trees
# Save the values to be used in the subsequent analyses

PPE1 <- phylopars(trait_data = trial_phylo$sp.list5, tree = trial_phylo$run.1)
PPE2 <- phylopars(trait_data = trial_phylo$sp.list5, tree = trial_phylo$run.2)
PPE3 <- phylopars(trait_data = trial_phylo$sp.list5, tree = trial_phylo$run.3)
PPE4 <- phylopars(trait_data = trial_phylo$sp.list5, tree = trial_phylo$run.4)
PPE5 <- phylopars(trait_data = trial_phylo$sp.list5, tree = trial_phylo$run.5)
PPE6 <- phylopars(trait_data = trial_phylo$sp.list5, tree = trial_phylo$run.6)
PPE7 <- phylopars(trait_data = trial_phylo$sp.list5, tree = trial_phylo$run.7)
PPE8 <- phylopars(trait_data = trial_phylo$sp.list5, tree = trial_phylo$run.8)
PPE9 <- phylopars(trait_data = trial_phylo$sp.list5, tree = trial_phylo$run.9)
PPE10 <- phylopars(trait_data = trial_phylo$sp.list5, tree = trial_phylo$run.10)

run1_length_mean <- as.data.frame(PPE1$anc_recon)
run2_length_mean <- as.data.frame(PPE2$anc_recon)
run3_length_mean <- as.data.frame(PPE3$anc_recon)
run4_length_mean <- as.data.frame(PPE4$anc_recon)
run5_length_mean <- as.data.frame(PPE5$anc_recon)
run6_length_mean <- as.data.frame(PPE6$anc_recon)
run7_length_mean <- as.data.frame(PPE7$anc_recon)
run8_length_mean <- as.data.frame(PPE8$anc_recon)
run9_length_mean <- as.data.frame(PPE9$anc_recon)
run10_length_mean <- as.data.frame(PPE10$anc_recon)

run1_length_mean$species <- rownames(run1_length_mean)
run2_length_mean$species <- rownames(run2_length_mean)
run3_length_mean$species <- rownames(run3_length_mean)
run4_length_mean$species <- rownames(run4_length_mean)
run5_length_mean$species <- rownames(run5_length_mean)
run6_length_mean$species <- rownames(run6_length_mean)
run7_length_mean$species <- rownames(run7_length_mean)
run8_length_mean$species <- rownames(run8_length_mean)
run9_length_mean$species <- rownames(run9_length_mean)
run10_length_mean$species <- rownames(run10_length_mean)

# Impute max seed mass 10 trees
# Save the values to be used in the subsequent analyses

PPE1 <- phylopars(trait_data = trial_phylo$sp.list6, tree = trial_phylo$run.1)
PPE2 <- phylopars(trait_data = trial_phylo$sp.list6, tree = trial_phylo$run.2)
PPE3 <- phylopars(trait_data = trial_phylo$sp.list6, tree = trial_phylo$run.3)
PPE4 <- phylopars(trait_data = trial_phylo$sp.list6, tree = trial_phylo$run.4)
PPE5 <- phylopars(trait_data = trial_phylo$sp.list6, tree = trial_phylo$run.5)
PPE6 <- phylopars(trait_data = trial_phylo$sp.list6, tree = trial_phylo$run.6)
PPE7 <- phylopars(trait_data = trial_phylo$sp.list6, tree = trial_phylo$run.7)
PPE8 <- phylopars(trait_data = trial_phylo$sp.list6, tree = trial_phylo$run.8)
PPE9 <- phylopars(trait_data = trial_phylo$sp.list6, tree = trial_phylo$run.9)
PPE10 <- phylopars(trait_data = trial_phylo$sp.list6, tree = trial_phylo$run.10)

run1_mass_max <- as.data.frame(PPE1$anc_recon)
run2_mass_max <- as.data.frame(PPE2$anc_recon)
run3_mass_max <- as.data.frame(PPE3$anc_recon)
run4_mass_max <- as.data.frame(PPE4$anc_recon)
run5_mass_max <- as.data.frame(PPE5$anc_recon)
run6_mass_max <- as.data.frame(PPE6$anc_recon)
run7_mass_max <- as.data.frame(PPE7$anc_recon)
run8_mass_max <- as.data.frame(PPE8$anc_recon)
run9_mass_max <- as.data.frame(PPE9$anc_recon)
run10_mass_max <- as.data.frame(PPE10$anc_recon)

run1_mass_max$species <- rownames(run1_mass_max)
run2_mass_max$species <- rownames(run2_mass_max)
run3_mass_max$species <- rownames(run3_mass_max)
run4_mass_max$species <- rownames(run4_mass_max)
run5_mass_max$species <- rownames(run5_mass_max)
run6_mass_max$species <- rownames(run6_mass_max)
run7_mass_max$species <- rownames(run7_mass_max)
run8_mass_max$species <- rownames(run8_mass_max)
run9_mass_max$species <- rownames(run9_mass_max)
run10_mass_max$species <- rownames(run10_mass_max)

# Impute max seed mass 10 trees
# Save the values to be used in the subsequent analyses

PPE1 <- phylopars(trait_data = trial_phylo$sp.list7, tree = trial_phylo$run.1)
PPE2 <- phylopars(trait_data = trial_phylo$sp.list7, tree = trial_phylo$run.2)
PPE3 <- phylopars(trait_data = trial_phylo$sp.list7, tree = trial_phylo$run.3)
PPE4 <- phylopars(trait_data = trial_phylo$sp.list7, tree = trial_phylo$run.4)
PPE5 <- phylopars(trait_data = trial_phylo$sp.list7, tree = trial_phylo$run.5)
PPE6 <- phylopars(trait_data = trial_phylo$sp.list7, tree = trial_phylo$run.6)
PPE7 <- phylopars(trait_data = trial_phylo$sp.list7, tree = trial_phylo$run.7)
PPE8 <- phylopars(trait_data = trial_phylo$sp.list7, tree = trial_phylo$run.8)
PPE9 <- phylopars(trait_data = trial_phylo$sp.list7, tree = trial_phylo$run.9)
PPE10 <- phylopars(trait_data = trial_phylo$sp.list7, tree = trial_phylo$run.10)

run1_mass_mean <- as.data.frame(PPE1$anc_recon)
run2_mass_mean <- as.data.frame(PPE2$anc_recon)
run3_mass_mean <- as.data.frame(PPE3$anc_recon)
run4_mass_mean <- as.data.frame(PPE4$anc_recon)
run5_mass_mean <- as.data.frame(PPE5$anc_recon)
run6_mass_mean <- as.data.frame(PPE6$anc_recon)
run7_mass_mean <- as.data.frame(PPE7$anc_recon)
run8_mass_mean <- as.data.frame(PPE8$anc_recon)
run9_mass_mean <- as.data.frame(PPE9$anc_recon)
run10_mass_mean <- as.data.frame(PPE10$anc_recon)

run1_mass_mean$species <- rownames(run1_mass_mean)
run2_mass_mean$species <- rownames(run2_mass_mean)
run3_mass_mean$species <- rownames(run3_mass_mean)
run4_mass_mean$species <- rownames(run4_mass_mean)
run5_mass_mean$species <- rownames(run5_mass_mean)
run6_mass_mean$species <- rownames(run6_mass_mean)
run7_mass_mean$species <- rownames(run7_mass_mean)
run8_mass_mean$species <- rownames(run8_mass_mean)
run9_mass_mean$species <- rownames(run9_mass_mean)
run10_mass_mean$species <- rownames(run10_mass_mean)

# Impute nectar concentration 10 trees
# Save the values to be used in the subsequent analyses

PPE1 <- phylopars(trait_data = trial_phylo$sp.list8, tree = trial_phylo$run.1)
PPE2 <- phylopars(trait_data = trial_phylo$sp.list8, tree = trial_phylo$run.2)
PPE3 <- phylopars(trait_data = trial_phylo$sp.list8, tree = trial_phylo$run.3)
PPE4 <- phylopars(trait_data = trial_phylo$sp.list8, tree = trial_phylo$run.4)
PPE5 <- phylopars(trait_data = trial_phylo$sp.list8, tree = trial_phylo$run.5)
PPE6 <- phylopars(trait_data = trial_phylo$sp.list8, tree = trial_phylo$run.6)
PPE7 <- phylopars(trait_data = trial_phylo$sp.list8, tree = trial_phylo$run.7)
PPE8 <- phylopars(trait_data = trial_phylo$sp.list8, tree = trial_phylo$run.8)
PPE9 <- phylopars(trait_data = trial_phylo$sp.list8, tree = trial_phylo$run.9)
PPE10 <- phylopars(trait_data = trial_phylo$sp.list8, tree = trial_phylo$run.10)

run1_nectar_mean <- as.data.frame(PPE1$anc_recon)
run2_nectar_mean <- as.data.frame(PPE2$anc_recon)
run3_nectar_mean <- as.data.frame(PPE3$anc_recon)
run4_nectar_mean <- as.data.frame(PPE4$anc_recon)
run5_nectar_mean <- as.data.frame(PPE5$anc_recon)
run6_nectar_mean <- as.data.frame(PPE6$anc_recon)
run7_nectar_mean <- as.data.frame(PPE7$anc_recon)
run8_nectar_mean <- as.data.frame(PPE8$anc_recon)
run9_nectar_mean <- as.data.frame(PPE9$anc_recon)
run10_nectar_mean <- as.data.frame(PPE10$anc_recon)

run1_nectar_mean$species <- rownames(run1_nectar_mean)
run2_nectar_mean$species <- rownames(run2_nectar_mean)
run3_nectar_mean$species <- rownames(run3_nectar_mean)
run4_nectar_mean$species <- rownames(run4_nectar_mean)
run5_nectar_mean$species <- rownames(run5_nectar_mean)
run6_nectar_mean$species <- rownames(run6_nectar_mean)
run7_nectar_mean$species <- rownames(run7_nectar_mean)
run8_nectar_mean$species <- rownames(run8_nectar_mean)
run9_nectar_mean$species <- rownames(run9_nectar_mean)
run10_nectar_mean$species <- rownames(run10_nectar_mean)

# Impute pollination syndrome 10 trees
# Save the values to be used in the subsequent analyses

PPE1 <- phylopars(trait_data = trial_phylo$sp.list9, tree = trial_phylo$run.1)
PPE2 <- phylopars(trait_data = trial_phylo$sp.list9, tree = trial_phylo$run.2)
PPE3 <- phylopars(trait_data = trial_phylo$sp.list9, tree = trial_phylo$run.3)
PPE4 <- phylopars(trait_data = trial_phylo$sp.list9, tree = trial_phylo$run.4)
PPE5 <- phylopars(trait_data = trial_phylo$sp.list9, tree = trial_phylo$run.5)
PPE6 <- phylopars(trait_data = trial_phylo$sp.list9, tree = trial_phylo$run.6)
PPE7 <- phylopars(trait_data = trial_phylo$sp.list9, tree = trial_phylo$run.7)
PPE8 <- phylopars(trait_data = trial_phylo$sp.list9, tree = trial_phylo$run.8)
PPE9 <- phylopars(trait_data = trial_phylo$sp.list9, tree = trial_phylo$run.9)
PPE10 <- phylopars(trait_data = trial_phylo$sp.list9, tree = trial_phylo$run.10)

run1_syndrome_mean <- as.data.frame(PPE1$anc_recon)
run2_syndrome_mean <- as.data.frame(PPE2$anc_recon)
run3_syndrome_mean <- as.data.frame(PPE3$anc_recon)
run4_syndrome_mean <- as.data.frame(PPE4$anc_recon)
run5_syndrome_mean <- as.data.frame(PPE5$anc_recon)
run6_syndrome_mean <- as.data.frame(PPE6$anc_recon)
run7_syndrome_mean <- as.data.frame(PPE7$anc_recon)
run8_syndrome_mean <- as.data.frame(PPE8$anc_recon)
run9_syndrome_mean <- as.data.frame(PPE9$anc_recon)
run10_syndrome_mean <- as.data.frame(PPE10$anc_recon)

run1_syndrome_mean$species <- rownames(run1_syndrome_mean)
run2_syndrome_mean$species <- rownames(run2_syndrome_mean)
run3_syndrome_mean$species <- rownames(run3_syndrome_mean)
run4_syndrome_mean$species <- rownames(run4_syndrome_mean)
run5_syndrome_mean$species <- rownames(run5_syndrome_mean)
run6_syndrome_mean$species <- rownames(run6_syndrome_mean)
run7_syndrome_mean$species <- rownames(run7_syndrome_mean)
run8_syndrome_mean$species <- rownames(run8_syndrome_mean)
run9_syndrome_mean$species <- rownames(run9_syndrome_mean)
run10_syndrome_mean$species <- rownames(run10_syndrome_mean)

# Explore imputed traits
# Compare distributions of imputed and original trait values

BIC(PPE1)
print.phylopars(PPE1)

hist(PPE1$anc_recon)
# hist(plant_df$Corolla_length) 

#### ADD IMPUTED TRAIT VALUES BACK TO TRAIT MATRIX ####

df_list <- list(plant_df_new, run1_corolla, run2_corolla, run3_corolla, run4_corolla, run5_corolla, 
                run6_corolla, run7_corolla, run8_corolla, run9_corolla, run10_corolla,
                run1_height_max, run2_height_max, run3_height_max, run4_height_max, run5_height_max, 
                run6_height_max, run7_height_max, run8_height_max, run9_height_max, run10_height_max,
                run1_height_mean, run2_height_mean, run3_height_mean, run4_height_mean, run5_height_mean, 
                run6_height_mean, run7_height_mean, run8_height_mean, run9_height_mean, run10_height_mean,
                run1_length_max, run2_length_max, run3_length_max, run4_length_max, run5_length_max, 
                run6_length_max, run7_length_max, run8_length_max, run9_length_max, run10_length_max,
                run1_length_mean, run2_length_mean, run3_length_mean, run4_length_mean, run5_length_mean, 
                run6_length_mean, run7_length_mean, run8_length_mean, run9_length_mean, run10_length_mean,
                run1_mass_max, run2_mass_max, run3_mass_max, run4_mass_max, run5_mass_max, 
                run6_mass_max, run7_mass_max, run8_mass_max, run9_mass_max, run10_mass_max,
                run1_mass_mean, run2_mass_mean, run3_mass_mean, run4_mass_mean, run5_mass_mean, 
                run6_mass_mean, run7_mass_mean, run8_mass_mean, run9_mass_mean, run10_mass_mean,
                run1_nectar_mean, run2_nectar_mean, run3_nectar_mean, run4_nectar_mean, run5_nectar_mean, 
                run6_nectar_mean, run7_nectar_mean, run8_nectar_mean, run9_nectar_mean, run10_nectar_mean,
                run1_syndrome_mean, run2_syndrome_mean, run3_syndrome_mean, run4_syndrome_mean, run5_syndrome_mean, 
                run6_syndrome_mean, run7_syndrome_mean, run8_syndrome_mean, run9_syndrome_mean, run10_syndrome_mean)

full_traits <- df_list %>% reduce(full_join, by = 'species')
full_traits <- full_traits[full_traits$species %in% plant_df_new$species, ]

colnames(full_traits) <- c("Plant_species", "Plant_family", "Plant_genus", "Corolla_length_original", "Woodiness_original", 
                           "Plant_height_max_original", "Plant_height_mean_original", "Self_fertilization_original", 
                           "Seed_length_max_original", "Seed_length_mean_original", "Seed_mass_max_original", 
                           "Seed_mass_mean_original", "Woodiness_final", "Self_fertilization_final", "Seed_length_max_final", 
                           "Seed_length_mean_final", "Seed_mass_max_final", "Seed_mass_mean_final", "species", "Nectar_concentration_original",
                           "Pollination_syndrome_original","corolla_length_tree1", "corolla_length_tree2", "corolla_length_tree3", "corolla_length_tree4", 
                           "corolla_length_tree5", "corolla_length_tree6", "corolla_length_tree7", "corolla_length_tree8", 
                           "corolla_length_tree9", "corolla_length_tree10", "plant_height_max_tree1", "plant_height_max_tree2", 
                           "plant_height_max_tree3", "plant_height_max_tree4", "plant_height_max_tree5", "plant_height_max_tree6", 
                           "plant_height_max_tree7", "plant_height_max_tree8", "plant_height_max_tree9", "plant_height_max_tree10",
                           "plant_height_mean_tree1", "plant_height_mean_tree2", "plant_height_mean_tree3", "plant_height_mean_tree4", "plant_height_mean_tree5", 
                           "plant_height_mean_tree6", "plant_height_mean_tree7", "plant_height_mean_tree8", "plant_height_mean_tree9", "plant_height_mean_tree10",
                           "seed_length_max_tree1", "seed_length_max_tree2", "seed_length_max_tree3", "seed_length_max_tree4", "seed_length_max_tree5", 
                           "seed_length_max_tree6", "seed_length_max_tree7", "seed_length_max_tree8", "seed_length_max_tree9", "seed_length_max_tree10", 
                           "seed_length_mean_tree1", "seed_length_mean_tree2", "seed_length_mean_tree3", "seed_length_mean_tree4", "seed_length_mean_tree5", 
                           "seed_length_mean_tree6", "seed_length_mean_tree7", "seed_length_mean_tree8", "seed_length_mean_tree9", "seed_length_mean_tree10", 
                           "seed_mass_max_tree1", "seed_mass_max_tree2", "seed_mass_max_tree3", "seed_mass_max_tree4", "seed_mass_max_tree5", 
                           "seed_mass_max_tree6", "seed_mass_max_tree7", "seed_mass_max_tree8", "seed_mass_max_tree9", "seed_mass_max_tree10", 
                           "seed_mass_mean_tree1", "seed_mass_mean_tree2", "seed_mass_mean_tree3", "seed_mass_mean_tree4", "seed_mass_mean_tree5", 
                           "seed_mass_mean_tree6", "seed_mass_mean_tree7", "seed_mass_mean_tree8", "seed_mass_mean_tree9", "seed_mass_mean_tree10",
                           "seed_nectar_mean_tree1", "seed_nectar_mean_tree2", "seed_nectar_mean_tree3", "seed_nectar_mean_tree4", "seed_nectar_mean_tree5", 
                           "seed_nectar_mean_tree6", "seed_nectar_mean_tree7", "seed_nectar_mean_tree8", "seed_nectar_mean_tree9", "seed_nectar_mean_tree10",
                           "pol_syndrome_mean_tree1", "pol_syndrome_mean_tree2", "pol_syndrome_mean_tree3", "pol_syndrome_mean_tree4", "pol_syndrome_mean_tree5", 
                           "pol_syndrome_mean_tree6", "pol_syndrome_mean_tree7", "pol_syndrome_mean_tree8", "pol_syndrome_mean_tree9", "pol_syndrome_mean_tree10")

# Take the mean of the imputed trait values to be used in the analyses

full_traits$corolla_length_imputed_mean <- rowMeans(full_traits[, 22:31])
full_traits$plant_height_max_imputed_mean <- rowMeans(full_traits[, 32:41])
full_traits$plant_height_mean_imputed_mean <- rowMeans(full_traits[, 42:51])
full_traits$seed_length_max_imputed_mean <- rowMeans(full_traits[, 52:61])
full_traits$seed_length_mean_imputed_mean <- rowMeans(full_traits[, 62:71])
full_traits$seed_mass_max_imputed_mean <- rowMeans(full_traits[, 72:81])
full_traits$seed_mass_mean_imputed_mean <- rowMeans(full_traits[, 82:91])
full_traits$nectar_concentration_imputed_mean <- rowMeans(full_traits[, 92:101])
full_traits$pollination_syndrome_imputed_mean <- rowMeans(full_traits[, 102:111])

# Check for correlations among imputed traits from different phylogenetic trees
# very strong correlation >0.96 always, > 0.99 mostly

cor(full_traits[, 22:31]) 
cor(full_traits[, 32:41])
cor(full_traits[, 42:51])
cor(full_traits[, 52:61])
cor(full_traits[, 62:71])
cor(full_traits[, 72:81])
cor(full_traits[, 82:91])
cor(full_traits[, 92:101])
cor(full_traits[, 102:111])

# Check for correlations among averaged traits
# Seed mass traits correlate very strongly with each other and were singular -> remove
# Plant height traits correlate quite strongly with each other -> remove the one with more NAs in original dataset (mean)

cor(full_traits[, 112:120])
sum(is.na(full_traits$Plant_height_max_original)) # 779
sum(is.na(full_traits$Plant_height_mean_original)) # 856

# Check correlations with imputed values based on taxonomy
# The correlation is poor. Potential reasons: 
# the final values come partly from GIFT that has genus- and even family-level averages available. Those values could include
# large average values that come from branches outside this dataset or geographic area. So it's better to use 
# phylogenetically imputed rather than family-level mean traits.

cor(full_traits$seed_length_max_imputed_mean, full_traits$Seed_length_max_final, use = "complete.obs")
plot(full_traits$seed_length_max_imputed_mean, full_traits$Seed_length_max_final)

head(full_traits$seed_length_max_imputed_mean)
head(full_traits$Seed_length_max_final)
hist(full_traits$seed_length_max_imputed_mean)
hist(full_traits$Seed_length_max_final)
range(full_traits$Seed_length_max_original, na.rm = TRUE)

cor(full_traits$seed_mass_max_imputed_mean, full_traits$Seed_mass_max_final, use = "complete.obs")
plot(full_traits$seed_mass_max_imputed_mean, full_traits$Seed_mass_max_final)

# Save the dataset for future use

save(full_traits, file = "imputed_plant_traits_231222.RData")

#### ADDING MISSING BRANCHES TO PHYLOGENY ####

# Code from Alejo

# Links to useful resources
# https://www.sciencedirect.com/science/article/pii/S2468265922001329 --> paper U.PhyloMaker
# https://github.com/jinyizju/U.PhyloMaker or https://github.com/ISMRCC/U.PhyloMaker

# Plant species list with taxonomic levels

sp_Emma <- read.csv("~/Library/CloudStorage/Dropbox/Aarhus Assistant Professor/Students/AU/PosDocs/Emma-Liina Marjakangas/Data/plant_species_table_231106.csv", sep=";")

# Download the megatree of trees from the online database
# Download also the genus list

megatree <- read.tree("~/Library/CloudStorage/Dropbox/Aarhus Assistant Professor/Students/AU/PosDocs/Emma-Liina Marjakangas/Data/plant_megatree.tre")
gen.list <- read.csv('~/Library/CloudStorage/Dropbox/Aarhus Assistant Professor/Students/AU/PosDocs/Emma-Liina Marjakangas/Data/plant_genus_list.csv', sep = ",")
dim(gen.list)

# Run the phylo.maker function with the three files and output the results

a <- Sys.time()

result <- phylo.maker(sp_Emma$Plant_species_2,
                      megatree, gen.list,
                      scenario = 2,
                      r = 10)

Sys.time() - a 

saveRDS(result, "~/Library/CloudStorage/Dropbox/Aarhus Assistant Professor/Students/AU/PosDocs/Emma-Liina Marjakangas/Data/Pruned_Filled_plant_tree.RDS")



