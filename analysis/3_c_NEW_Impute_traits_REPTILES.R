################################################################################
##
## Script to impute traits based on taxonomic data - for REPTILES
##
## Camille Magneville
##
## 09/2024
##
## 4_a_NEW_Impute_traits_REPTILES.R
##
################################################################################

`%>%` <- magrittr::`%>%`

# 1 - Load data ========================================================

sp_tr_REPTILES <- readRDS(here::here("transformed_data",
                                  "raw_traits_REPTILES.rds"))

# See missing values:
# Change first column name for funbiogeo use:
funbiogeo_df <- dplyr::rename(sp_tr_REPTILES, species = Species)

# Get the percentage of completedness per traits:
funbiogeo::fb_plot_number_species_by_trait(funbiogeo_df)

# Get the percentage of species with for than 1, 2, 3 etc traits:
funbiogeo::fb_plot_number_traits_by_species(funbiogeo_df)

# Check traits in the right format:
str(sp_tr_REPTILES)


# 2 - Retrieve species Genus/Family information =================================


# New format:
sp_tr_REPTILES_df <- sp_tr_REPTILES %>%
  dplyr::mutate(dplyr::across("Species", stringr::str_replace, ' ', '_')) %>%
  tibble::column_to_rownames("Species")

# Retrieve Family and Order levels:

# Get species names without "_":
species_nm <- rownames(sp_tr_REPTILES_df)
species_nm_corrected <- stringr::str_replace(species_nm, '_', ' ')

# Retrieve Genus, Family, Order:
family_order_nms <- taxize::tax_name(species_nm, get = c("genus", "family", "order"),
                                     db = 'itis')

# Which species where not found?
missing_sp <- family_order_nms[which(is.na(family_order_nms$genus)), "query"]

# If a species from the same Genus has been found - complete:
family_order_completed_nms <- check.genus(taxo_df = family_order_nms,
                                          missing_sp_vect = missing_sp)

# Few species with missing information - complete by hand:
missing_sp <- family_order_completed_nms[which(is.na(family_order_completed_nms$genus)), "query"]
family_order_completed_nms[which(family_order_completed_nms$query == "Laudakia_stellio"), c("genus", "family", "order")] <- c("Stellagma",
                                                                                                                            "Agamidae",
                                                                                                                            "Squamata")
family_order_completed_nms[which(family_order_completed_nms$query == "Rhinechis_scalaris"), c("genus", "family", "order")] <- c("Zamenis",
                                                                                                                             "Colubridae",
                                                                                                                             "Squamata")
family_order_completed_nms[which(family_order_completed_nms$query == "Typhlops_vermicularis"), c("genus", "family", "order")] <- c("Xerotyphlops",
                                                                                                                              "Typhlopidae",
                                                                                                                              "Squamata")
# Save it:
saveRDS(family_order_completed_nms, file = here::here("transformed_data",
                                                      "taxo_info_REPTILES.rds"))


# No missing information now:
missing_sp <- family_order_completed_nms[which(is.na(family_order_completed_nms$genus)), "query"]


# 3 - Complete based on the closest taxonomic level which has information ======

family_order_completed_nms <- readRDS(file = here::here("transformed_data",
                                                      "taxo_info_REPTILES.rds"))

# Note: Do the mean for quantitative traits and the mode for qualitative traits

imputed_sp_tr_REPTILES_df <- impute.missing.traits(sp_tr_NA_df = sp_tr_REPTILES_df,
                                                taxo_df = family_order_completed_nms,
                                                traits_NA_nms = c("OffspringPerYear",
                                                                  "LongevityMax",
                                                                  "SVLMax"))
# No value missing at all :)

# Save this final version of traits:
saveRDS(imputed_sp_tr_REPTILES_df, file = here::here("transformed_data",
                                                  "final_traits_REPTILES.rds"))
