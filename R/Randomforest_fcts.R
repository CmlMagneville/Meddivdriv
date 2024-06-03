################################################################################
##
## Function(s) to do the random forest analyses - n iteration of the rf model to
## ... test its performance
##
## Adaptated from https://github.com/LoiseauN/Predict_productivity/blob/main/R/test_model.R
##
## 23/05/2024
##
## Randomforest_fcts.R
##
################################################################################



#' Compute n iteration of the random forest model to test its accuracy and
#' variable importance
#'
#' @param rf_data a data frame containing data to run the random forest - ie
#' predictor variables in columns with the last column being the ses of a given
#' diversity metric. This last column must be named \code{ses}.
#'
#' @param iteration_nb the number of time the random forest should be repeated.
#'
#' @return a data frame with variable importance and model performance
#'
#' @export
#'

test.rf.model <- function(rf_data,
                       iteration_nb) {


  # Create one final db that will be returned:
  var_imp_final_df <- as.data.frame(matrix(ncol = 1,
                                           nrow = ncol(rf_data) - 1,
                                           NA))
  var_imp_final_df[, 1] <- colnames(rf_data)[-ncol(rf_data)]
  colnames(var_imp_final_df) <- "drivers"


  # Running random forest n times:
  for (i in c(1:iteration_nb)) {

    # Split into training and testing sets:
    data_split <- rsample::initial_split(rf_data,
                                         prop = 0.8)
    train <- rsample::training(data_split)
    test <- rsample::testing(data_split)

    # Run the rf model on the training set:
    rf_mod <- randomForest::randomForest(ses~ .,
                          data = train,
                          mtry = 16,
                          importance = TRUE,
                          num.trees = 500)

    # test the model with the testing set:
    score <- predict(rf_mod, test)

    # Get the variables importance:
    var_imp_rf <- randomForest::importance(rf_mod)
    var_imp_rf_df <- as.data.frame(var_imp_rf) %>%
      tibble::rownames_to_column() %>%
      dplyr::rename("drivers" = rowname) %>%
      dplyr::select(-c("IncNodePurity"))

    # Print the model accuracy:
    print(rf_mod)

    # Update the dataframes:
    # Variable importance:
    var_imp_final_df <- dplyr::left_join(var_imp_final_df,
                                        var_imp_rf_df,
                                        by = "drivers")


    # Rename columns and remove NA rows if needed:
    if (i == iteration_nb) {
      colnames(var_imp_final_df)[-1] <- paste0("%IncMSE", sep = "_",
                                               "rfmod", sep = "_",
                                               c(1:iteration_nb))
    }

  }

  # For each variable, get the mean importance (also sd) and percentage:
  var_imp_mean_df <- var_imp_final_df %>%
    tibble::column_to_rownames("drivers")
  var_imp_mean_df$mean_imp <- apply(var_imp_mean_df, 1, mean)
  var_imp_mean_df$sd_imp <- apply(var_imp_mean_df, 1, sd)

  return(var_imp_mean_df)

}






#' Plot the variable importance for a given taxa and metric (lollipop plot)
#'
#' @param var_imp_df output of the \code{test.rf.model} function.
#'
#' @return a lollipop plot with drivers on columns and mean %IncMSE on x axis
#' with colors referring to drivers category
#'
#' @export
#'

varimp.plot <- function(var_imp_df) {


  # Add a new column that will refer to drivers categories:
  var_imp_plot_df <- var_imp_df
  var_imp_plot_df$cat <- rep("Past Land Use", nrow(var_imp_df))

  # Fill this new column:
  for (i in (1:nrow(var_imp_plot_df))) {

    if (rownames(var_imp_plot_df)[i] %in% c("Past_CCVelHolocene_mean.voccMag",
                                            "Past_CCVelLGM_mean.voccMag",
                                            "Past_CCVelShortTerm_mean.voccMag",
                                            "Past_CCVelYoungerDryas_mean.voccMag",
                                            "Past_MAT_sd",
                                            "Past_TAP_sd")) {
      var_imp_plot_df$cat[i] <- "Past Climate Stability"
    }

    if (rownames(var_imp_plot_df)[i] %in% c("Present_AI_stdev",
                                            "Present_MAT_stdev",
                                            "Present_TAP_stdev",
                                            "pH_stdev",
                                            "OC_stdev",
                                            "Elv_stdev",
                                            "Depth_stdev",
                                            "VWC_stdev")) {
      var_imp_plot_df$cat[i] <- "Habitat characteristics variation"
    }

    if (rownames(var_imp_plot_df)[i] %in% c("Present_AI_mean",
                                            "Present_MAT_mean",
                                            "Present_TAP_mean",
                                            "pH_mean",
                                            "OC_mean",
                                            "Elv_mean",
                                            "Depth_mean",
                                            "VWC_mean")) {
      var_imp_plot_df$cat[i] <- "Habitat characteristics mean"
    }

    if (rownames(var_imp_plot_df)[i] %in% c("Pr_FInt_2000_2023_median",
                                            "Pr_FInt_2000_2023_sd",
                                            "Pr_FSurf_2000_2023_pixels")) {
      var_imp_plot_df$cat[i] <- "Disturbances"
    }

    if (rownames(var_imp_plot_df)[i] %in% c("Past_Perc_croplands_Weighted_Mean",
                                            "Past_Perc_croplands_Weighted_Sd",
                                            "Past_Perc_dense_settlements_Weighted_Mean",
                                            "Past_Perc_dense_settlements_Weighted_Sd",
                                            "Past_Perc_rangelands_Weighted_Mean",
                                            "Past_Perc_rangelands_Weighted_Sd",
                                            "Past_Perc_seminatural_lands_Weighted_Mean",
                                            "Past_Perc_seminatural_lands_Weighted_Sd",
                                            "Past_Perc_villages_Weighted_Mean",
                                            "Past_Perc_villages_Weighted_Sd",
                                            "Past_Perc_wild_lands_Weighted_Mean",
                                            "Past_Perc_wild_lands_Weighted_Sd" )) {
      var_imp_plot_df$cat[i] <- "Past Land Use"
    }

    if (rownames(var_imp_plot_df)[i] %in% c("Present_Perc_croplands_Weighted_Mean",
                                            "Present_Perc_croplands_Weighted_Sd",
                                            "Present_Perc_dense_settlements_Weighted_Mean",
                                            "Present_Perc_dense_settlements_Weighted_Sd",
                                            "Present_Perc_rangelands_Weighted_Mean",
                                            "Present_Perc_rangelands_Weighted_Sd",
                                            "Present_Perc_seminatural_lands_Weighted_Mean",
                                            "Present_Perc_seminatural_lands_Weighted_Sd",
                                            "Present_Perc_villages_Weighted_Mean",
                                            "Present_Perc_villages_Weighted_Sd",
                                            "Present_Perc_wild_lands_Weighted_Mean",
                                            "Present_Perc_wild_lands_Weighted_Sd")) {
      var_imp_plot_df$cat[i] <- "Present Land Use"
    }

    if (rownames(var_imp_plot_df)[i] %in% c("Pr_Pop_2020_median",
                                            "Pr_RatePop_2020_median")) {
      var_imp_plot_df$cat[i] <- "Present Population"
    }

  }

  # Put drivers as column:
  var_imp_plot_df <- var_imp_plot_df %>%
    tibble::rownames_to_column("drivers")


  # Plot:
  var_plot <- ggpubr::ggdotchart(var_imp_plot_df,
                                 x = "drivers",
                                 y = "mean_imp",
                                 color = "cat",
                                 palette = c("tan1",
                                             "palegreen2",
                                             "palegreen4",
                                             "paleturquoise",
                                             "orchid4",
                                             "orchid",
                                             "orchid"),
                                 sorting = "descending",
                                 rotate = TRUE,
                                 add = "segments",
                                 dot.size = 4) +
    ggplot2::ylab("mean %IncMSE over 100 repetitions") +

    ggplot2::theme(ggpubr::theme_pubr()) +

    ggplot2::labs(color = "Drivers category")

    print(var_plot)


}



#' Plot a heatmap illustrating the importance of each variable for the three taxa
#'
#' @param rf_all_taxa_list a list containing the result of the
#' \code{test.rf.model} function for all the taxa studied.
#' @param metric_nm a vector containing the name of the studied metric
#' @param plot_nb a TRUE/FALSE value to indicate whether the mean%IncMSE is to
#' be plotted or not on the heatmap
#'
#' @return a heatmap with species on columns and drivers as rows,
#' gathered by type and colors refelecting variable importance.
#'
#' @export
#'

heatmap.varimp <- function(rf_all_taxa_list,
                           metric_nm,
                           plot_nb) {


  # Create a big data frame containing data for all the taxa studied:
  var_imp_df <- as.data.frame(matrix(ncol = 4, nrow = 1, NA))
  colnames(var_imp_df) <- c("Driver_nm", "Taxa", "Driver_cat", "mean%IncMSE")


  # Fill it:
  for (i in c(1:length(rf_all_taxa_list))) {

    # Retrieve the name of taxa studied:
    taxa_nm <- gsub("_rf", "", names(rf_all_taxa_list)[i])

    # Get the associated df, put drivers as a column, select col and new col:
    taxa_rf_df <- rf_all_taxa_list[[i]] %>%
      tibble::rownames_to_column("Driver_nm") %>%
      dplyr::select("Driver_nm", "mean_imp")
    taxa_rf_df$Driver_cat <- rep(NA, nrow(taxa_rf_df))

    # Add drivers category:
    for (j in (1:nrow(taxa_rf_df))) {

      if (taxa_rf_df$Driver_nm[j] %in% c("Past_CCVelHolocene_mean.voccMag",
                                              "Past_CCVelLGM_mean.voccMag",
                                              "Past_CCVelShortTerm_mean.voccMag",
                                              "Past_CCVelYoungerDryas_mean.voccMag",
                                              "Past_MAT_sd",
                                              "Past_TAP_sd")) {
        taxa_rf_df$Driver_cat[j] <- "Past Climate Stability"
      }

      if (taxa_rf_df$Driver_nm[j] %in% c("Present_AI_stdev",
                                              "Present_MAT_stdev",
                                              "Present_TAP_stdev",
                                              "pH_stdev",
                                              "OC_stdev",
                                              "Elv_stdev",
                                              "Depth_stdev",
                                              "VWC_stdev")) {
        taxa_rf_df$Driver_cat[j] <- "Habitat characteristics variation"
      }

      if (taxa_rf_df$Driver_nm[j] %in% c("Present_AI_mean",
                                              "Present_MAT_mean",
                                              "Present_TAP_mean",
                                              "pH_mean",
                                              "OC_mean",
                                              "Elv_mean",
                                              "Depth_mean",
                                              "VWC_mean")) {
        taxa_rf_df$Driver_cat[j] <- "Habitat characteristics mean"
      }

      if (taxa_rf_df$Driver_nm[j] %in% c("Pr_FInt_2000_2023_median",
                                              "Pr_FInt_2000_2023_sd",
                                              "Pr_FSurf_2000_2023_pixels")) {
        taxa_rf_df$Driver_cat[j] <- "Disturbances"
      }

      if (taxa_rf_df$Driver_nm[j] %in% c("Past_Perc_croplands_Weighted_Mean",
                                              "Past_Perc_croplands_Weighted_Sd",
                                              "Past_Perc_dense_settlements_Weighted_Mean",
                                              "Past_Perc_dense_settlements_Weighted_Sd",
                                              "Past_Perc_rangelands_Weighted_Mean",
                                              "Past_Perc_rangelands_Weighted_Sd",
                                              "Past_Perc_seminatural_lands_Weighted_Mean",
                                              "Past_Perc_seminatural_lands_Weighted_Sd",
                                              "Past_Perc_villages_Weighted_Mean",
                                              "Past_Perc_villages_Weighted_Sd",
                                              "Past_Perc_wild_lands_Weighted_Mean",
                                              "Past_Perc_wild_lands_Weighted_Sd" )) {
        taxa_rf_df$Driver_cat[j] <- "Past Land Use"
      }

      if (taxa_rf_df$Driver_nm[j] %in% c("Present_Perc_croplands_Weighted_Mean",
                                              "Present_Perc_croplands_Weighted_Sd",
                                              "Present_Perc_dense_settlements_Weighted_Mean",
                                              "Present_Perc_dense_settlements_Weighted_Sd",
                                              "Present_Perc_rangelands_Weighted_Mean",
                                              "Present_Perc_rangelands_Weighted_Sd",
                                              "Present_Perc_seminatural_lands_Weighted_Mean",
                                              "Present_Perc_seminatural_lands_Weighted_Sd",
                                              "Present_Perc_villages_Weighted_Mean",
                                              "Present_Perc_villages_Weighted_Sd",
                                              "Present_Perc_wild_lands_Weighted_Mean",
                                              "Present_Perc_wild_lands_Weighted_Sd")) {
        taxa_rf_df$Driver_cat[j] <- "Present Land Use"
      }

      if (taxa_rf_df$Driver_nm[j] %in% c("Pr_Pop_2020_median",
                                         "Pr_RatePop_2020_median")) {
        taxa_rf_df$Driver_cat[j] <- "Present Population"
      }

    } # end loop drivers category


    # Put taxa_rf_df in the right format so can be added to var_imp_df:
    taxa_rf_df$Taxa <- rep(taxa_nm, nrow(taxa_rf_df))
    taxa_rf_df <- taxa_rf_df %>%
      dplyr::rename("mean%IncMSE" = "mean_imp") %>%
      dplyr::select("Driver_nm", "Taxa", "Driver_cat", "mean%IncMSE")

    # Add it to the final df:
    var_imp_df <- rbind(var_imp_df, taxa_rf_df)

  } # end loop on all the taxa


  # Remove the first row which is NA:
  var_imp_df <- var_imp_df[-1, ]

  # Class drivers category/taxa as factor:
  var_imp_df$Driver_cat <- factor(var_imp_df$Driver_cat,
                                  levels = c("Past Climate Stability",
                                             "Habitat characteristics mean",
                                             "Habitat characteristics variation",
                                             "Disturbances",
                                             "Past Land Use",
                                             "Present Land Use",
                                             "Present Population"))
  var_imp_df$Taxa <- as.factor(var_imp_df$Taxa)


  # For each driver, compute the mean imp value over all taxa (to order plot):
  mean_impval_taxa <- var_imp_df %>%
    dplyr::group_by(Driver_nm) %>%
    dplyr::summarise(mean_over_taxa = mean(`mean%IncMSE`))

  # Link with the final db:
  var_imp_df <- dplyr::left_join(var_imp_df, mean_impval_taxa)

  # Order mean%IncMSE per driver per taxa according to mean values over taxa:
  var_imp_df <- dplyr::arrange(var_imp_df,
                               by = desc(mean_over_taxa))


  # Plot the heatmap with numbers:
  if (plot_nb == TRUE) {

    heatmap_plot <- ggplot2::ggplot(data = var_imp_df,
                                    ggplot2::aes(x = `Taxa`,
                                                 y = `Driver_nm`,
                                                 fill = `mean%IncMSE`)) +
      ggplot2::geom_raster() +

      ggplot2::geom_text(ggplot2::aes(label = round(`mean%IncMSE`, 2)), color = "white",
                         size = 3) +

      ggplot2::scale_fill_viridis_c(limits = c(min(var_imp_df$`mean%IncMSE`),
                                               max(var_imp_df$`mean%IncMSE`))) +

      ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white",
                                                              colour = "grey83"),
                     panel.grid.major = ggplot2::element_line(colour = "grey83"),
                     legend.title = ggplot2::element_text(size = 10),
                     legend.text = ggplot2::element_text(size = 10),
                     axis.title.x = ggplot2::element_text(colour = "grey55",
                                                          size = 9),
                     strip.text.y = ggplot2::element_text(angle = 0, size = 9,
                                                          colour = "grey55"),
                     strip.background = ggplot2::element_rect(fill = NA,
                                                              colour = NA)) +
      ggplot2::xlab("") +
      ggplot2::ylab("") +

      ggplot2::facet_grid(rows = ggplot2::vars(`Driver_cat`),
                          scales = "free", space = "free_y") +

      ggplot2::ggtitle(metric_nm)


    # Save it:
    ggplot2::ggsave(plot = heatmap_plot,
                    filename = here::here("outputs",
                                          paste0("varimp",
                                                 sep = "_",
                                                 metric_nm,
                                                 sep = "_",
                                                 "alltaxa_nb_50.pdf")),
                    device = "pdf",
                    scale = 0.9,
                    height = 5500,
                    width = 6000,
                    units = "px",
                    dpi = 600)

  }

  # Plot the heatmap without numbers:
  if (plot_nb == FALSE) {

    heatmap_plot <- ggplot2::ggplot(data = var_imp_df,
                                    ggplot2::aes(x = `Taxa`,
                                                 y = `Driver_nm`,
                                                 fill = `mean%IncMSE`)) +
      ggplot2::geom_raster() +

      ggplot2::scale_fill_viridis_c(limits = c(min(var_imp_df$`mean%IncMSE`),
                                               max(var_imp_df$`mean%IncMSE`))) +

      ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white",
                                                              colour = "grey83"),
                     panel.grid.major = ggplot2::element_line(colour = "grey83"),
                     legend.title = ggplot2::element_text(size = 10),
                     legend.text = ggplot2::element_text(size = 10),
                     axis.title.x = ggplot2::element_text(colour = "grey55",
                                                          size = 9),
                     strip.text.y = ggplot2::element_text(angle = 0, size = 9,
                                                          colour = "grey55"),
                     strip.background = ggplot2::element_rect(fill = NA,
                                                              colour = NA)) +
      ggplot2::xlab("") +
      ggplot2::ylab("") +

      ggplot2::facet_grid(rows = ggplot2::vars(`Driver_cat`),
                          scales = "free", space = "free_y") +

      ggplot2::ggtitle(metric_nm)


    # Save it:
    ggplot2::ggsave(plot = heatmap_plot,
                    filename = here::here("outputs",
                                          paste0("varimp",
                                                 sep = "_",
                                                 metric_nm,
                                                 sep = "_",
                                                 "alltaxa_nb_50.pdf")),
                    device = "pdf",
                    scale = 0.9,
                    height = 5500,
                    width = 6000,
                    units = "px",
                    dpi = 600)

  }

  print(heatmap_plot)

}

