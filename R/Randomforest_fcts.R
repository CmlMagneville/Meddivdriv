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
#' @param rf_all_taxa_vect a vector containing the result of the
#' \code{test.rf.model} function for all the taxa studied.
#'
#' @return
#'
#' @export
#'

heatmap.varimp <- function(rf_all_taxa_vect) {



}

