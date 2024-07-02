################################################################################
##
## Function(s) to do the random forest analyses - n iteration of the rf model to
## ... test its performance
##
## Camille Magneville
##
## 23/05/2024
##
## Randomforest_fcts.R
##
################################################################################



#' Compute n iteration of the random forest model to test its accuracy and
#' variable importance and compute partial dependance plots
#'
#' @param rf_data a data frame containing data to run the random forest - ie
#' predictor variables in columns with the last column being the ses of a given
#' diversity metric. This last column must be named \code{ses}.
#'
#' @param iteration_nb the number of time the random forest should be repeated.
#'
#' @param metric_nm a character string referring to the name of the metric studied
#' (used for saving partial dependance files)
#'
#' @param taxa_nm a character string referring to the name of the taxa studied
#' (used for saving partial dependance files)
#'
#' @param plot a TRUE/FALSE value according to whether or not partial dependance
#' are to be plotted and saved (it takes a lot of time)
#'
#' @return a data frame with variable importance for each rf and the mean/sd.
#' It also returns partial dependance plot for each driver studied.
#'
#' @export
#'

test.rf.model <- function(rf_data,
                       iteration_nb,
                       metric_nm,
                       taxa_nm,
                       plot) {


  # Create one final db that will be returned:
  var_imp_final_df <- as.data.frame(matrix(ncol = 1,
                                           nrow = ncol(rf_data) - 1,
                                           NA))
  var_imp_final_df[, 1] <- colnames(rf_data)[-ncol(rf_data)]
  colnames(var_imp_final_df) <- "drivers"

  # create a list that will contains all rf results (from the n iterations):
  rf_models <- vector("list", iteration_nb)


  # Running random forest n times:
  for (i in c(1:iteration_nb)) {

    # Split into training and testing sets:
    # data_split <- rsample::initial_split(rf_data,
    #                                      prop = 0.8)
    # train <- rsample::training(data_split)
    # test <- rsample::testing(data_split)

    # Run the rf model on the training set:
    rf_mod <- randomForest::randomForest(ses~ .,
                          data = rf_data,
                          mtry = 16,
                          importance = TRUE,
                          num.trees = 500)

    # Put the output of the model in the rf vect:
    rf_models[[i]] <- rf_mod

    # test the model with the testing set:
    #score <- predict(rf_mod, test)

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


  # If partial dependance plots are to be plotted and saved:
  if (plot == TRUE) {

    # Do partial regression plot for each variable:
    # Get the list of predictor variables:
    variables <- names(rf_data)[names(rf_data) != "ses"]

    # Loop through each predictor variable:
    for (var in variables) {

      # Get combined partial dependence data for the variable:
      combined_pd <- plot.partial.dependence(rf_models, var, rf_data, iteration_nb)

      # Create a ggplot for the partial dependence data:
      p <- ggplot2::ggplot(combined_pd, ggplot2::aes(x = !!rlang::sym(var),
                                                     y = yhat,
                                                     group = Iteration)) +
        # Plot lines with transparency for each iteration:
        ggplot2::geom_line(alpha = 0.2, color = "aquamarine2") +

        ggplot2::labs(title = paste("Partial Dependence of", var),
                      x = var, y = "Partial Dependence") +
        ggplot2::theme_minimal() +
        # Remove legend:
        ggplot2::theme(legend.position = "none")

      print(p)

      ggplot2::ggsave(plot = p,
                      filename = here::here("outputs",
                                            "Partial_dependance_plots",
                                            paste0(metric_nm, sep = "_",
                                                   "50", sep = "_", taxa_nm,
                                                   sep = "_", var, ".jpeg")),
                      device = "jpeg",
                      scale = 1.6,
                      height = 1600,
                      width = 1800,
                      units = "px",
                      dpi = 600)

  }



  }

  return(var_imp_mean_df)

}



#' Take a list of models, a predictor variable name, and the training data,
#' then computes and combines the partial dependence plots for the given
#' variable across all models.
#'
#' @param models
#' @param var_name
#' @param data
#'
#' @return
#' @export
#'

plot.partial.dependence <- function(models, var_name, data, iteration_nb) {

  # Generate partial dependence plots for each model and the studied variable:
  partial_plots <- lapply(models, function(model) {
    pdp::partial(object = model, pred.var = var_name, train = data,
                 plot = FALSE)
  })

  # Combine results into a single data frame:
  combined_pd <- do.call(rbind, lapply(partial_plots, function(p) data.frame(p)))
  combined_pd$Iteration <- rep(1:iteration_nb, each = nrow(partial_plots[[1]]))

  return(combined_pd)

}



#' Plot the variable importance for a given taxa and metric (lollipop plot)
#'
#' @param var_imp_df output of the \code{test.rf.model} function.
#'
#' @param max a number referring to the maximal value that the x axis should take
#' ie. max over the five taxa of interest for one metric.
#'
#' @return a lollipop plot with drivers on columns and mean %IncMSE on x axis
#' with colors referring to drivers category
#'
#' @export
#'

varimp.plot <- function(var_imp_df,
                        max) {


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
                                            "Present_AI_mean",
                                            "Present_MAT_mean",
                                            "Present_TAP_mean")) {
      var_imp_plot_df$cat[i] <- "Present Climate"
    }

    if (rownames(var_imp_plot_df)[i] %in% c("pH_mean",
                                            "OC_mean",
                                            "Elv_mean",
                                            "Depth_mean",
                                            "VWC_mean",
                                            "pH_stdev",
                                            "OC_stdev",
                                            "Elv_stdev",
                                            "Depth_stdev",
                                            "VWC_stdev")) {
      var_imp_plot_df$cat[i] <- "Present Habitat Characteristics"
    }

    if (rownames(var_imp_plot_df)[i] %in% c("Pr_FInt_2000_2023_mean",
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

    if (rownames(var_imp_plot_df)[i] %in% c("Pr_Pop_2020_mean",
                                            "Pr_RatePop_2020_mean")) {
      var_imp_plot_df$cat[i] <- "Present Population"
    }

  }

  # Add a new column about whether it's sd or mean:
  var_imp_plot_df$var <- rep("sd", nrow(var_imp_df))

  # Fill this new column:
  for (i in (1:nrow(var_imp_plot_df))) {

    if (grepl("mean", rownames(var_imp_plot_df)[i])) {
      var_imp_plot_df$var[i] <- "mean"
    }

  }

  # Put drivers as column:
  var_imp_plot_df <- var_imp_plot_df %>%
    tibble::rownames_to_column("drivers")

  # Order drivers column:
  var_imp_plot_df$cat <- factor(var_imp_plot_df$cat,
                                  levels = c("Past Climate Stability",
                                             "Present Climate",
                                             "Present Habitat Characteristics",
                                             "Disturbances",
                                             "Past Land Use",
                                             "Present Land Use",
                                             "Present Population"))


  # Plot:
  var_plot <- ggpubr::ggdotchart(var_imp_plot_df,
                                 x = "drivers",
                                 y = "mean_imp",
                                 color = "cat",
                                 palette = c("darkslategray3",
                                             "paleturquoise",
                                             "palegreen2",
                                             "tan1",
                                             "orchid4",
                                             "orchid",
                                             "plum2"),
                                 shape = "var",
                                 sorting = "descending",
                                 rotate = TRUE,
                                 add = "segments",
                                 dot.size = 3.5,
                                 alpha = 0.6) +

    ggplot2::ylim(0, max) +
    ggplot2::ylab("mean %IncMSE over 100 repetitions") +

    ggplot2::theme(ggpubr::theme_pubr()) +

    ggplot2::labs(color = "Drivers category",
                  shape = "Metric")

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

      if (taxa_rf_df$Driver_nm[j] %in% c("Pr_FInt_2000_2023_mean",
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

      if (taxa_rf_df$Driver_nm[j] %in% c("Pr_Pop_2020_mean",
                                         "Pr_RatePop_2020_mean")) {
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
                                                 "alltaxa_50.pdf")),
                    device = "pdf",
                    scale = 0.9,
                    height = 5500,
                    width = 6000,
                    units = "px",
                    dpi = 600)

  }

  print(heatmap_plot)

}



#' Create a data frame to plot the circular plot of drivers for each taxa
#'
#' @param rf_df_list a list containing the rf results from the
#' \code{test.rf.model} function - each element should be given the name of the
#' metric studied.
#'
#' @param var_nb a number referring to the number of variables that should be
#' kept in the graph - variables ordered according to their importance.
#'
#' @return a data frame with the following columns: Driver_nm, Div_metric,
#' Driver_imp, Driv_cat
#'
#' @export
#'

create.df.circular.plot <- function(rf_df_list,
                                    var_nb) {


  # Build the final df that will be the one to return - fill it with first rf df:
  final_df <- rf_df_list[[1]]
  metric_nm <- names(rf_df_list)[1]
  # Remove unused variables and rename column based on metric name:
  final_df <- final_df %>%
    tibble::rownames_to_column("Drivers_nm") %>%
    dplyr::select(c("Drivers_nm", "mean_imp"))
  # Add a new column with the diversity metric name:
  final_df$Div_metric <- rep(metric_nm, nrow(final_df))

  # Add Drivers category:
  final_df$Drivers_cat <- rep("Past Climate Stability", nrow(final_df))
  # Fill this new column:
  for (i in (1:nrow(final_df))) {

    if (final_df$Drivers_nm[i] %in% c("Present_AI_stdev",
                                      "Present_MAT_stdev",
                                      "Present_TAP_stdev",
                                      "pH_stdev",
                                      "OC_stdev",
                                      "Elv_stdev",
                                      "Depth_stdev",
                                      "VWC_stdev")) {
      final_df$Drivers_cat[i] <- "Present Habitat variations"
    }

    if (final_df$Drivers_nm[i] %in% c("pH_mean",
                                      "OC_mean",
                                      "Elv_mean",
                                      "Depth_mean",
                                      "VWC_mean",
                                      "Present_AI_mean",
                                      "Present_MAT_mean",
                                      "Present_TAP_mean")) {
      final_df$Drivers_cat[i] <- "Present Habitat mean"
    }

    if (final_df$Drivers_nm[i] %in% c("Pr_FInt_2000_2023_mean",
                                      "Pr_FInt_2000_2023_sd",
                                      "Pr_FSurf_2000_2023_pixels")) {
      final_df$Drivers_cat[i] <- "Disturbances"
    }

    if (final_df$Drivers_nm[i] %in% c("Past_Perc_croplands_Weighted_Mean",
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
      final_df$Drivers_cat[i] <- "Past Land Use"
    }

    if (final_df$Drivers_nm[i] %in% c("Present_Perc_croplands_Weighted_Mean",
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
      final_df$Drivers_cat[i] <- "Present Land Use"
    }

    if (final_df$Drivers_nm[i] %in% c("Pr_Pop_2020_mean",
                                      "Pr_RatePop_2020_mean")) {
      final_df$Drivers_cat[i] <- "Present Population"
    }

  }

  # Only keep the first n variables:
  final_df <- dplyr::arrange(final_df, desc(mean_imp))
  # Only keep the first "var_nb" variables:
  final_df <- final_df[c(1:var_nb), ]


  # Now do a loop on the other data frames (one for each diversity metric left):
  for (j in (2:length(rf_df_list))) {

    # Build a temp df that will be rowbin with the final_df one:
    temp_df <- rf_df_list[[j]]
    metric_nm <- names(rf_df_list)[j]
    # Remove unused variables and rename column based on metric name:
    temp_df <- temp_df %>%
      tibble::rownames_to_column("Drivers_nm") %>%
      dplyr::select(c("Drivers_nm", "mean_imp"))
    # Add a new column with the diversity metric name:
    temp_df$Div_metric <- rep(metric_nm, nrow(temp_df))

    # Add Drivers category:
    temp_df$Drivers_cat <- rep("Past Climate Stability", nrow(temp_df))
    # Fill this new column:
    for (i in (1:nrow(temp_df))) {

      if (temp_df$Drivers_nm[i] %in% c("Present_AI_stdev",
                                        "Present_MAT_stdev",
                                        "Present_TAP_stdev",
                                        "pH_stdev",
                                        "OC_stdev",
                                        "Elv_stdev",
                                        "Depth_stdev",
                                        "VWC_stdev")) {
        temp_df$Drivers_cat[i] <- "Present Habitat variations"
      }

      if (temp_df$Drivers_nm[i] %in% c("pH_mean",
                                        "OC_mean",
                                        "Elv_mean",
                                        "Depth_mean",
                                        "VWC_mean",
                                        "Present_AI_mean",
                                        "Present_MAT_mean",
                                        "Present_TAP_mean")) {
        temp_df$Drivers_cat[i] <- "Present Habitat mean"
      }

      if (temp_df$Drivers_nm[i] %in% c("Pr_FInt_2000_2023_mean",
                                        "Pr_FInt_2000_2023_sd",
                                        "Pr_FSurf_2000_2023_pixels")) {
        temp_df$Drivers_cat[i] <- "Disturbances"
      }

      if (temp_df$Drivers_nm[i] %in% c("Past_Perc_croplands_Weighted_Mean",
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
        temp_df$Drivers_cat[i] <- "Past Land Use"
      }

      if (temp_df$Drivers_nm[i] %in% c("Present_Perc_croplands_Weighted_Mean",
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
        temp_df$Drivers_cat[i] <- "Present Land Use"
      }

      if (temp_df$Drivers_nm[i] %in% c("Pr_Pop_2020_mean",
                                        "Pr_RatePop_2020_mean")) {
        temp_df$Drivers_cat[i] <- "Present Population"
      }

    }

    # Only keep the first n variables:
    temp_df <- dplyr::arrange(temp_df, desc(mean_imp))
    # Only keep the first "var_nb" variables:
    temp_df <- temp_df[c(1:var_nb), ]

    final_df <- rbind(final_df, temp_df)

  } # loop on the diversity df

  return(final_df)

}




#' Plot n drivers for a given taxa and all diversity metrics
#'
#' @param taxa_plot_df data frame from the \code{create.df.circular.plot} function
#' @param drivers_nm_df a data frame containing shortened names of drivers
#' @param palette a color palette containing the color names of the category present
#' in the \code{taxa_plot_df}
#' @param div_facet a character string referring to whether FD of PD is studied here.
#' It could be either \code{"FD"} or \code{"PD"}.
#'
#' @return
#'
#' @export
#'


circular.drivers.plot <- function(taxa_plot_df,
                                  drivers_nm_df,
                                  palette,
                                  div_facet) {


  # Set classes:
  taxa_plot_df$Div_metric <- as.factor(taxa_plot_df$Div_metric)
  taxa_plot_df$Drivers_cat <- as.factor(taxa_plot_df$Drivers_cat)

  # Order drivers column:
  taxa_plot_df$Drivers_cat <- factor(taxa_plot_df$Drivers_cat,
                                     levels = c("Past Climate Stability",
                                                "Present Habitat mean",
                                                "Present Habitat variations",
                                                "Disturbances",
                                                "Past Land Use",
                                                "Present Land Use",
                                                "Present Population"))

  # Decreasing variable importance:
  taxa_plot_df <- taxa_plot_df %>%
    dplyr::arrange(dplyr::desc(mean_imp))

  # Add shortened names of drivers:
  taxa_plot_df <- dplyr::left_join(taxa_plot_df,
                                   drivers_nm_df,
                                   by = "Drivers_nm")

  # Set a number of 'empty bars' to add at the end of each group:
  empty_bar <- 3
  to_add <- data.frame(matrix(NA, empty_bar*nlevels(taxa_plot_df$Div_metric),
                              ncol(taxa_plot_df)))
  colnames(to_add) <- colnames(taxa_plot_df)
  to_add$Div_metric <- rep(levels(taxa_plot_df$Div_metric), each = empty_bar)
  taxa_plot_df <- rbind(taxa_plot_df, to_add)
  taxa_plot_df <- taxa_plot_df %>%
    dplyr::arrange(Div_metric)
  taxa_plot_df$ind <- seq(1, nrow(taxa_plot_df))


  # Get the name and the y position of each label
  label_data <- taxa_plot_df
  number_of_bar <- nrow(label_data)
  angle <- 90 - 360 * (label_data$ind-0.5)/number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
  label_data$hjust <- ifelse( angle < -90, 1, 0)
  label_data$angle <- ifelse(angle < -90, angle+180, angle)


  # Prepare a data frame for base lines
  base_data <- taxa_plot_df %>%
    dplyr::group_by(Div_metric) %>%
    dplyr::summarize(start = min(ind), end = max(ind) - empty_bar) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(title = mean(c(start, end)))

  # Prepare a data frame for grid (scales)
  grid_data <- base_data
  grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
  grid_data$start <- grid_data$start - 1
  grid_data <- grid_data[-1,]

  # Prepare a dataframe for metrics labels
  if (div_facet == "PD") {
    data_labmetric <- data.frame(x = c(8, 26, 44),
                                 y = c(-8, -8, -8),
                                 label = c("PD Dispersion",
                                           "PD Originality",
                                           "PD Richness"))
  }
  if (div_facet == "FD") {
    data_labmetric <- data.frame(x = c(8, 26, 44),
                                 y = c(-8, -8, -8),
                                 label = c("FD Dispersion",
                                           "FD Originality",
                                           "FD Richness"))
  }




  # Make the plot
  drivers_circ_plot <- ggplot2::ggplot(data = taxa_plot_df,
                                       ggplot2::aes(x = as.factor(ind),
                                                    y = mean_imp,
                                                    fill = Div_metric)) +

    ggplot2::geom_bar(ggplot2::aes(x = as.factor(ind),
                                   y = mean_imp,
                                   fill = Drivers_cat),
                      stat = "identity", alpha = 0.5) +

    ggplot2::scale_fill_manual(values = palette) +

    # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
    ggplot2::geom_segment(data = grid_data,
                          ggplot2::aes(x = end, y = 80, xend = start, yend = 80),
                          colour = "grey", alpha = 1, size = 0.3 ,
                          inherit.aes = FALSE ) +
    ggplot2::geom_segment(data = grid_data,
                          ggplot2::aes(x = end, y = 60, xend = start, yend = 60),
                          colour = "grey", alpha = 1, size = 0.3 ,
                          inherit.aes = FALSE ) +
    ggplot2::geom_segment(data = grid_data,
                          ggplot2::aes(x = end, y = 40, xend = start, yend = 40),
                          colour = "grey", alpha = 1, size = 0.3,
                          inherit.aes = FALSE ) +
    ggplot2::geom_segment(data = grid_data,
                          ggplot2::aes(x = end, y = 20, xend = start, yend = 20),
                          colour = "grey", alpha = 1, size = 0.3 ,
                          inherit.aes = FALSE ) +

    # Add text showing the value of lines
    ggplot2::annotate("text", x = c(max(taxa_plot_df$ind), max(taxa_plot_df$ind)),
                      y = c(20, 40),
                      label = c("20", "40"), color = "grey",
                      size = 3, angle = 0, fontface = "bold", hjust = 1) +

    ggplot2::ylim(-50,40) +

    ggplot2::labs(fill = "Drivers category") +

    ggplot2::theme_minimal() +

    ggplot2::theme(
      legend.position = "none",
      legend.text = ggplot2::element_text(size = 7),
      axis.text = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      plot.margin = grid::unit(rep(-1,4), "cm")) +

    ggplot2::coord_polar() +

    ggplot2::geom_text(data = label_data, ggplot2::aes(x = ind,
                                                       y = mean_imp + 10,
                                                       label = Drivers_short_nm,
                                                       hjust = hjust),
                       color = "black", alpha = 0.6, fontface = "bold",
                       size = 2.5, angle = label_data$angle,
                       inherit.aes = FALSE) +

    # Add metrics retangles and name:
    ggplot2::geom_rect(ggplot2::aes(xmin = 0, xmax = 16,
                                    ymin = -15, ymax = -2),
                       fill = "grey80",
                       alpha = 0.7,
                       color = "white",
                       size = 2) +
    ggplot2::geom_rect(ggplot2::aes(xmin = 18, xmax = 34,
                                  ymin = -15, ymax = -2),
                      fill = "grey80",
                      alpha = 0.7,
                      color = "white",
                      size = 2) +
    ggplot2::geom_rect(ggplot2::aes(xmin = 36, xmax = 52,
                                  ymin = -15, ymax = -2),
                      fill = "grey80",
                      alpha = 0.7,
                      color = "white",
                      size = 2) +

    geomtextpath::geom_textpath(data = data_labmetric,
                                ggplot2::aes(x = x, y = y, label = label),
                                size = 4,
                                color = "white",
                                fontface = "bold",
                                inherit.aes = FALSE)

  return(drivers_circ_plot)

}
