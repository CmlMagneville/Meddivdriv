################################################################################
##
## Script gathering functions which help to study the directionality of
## ... the effect of the main drivers for all taxa and diversity dimensions
##
## Camille Magneville
##
##  11/2024
##
## Main_drivers_effects_fcts.R
##
################################################################################



#' Contingency table, Chi2 and associated plots to get drivers effect
#'
#' @param driver_ses_df a dataframe linking Idgrid, drivers values and ses
#' values for each cell (rows)
#' @param driver_nm a character string referring to the name of the driver to
#' study
#' @param color_nms a vector containing the hexadecimal names of two colors used
#' to plot low and high values (negative/positive) of the studied driver
#' @param drivers_nm_df the dataframe containing shorten names of each driver
#' @param facet_nm the name of the facet to be studied, either FD or PD
#' @param dim_nm the name of the dimension of the facte to be studied, Richness,
#' Dispersion or Originality
#' @param taxa_nm the name of the taxa to be studied
#'
#' @return
#' @export
#'

contingency.analyses <- function(driver_ses_df,
                                 driver_nm,
                                 color_nms,
                                 drivers_nm_df,
                                 facet_nm,
                                 dim_nm,
                                 taxa_nm) {


  # Only keep interesting columns:
  simple_driver_ses_df <- driver_ses_df[, which(colnames(driver_ses_df) %in% c("Idgrid",
                                                                              "ses",
                                                                              driver_nm))]

  # Print an histogram:
  hist(simple_driver_ses_df[, driver_nm])
  abline(v = mean(simple_driver_ses_df[, driver_nm]), col = "red4")
  abline(v = mean(simple_driver_ses_df[, driver_nm])
         + sd(simple_driver_ses_df[, driver_nm]), col = "blue4")
  abline(v = mean(simple_driver_ses_df[, driver_nm])
         - sd(simple_driver_ses_df[, driver_nm]), col = "blue4")

  # If the driver is not velocity or growth rate - compute the mean:
  if (! driver_nm %in% c("Past_CCVelHolocene_mean.voccMag",
                        "Past_CCVelLGM_mean.voccMag",
                        "Past_CCVelShortTerm_mean.voccMag",
                        "Past_CCVelYoungerDryas_mean.voccMag",
                        "Pr_RatePop_2020_mean")) {

    threshold_value <- mean(simple_driver_ses_df[, driver_nm])

    # Complete the table with high/low compared to the mean:
    cat_drivers_ses_df <- simple_driver_ses_df %>%
      dplyr::mutate(driver_cat = dplyr::if_else(get(driver_nm) > threshold_value,
                                                "high", "low"))

    # Complete the table with +/- for ses:
    cat_drivers_ses_df <- cat_drivers_ses_df %>%
      dplyr::mutate(ses_cat = dplyr::if_else(ses > 0,
                                             "positive", "negative"))

    # Only keep the columns with categories and rename them:
    final_df <- cat_drivers_ses_df[, c(1, 4, 5)]
    colnames(final_df)[2] <- "driver"
    colnames(final_df)[3] <- "ses"

  }

  # If the driver is velocity or growth rate - use 0 as a threshold:
  if (driver_nm %in% c("Past_CCVelHolocene_mean.voccMag",
                       "Past_CCVelLGM_mean.voccMag",
                       "Past_CCVelShortTerm_mean.voccMag",
                       "Past_CCVelYoungerDryas_mean.voccMag",
                       "Pr_RatePop_2020_mean")) {

    threshold_value <- 0

    # Complete the table with high/low compared to 0:
    cat_drivers_ses_df <- simple_driver_ses_df %>%
      dplyr::mutate(driver_cat = dplyr::if_else(get(driver_nm) > threshold_value,
                                                "increase", "decrease"))

    # Complete the table with +/- for ses:
    cat_drivers_ses_df <- cat_drivers_ses_df %>%
      dplyr::mutate(ses_cat = dplyr::if_else(ses > 0,
                                             "positive", "negative"))

    # Only keep the columns with categories and rename them:
    final_df <- cat_drivers_ses_df[, c(1, 4, 5)]
    colnames(final_df)[2] <- "driver"
    colnames(final_df)[3] <- "ses"

  }


  # Create the contingency table based on this data:
  # Table with total counts in each category:
  print("Count table")
  count_table <- xtabs(~ driver + ses, data = final_df)
  print(count_table)
  print("Frequency table")
  print((count_table/nrow(final_df))*100)

  # Do the Chi squared test of independence:
  # First check that ok to do the test: expected values > 5
  print("Expected values if no association - should all be higher than 5")
  expected <- chisq.test(count_table)$expected
  print(expected)
  # pvalue significant (inf 0.05) - signif association between color and driver:
  print("Chi2 test test - pvalue < 0.05 significant association")
  test <- chisq.test(count_table)
  print(test)


  # Mosaic plot:
  mosaic_plot <- ggplot2::ggplot(data = final_df) +
    ggmosaic::geom_mosaic(ggplot2::aes(x = ggmosaic::product(ses),
                                       fill = driver),
                          show.legend = FALSE) +
    ggmosaic::theme_mosaic() +
    ggplot2::scale_fill_manual(values = color_nms) +
    ggplot2::labs(y = drivers_nm_df$Drivers_short_nm[which(drivers_nm_df$Drivers_nm == driver_nm)],
                  x = "SES")
  mosaic_plot

  # Save it:
  ggplot2::ggsave(plot = mosaic_plot,
                  filename = here::here("outputs",
                                        "directionality",
                                        paste0("mosaic_plot", sep = "_",
                                        driver_nm, sep = "_",
                                        facet_nm, sep = "_",
                                        dim_nm, sep = "_",
                                        taxa_nm, sep = "",
                                        ".jpeg")),
                  device = "jpeg",
                  scale = 1,
                  height = 3000,
                  width = 3000,
                  units = "px",
                  dpi = 600)


  # Continuous Plot:

  # Rename the driver column:
  simple_driver_plot_df <- simple_driver_ses_df
  colnames(simple_driver_plot_df)[2] <- "driver"

  continuous_plot <- ggplot2::ggplot(data = simple_driver_plot_df) +
    ggplot2::geom_jitter(ggplot2::aes(x = ses, y = 0, color = driver),
                         height = 0.2, size = 2) +
    ggplot2::scale_colour_gradient(low = "#ccdbe2", high = "#0881bd") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.ticks.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank()) +
    ggplot2::coord_cartesian(ylim = c(-0.4, 0.4),
                             xlim = c(-max(abs(simple_driver_plot_df$driver)),
                             max(abs(simple_driver_plot_df$driver)))) +
    ggplot2::labs(color = drivers_nm_df$Drivers_short_nm[which(drivers_nm_df$Drivers_nm == driver_nm)])

  continuous_plot

  ggplot2::ggsave(plot = continuous_plot,
                  filename = here::here("outputs",
                                        "directionality",
                                        paste0("continuous_plot", sep = "_",
                                               driver_nm, sep = "_",
                                               facet_nm, sep = "_",
                                               dim_nm, sep = "_",
                                               taxa_nm, sep = "",
                                               ".jpeg")),
                  device = "jpeg",
                  scale = 1,
                  height = 3000,
                  width = 4000,
                  units = "px",
                  dpi = 600)


}
