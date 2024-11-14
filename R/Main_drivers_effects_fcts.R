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
#' @param threshold_type a character string referring to whether the threshold
#' should divide the driver data in two (mean, 0 or mean of positive values), or
#' only study extreme values (superior to 75% quantile and inferior to 25% quantiles)
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
                                 threshold_type,
                                 drivers_nm_df,
                                 facet_nm,
                                 dim_nm,
                                 taxa_nm) {


  # Only keep interesting columns:
  simple_driver_ses_df <- driver_ses_df[, which(colnames(driver_ses_df) %in% c("Idgrid",
                                                                              "ses",
                                                                              driver_nm))]

  # If the threshold is one which divides the data in two:
  if (threshold_type == "normal") {


    # If the driver is not velocity or growth rate - compute the mean:
    if (! driver_nm %in% c("Past_CCVelHolocene_mean.voccMag",
                           "Past_CCVelLGM_mean.voccMag",
                           "Past_CCVelShortTerm_mean.voccMag",
                           "Past_CCVelYoungerDryas_mean.voccMag",
                           "Pr_RatePop_2020_mean")) {

      # Print an histogram:
      hist(simple_driver_ses_df[, driver_nm])
      abline(v = mean(simple_driver_ses_df[, driver_nm]), col = "red4")
      abline(v = mean(simple_driver_ses_df[, driver_nm])
             + sd(simple_driver_ses_df[, driver_nm]), col = "blue4")
      abline(v = mean(simple_driver_ses_df[, driver_nm])
             - sd(simple_driver_ses_df[, driver_nm]), col = "blue4")

      # Define the mean as the threshold value:
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

    # If the driver is velocity - use mean of positive values as a threshold:
    if (driver_nm %in% c("Past_CCVelLGM_mean.voccMag",
                         "Past_CCVelYoungerDryas_mean.voccMag")) {



      # Define the mean of positive value as the threshold:
      threshold_value <- mean(simple_driver_ses_df[which(simple_driver_ses_df[, driver_nm] > 0), driver_nm])

      # Print an histogram:
      hist(simple_driver_ses_df[, driver_nm])
      abline(v = threshold_value, col = "red4")

      # Complete the table with high/low compared to 0:
      cat_drivers_ses_df <- simple_driver_ses_df %>%
        dplyr::mutate(driver_cat = dplyr::if_else(get(driver_nm) > threshold_value,
                                                  "high increase", "decrease or low increase"))

      # Complete the table with +/- for ses:
      cat_drivers_ses_df <- cat_drivers_ses_df %>%
        dplyr::mutate(ses_cat = dplyr::if_else(ses > 0,
                                               "positive", "negative"))

      # Only keep the columns with categories and rename them:
      final_df <- cat_drivers_ses_df[, c(1, 4, 5)]
      colnames(final_df)[2] <- "driver"
      colnames(final_df)[3] <- "ses"

    }

    # If the driver is velocity YD increase use mean as a threshold (but only - values):
    if (driver_nm == "Past_CCVelShortTerm_mean.voccMag") {


      # Define the mean of positive value as the threshold:
      threshold_value <- mean(simple_driver_ses_df[, driver_nm])

      # Print an histogram:
      hist(simple_driver_ses_df[, driver_nm])
      abline(v = threshold_value, col = "red4")

      # Complete the table with high/low compared to 0:
      cat_drivers_ses_df <- simple_driver_ses_df %>%
        dplyr::mutate(driver_cat = dplyr::if_else(get(driver_nm) > threshold_value,
                                                  "low decrease", "high decrease"))

      # Complete the table with +/- for ses:
      cat_drivers_ses_df <- cat_drivers_ses_df %>%
        dplyr::mutate(ses_cat = dplyr::if_else(ses > 0,
                                               "positive", "negative"))

      # Only keep the columns with categories and rename them:
      final_df <- cat_drivers_ses_df[, c(1, 4, 5)]
      colnames(final_df)[2] <- "driver"
      colnames(final_df)[3] <- "ses"

    }

    # If the driver is velocity Holocene - use mean as a threshold (but only + values):
    if (driver_nm == "Past_CCVelHolocene_mean.voccMag") {


      # Define the mean of positive value as the threshold:
      threshold_value <- mean(simple_driver_ses_df[, driver_nm])

      # Print an histogram:
      hist(simple_driver_ses_df[, driver_nm])
      abline(v = threshold_value, col = "red4")

      # Complete the table with high/low compared to 0:
      cat_drivers_ses_df <- simple_driver_ses_df %>%
        dplyr::mutate(driver_cat = dplyr::if_else(get(driver_nm) > threshold_value,
                                                  "high increase", "low increase"))

      # Complete the table with +/- for ses:
      cat_drivers_ses_df <- cat_drivers_ses_df %>%
        dplyr::mutate(ses_cat = dplyr::if_else(ses > 0,
                                               "positive", "negative"))

      # Only keep the columns with categories and rename them:
      final_df <- cat_drivers_ses_df[, c(1, 4, 5)]
      colnames(final_df)[2] <- "driver"
      colnames(final_df)[3] <- "ses"

    }




    # If the driver is growth rate - use 0 as a threshold:
    if (driver_nm == "Pr_RatePop_2020_mean") {

      threshold_value <- 0

      # Print an histogram:
      hist(simple_driver_ses_df[, driver_nm])
      abline(v = 0, col = "red4")

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


  } # end - if threshold_type == normal


  # if the threshold type only studies the most extreme values (25 and 75 quantiles):
  if (threshold_type == "extremes") {


    # - Only labels are changing here, not thresholds -

    # If the driver is not velocity or growth rate:
    if (! driver_nm %in% c("Past_CCVelHolocene_mean.voccMag",
                           "Past_CCVelLGM_mean.voccMag",
                           "Past_CCVelShortTerm_mean.voccMag",
                           "Past_CCVelYoungerDryas_mean.voccMag",
                           "Pr_RatePop_2020_mean")) {

      # Define the thwo quantiles:
      threshold_value_low <- quantile(simple_driver_ses_df[, driver_nm])[[2]]
      threshold_value_high <- quantile(simple_driver_ses_df[, driver_nm])[[4]]


      # Print an histogram:
      hist(simple_driver_ses_df[, driver_nm])
      abline(v = threshold_value_low, col = "blue4")
      abline(v = threshold_value_high, col = "blue4")

      # Complete the table with high/low and remove rows in the middle:
      cat_drivers_ses_df <- simple_driver_ses_df %>%
        dplyr::mutate(driver_cat = dplyr::case_when(
          get(driver_nm) >= threshold_value_high ~ "extremely high",
          get(driver_nm) <= threshold_value_low ~ "extremely low",
          TRUE ~ "other")) %>%
        dplyr::filter(driver_cat != "other")

      # Complete the table with +/- for ses:
      cat_drivers_ses_df <- cat_drivers_ses_df %>%
        dplyr::mutate(ses_cat = dplyr::if_else(ses > 0,
                                               "positive", "negative"))

      # Only keep the columns with categories and rename them:
      final_df <- cat_drivers_ses_df[, c(1, 4, 5)]
      colnames(final_df)[2] <- "driver"
      colnames(final_df)[3] <- "ses"

    }

    # If the driver is velocity:
    if (driver_nm %in% c("Past_CCVelLGM_mean.voccMag",
                         "Past_CCVelYoungerDryas_mean.voccMag")) {



      # Define the thwo quantiles:
      threshold_value_low <- quantile(simple_driver_ses_df[, driver_nm])[[2]]
      threshold_value_high <- quantile(simple_driver_ses_df[, driver_nm])[[4]]


      # Print an histogram:
      hist(simple_driver_ses_df[, driver_nm])
      abline(v = threshold_value_low, col = "blue4")
      abline(v = threshold_value_high, col = "blue4")

      # Complete the table with high/low and remove rows in the middle:
      cat_drivers_ses_df <- simple_driver_ses_df %>%
        dplyr::mutate(driver_cat = dplyr::case_when(
          get(driver_nm) >= threshold_value_high ~ "extremely high increase",
          get(driver_nm) <= threshold_value_low ~ "extremely low increase or decrease",
          TRUE ~ "other")) %>%
        dplyr::filter(driver_cat != "other")
      # Complete the table with +/- for ses:
      cat_drivers_ses_df <- cat_drivers_ses_df %>%
        dplyr::mutate(ses_cat = dplyr::if_else(ses > 0,
                                               "positive", "negative"))

      # Only keep the columns with categories and rename them:
      final_df <- cat_drivers_ses_df[, c(1, 4, 5)]
      colnames(final_df)[2] <- "driver"
      colnames(final_df)[3] <- "ses"

    }

    # If the driver is velocity YD Increase :
    if (driver_nm %in% c("Past_CCVelShortTerm_mean.voccMag")) {


      # Define the two quantiles:
      threshold_value_low <- quantile(simple_driver_ses_df[, driver_nm])[[2]]
      threshold_value_high <- quantile(simple_driver_ses_df[, driver_nm])[[4]]


      # Print an histogram:
      hist(simple_driver_ses_df[, driver_nm])
      abline(v = threshold_value_low, col = "blue4")
      abline(v = threshold_value_high, col = "blue4")

      # Complete the table with high/low and remove rows in the middle:
      cat_drivers_ses_df <- simple_driver_ses_df %>%
        dplyr::mutate(driver_cat = dplyr::case_when(
          get(driver_nm) >= threshold_value_high ~ "extremely low decrease",
          get(driver_nm) <= threshold_value_low ~ "extremely high decrease",
          TRUE ~ "other")) %>%
        dplyr::filter(driver_cat != "other")
      # Complete the table with +/- for ses:
      cat_drivers_ses_df <- cat_drivers_ses_df %>%
        dplyr::mutate(ses_cat = dplyr::if_else(ses > 0,
                                               "positive", "negative"))

      # Only keep the columns with categories and rename them:
      final_df <- cat_drivers_ses_df[, c(1, 4, 5)]
      colnames(final_df)[2] <- "driver"
      colnames(final_df)[3] <- "ses"

    }

    # If the driver is velocity Holocene :
    if (driver_nm %in% c("Past_CCVelHolocene_mean.voccMag")) {


      # Define the two quantiles:
      threshold_value_low <- quantile(simple_driver_ses_df[, driver_nm])[[2]]
      threshold_value_high <- quantile(simple_driver_ses_df[, driver_nm])[[4]]


      # Print an histogram:
      hist(simple_driver_ses_df[, driver_nm])
      abline(v = threshold_value_low, col = "blue4")
      abline(v = threshold_value_high, col = "blue4")

      # Complete the table with high/low and remove rows in the middle:
      cat_drivers_ses_df <- simple_driver_ses_df %>%
        dplyr::mutate(driver_cat = dplyr::case_when(
          get(driver_nm) >= threshold_value_high ~ "extremely high increase",
          get(driver_nm) <= threshold_value_low ~ "extremely low increase",
          TRUE ~ "other")) %>%
        dplyr::filter(driver_cat != "other")
      # Complete the table with +/- for ses:
      cat_drivers_ses_df <- cat_drivers_ses_df %>%
        dplyr::mutate(ses_cat = dplyr::if_else(ses > 0,
                                               "positive", "negative"))

      # Only keep the columns with categories and rename them:
      final_df <- cat_drivers_ses_df[, c(1, 4, 5)]
      colnames(final_df)[2] <- "driver"
      colnames(final_df)[3] <- "ses"

    }

    # If the driver is growth rate - use 0 as a threshold:
    if (driver_nm == "Pr_RatePop_2020_mean") {

      # Define the thwo quantiles:
      threshold_value_low <- quantile(simple_driver_ses_df[, driver_nm])[[2]]
      threshold_value_high <- quantile(simple_driver_ses_df[, driver_nm])[[4]]


      # Print an histogram:
      hist(simple_driver_ses_df[, driver_nm])
      abline(v = threshold_value_low, col = "blue4")
      abline(v = threshold_value_high, col = "blue4")

      # Complete the table with high/low and remove rows in the middle:
      cat_drivers_ses_df <- simple_driver_ses_df %>%
        dplyr::mutate(driver_cat = dplyr::case_when(
          get(driver_nm) >= threshold_value_high ~ "extremely high increase",
          get(driver_nm) <= threshold_value_low ~ "extremely low increase or decrease",
          TRUE ~ "other")) %>%
        dplyr::filter(driver_cat != "other")

      # Complete the table with +/- for ses:
      cat_drivers_ses_df <- cat_drivers_ses_df %>%
        dplyr::mutate(ses_cat = dplyr::if_else(ses > 0,
                                               "positive", "negative"))

      # Only keep the columns with categories and rename them:
      final_df <- cat_drivers_ses_df[, c(1, 4, 5)]
      colnames(final_df)[2] <- "driver"
      colnames(final_df)[3] <- "ses"

    }

  } # end - if threshold type == extremes


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
  print("Chi2 test No Yate's corr as big sample size - pvalue < 0.05 significant association")
  test <- chisq.test(count_table, correct = FALSE)
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
  print(mosaic_plot)

  # Save it:
  ggplot2::ggsave(plot = mosaic_plot,
                  filename = here::here("outputs",
                                        "directionality",
                                        paste0("mosaic_plot", sep = "_",
                                        driver_nm, sep = "_",
                                        threshold_type, sep = "_",
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
    ggplot2::scale_colour_gradient(low = color_nms[2], high = color_nms[1]) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.ticks.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_blank()) +
    ggplot2::coord_cartesian(ylim = c(-0.4, 0.4),
                             xlim = c(-max(abs(simple_driver_plot_df$ses)),
                             max(abs(simple_driver_plot_df$ses)))) +
    ggplot2::labs(color = drivers_nm_df$Drivers_short_nm[which(drivers_nm_df$Drivers_nm == driver_nm)])

  print(continuous_plot)

  ggplot2::ggsave(plot = continuous_plot,
                  filename = here::here("outputs",
                                        "directionality",
                                        paste0("continuous_plot", sep = "_",
                                               driver_nm, sep = "_",
                                               threshold_type, sep = "_",
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
