################################################################################
##
## A function to impute traits based on random forest and try its
## ... performance
##
## Based on Nicolas Loiseau's script - Camille Magneville
##
## 18/04/2024
##
## Impute_traits_fcts.R
##
################################################################################



test.mf <- function(raw_sp_tr){


  # Deleting NA in data for test of missForest:
  sp_tr_noNA <- raw_sp_tr %>%
    na.omit()

  # Get unique length of all the factor traits:
  factor_length <- data.frame()

  for (i in 1:length(raw_sp_tr[, ])) {

    # If the trait is your factor the get the length
    if (is.factor(raw_sp_tr[, i]) == TRUE) {
      data_frame <- data.frame(id = colnames(raw_sp_tr[i]),
                              length = length(unique(raw_sp_tr[, i])))
      factor_length <- rbind(factor_length, data_frame)
    }

  }


  #Imputing 20% of NA in complete data:
  sp_tr_20_NA <- sp_tr_noNA %>%
                missForest::prodNA(0.2)
  sp_tr_20_NA <- as.data.frame(sp_tr_20_NA)
  sp_tr_mf <- missForest::missForest(sp_tr_20_NA,
                                     verbose = T,
                                     variablewise = T)

  # Get dataframe with all predictions:
  preds <- sp_tr_mf$ximp

  # Merge with complete data:
  # So we have column order: 1- data with 20%NA
  # ... 2- data predicted by miss forest with 20%NA
  # ... 3- raw data with known values:
  data_imp <- sp_tr_20_NA %>%
    merge(preds, by = "row.names") %>%
    tibble::column_to_rownames("Row.names") %>%
    merge(sp_tr_noNA, by = "row.names")


  # Create a list that will contain one df for each tr,
  # ... containing only NAs - and fill it:
  all <- list()

  # For each of the traits:
  for (i in colnames(sp_tr_noNA)){

    # Keep only rows with NA in it:
    temp <- data_imp %>%
      dplyr::filter(is.na(data_imp[, paste(i, ".x", sep="")]))

    # List with one dataframes for each traits containing only NAs
    all[[i]] <- temp

  }

  # Create an empty list where to put missForest performance:
  test_results <- list()

  # Looping through all our traits:
  for (p in colnames(sp_tr_noNA)) {

    # IF TRAIT IS NUMERIC THEN LINEAR REGRESSION BETWEEN known and
    # ... predicted by missforests to get performance:
    if (is.numeric(sp_tr_noNA[, p])) {

      lm_test <- lm(unlist(all[[p]][paste(p,".y", sep = "")]) ~ unlist(all[[p]][p]),
                                                                all[[p]])

      # Save adjusted R squared to the results list:
      test_results[[p]] <- summary(lm_test)$adj.r.squared

      # IF TRAIT IS CATEGORICAL THEN GET MEAN OF THE SAME OBS BETWEEN known and
      # ... predicted by missforest to get performance:
    } else {

      m_test <- mean(unlist(all[[p]][paste(p, ".y", sep = "")]) == unlist(all[[p]][p]))

      # Save mean to the results list:
      test_results[[p]] = m_test
    }

  }

  # Get the number of values missing per trait:
  nb_missing <- data.frame(nb_na = sapply(raw_sp_tr, function(y) sum(length(which(is.na(y))))))

  # Create a new df to return with results from the performance test:
  tests_results_final <- data.frame(Rsquare = do.call(rbind, test_results))
  tests_results_final <-  merge(tests_results_final, nb_missing, by="row.names",
                                all.x = T)
  rownames(tests_results_final) <- tests_results_final[, 1]

  tests_results_final <- tests_results_final[, -1]

  return(tests_results_final)
  print(tests_results_final)

}
