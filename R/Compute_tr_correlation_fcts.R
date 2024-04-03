################################################################################
##
## Function to compute correlations between traits
##
## Camille Magneville
##
## 13/12/2023
##
## Compute_tr_correlation_fcts.R
##
################################################################################




#' Compute Cramer's V on a set of traits
#'
#' @param sp_tr_df dataframe gathering species in rows and traits in columns
#' @param var_nm a vector gathering the name of the traits to study (so only
#' categorical ones)
#'
#' @return a dataframe with Cramer's V values for eauch pair of trait
#' @export
#'
#' @examples
#'

cramerv.compute <- function(sp_tr_df, var_nm) {

  # Build an empty dataframe with the traits names in rows and columns:
  cramerv_df <- as.data.frame(matrix(ncol = length(var_nm),
                                     nrow = length(var_nm),
                                     NA))
  colnames(cramerv_df) <- var_nm
  rownames(cramerv_df) <- var_nm

  # Fill the df by computing cramer's v for each pair of traits:
  for (i in (1:ncol(cramerv_df))) {

    for (j in (1:nrow(cramerv_df))) {

      # if we are not comparing the same variables:
      if (i != j) {

        var1 <- dplyr::select(sp_tr_df,
                              colnames(cramerv_df)[i])
        var2 <- dplyr::select(sp_tr_df,
                              colnames(cramerv_df)[j])
        value <- rcompanion::cramerV(var1[[1]], var2[[1]])

        cramerv_df[i, j] <- value
      }

    }

  }

  return(cramerv_df)

}
