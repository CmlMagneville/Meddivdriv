################################################################################
##
## Script for functions needed to compute for each land use type, a present
## ... and a past dataset of weighted mean and weighted sd
##
## Camille Magneville
##
## 24/04/2024
##
## Compute_metrics_landuse_fcts.R
##
################################################################################


#' Divide the dataset in present and past dataframes
#'
#' @param landuse_df a dataframe with the INTEGRADIV_en db columns,
#' reducted to the 50*50 scale
#'
#' @return two dataframes (one for past, one for present) with the following
#' columns \code{IdGrid}, \code{Time} and \code{Value} where Time reflects the
#' time period for which the metric has been computed and value reflect the % of
#' the cell filled by the studied land use
#'
#' @export
#'

divide.pres.past <- function(landuse_df) {


  # Create a global data frames that will be splitted into present and past:
  general_landuse_df <- as.data.frame(matrix(nrow = 1,
                                             ncol = 3,
                                             NA))
  colnames(general_landuse_df) <- c("Idgrid", "Time", "Value")


  # Group the IdGrid:
  landuse_df$Idgrid <- as.factor(landuse_df$Idgrid)


  # For each grid cell - get a subset df and get times:
  for (grid_nm in unique(landuse_df$Idgrid)) {


    # Subset data for the studied grid cell:
    grid_cell_df <- dplyr::filter(landuse_df,
                                  Idgrid == grid_nm)

    # Make sure that value is a numeric column:
    grid_cell_df$Value <- as.numeric(grid_cell_df$Value)

    # Compute the sum for each year
    # (several land uses in the original raster layer have been reclassified to crops)
    grid_cell_sum_df <- grid_cell_df %>%
      dplyr::group_by(FinalVariableCode) %>%
      dplyr::summarise(sum = sum(Value))

    # Add a column with will help to get the time:
    grid_cell_sum_df <- grid_cell_sum_df %>%
      dplyr::mutate("Time" = stringr::str_sub(FinalVariableCode,
                                              start = 14,
                                              end = -11))

    # Remove unused columns and rename the columns:
    grid_cell_final_df <- grid_cell_sum_df %>%
      dplyr::select(c("sum", "Time")) %>%
      dplyr::rename(Value = "sum")

    # Add a new column with the grid cell name:
    grid_cell_final_df <- grid_cell_final_df %>%
      dplyr::mutate(Idgrid = rep(unique(grid_cell_df$Idgrid), nrow(grid_cell_final_df)))

    # Add this data frame to the global one:
    general_landuse_df <- rbind(general_landuse_df,
                                grid_cell_final_df)

  } # End loop on grid cells


  # Remove the first row (NA):
  general_landuse_df <- general_landuse_df[-1, ]


  # vector with names of years - past:
  past_dates <- c("8000BC", "7000BC", "6000BC", "5000BC", "4000BC",
                  "3000BC", "2000BC", "1000BC", "0AD", "100AD", "200AD",
                  "300AD", "400AD", "500AD", "600AD", "700AD", "800AD",
                  "900AD", "1000AD", "1100AD", "1200AD", "1300AD",
                  "1400AD", "1500AD", "1600AD", "1700AD",
                  "1710AD", "1720AD", "1730AD", "1740AD", "1750AD", "1760AD",
                  "1770AD", "1780AD", "1790AD", "1800AD", "1810AD", "1820AD",
                  "1830AD", "1840AD")

  # vector with names of years - present:
  present_dates <- c("1850AD", "1860AD", "1870AD", "1880AD", "1890AD",
                     "1900AD", "1910AD", "1920AD", "1930AD", "1940AD",
                     "1950AD",
                     "1951AD", "1952AD", "1953AD", "1954AD", "1955AD",
                     "1956AD", "1957AD", "1958AD", "1959AD", "1960AD",
                     "1961AD", "1962AD", "1963AD", "1964AD", "1965AD",
                     "1966AD", "1967AD", "1968AD", "1969AD", "1970AD",
                     "1971AD", "1972AD", "1973AD", "1974AD", "1975AD",
                     "1976AD", "1977AD", "1978AD", "1979AD", "1980AD",
                     "1981AD", "1982AD", "1983AD", "1984AD", "1985AD",
                     "1986AD", "1987AD", "1988AD", "1989AD", "1990AD",
                     "1991AD", "1992AD", "1993AD", "1994AD", "1995AD",
                     "1996AD", "1997AD", "1998AD", "1999AD", "2000AD",
                     "2001AD", "2002AD", "2003AD", "2004AD", "2005AD",
                     "2006AD", "2007AD", "2008AD", "2009AD", "2010AD",
                     "2011AD", "2012AD", "2013AD", "2014AD", "2015AD",
                     "2016AD", "2017AD", "2018AD", "2019AD", "2020AD",
                     "2021AD", "2022AD", "2023AD")


  # Divide into past and present:
  past_landuse_df <- dplyr::filter(general_landuse_df,
                                   Time %in% past_dates)
  present_landuse_df <- dplyr::filter(general_landuse_df,
                                   Time %in% present_dates)

  return(list("past_landuse_df" = past_landuse_df,
              "present_landuse_df" = present_landuse_df))

}






#' Compute weighted mean and sd across different resolutions
#'
#' @param landuse_df a dataframe with the INTEGRADIV_en db columns,
#' reducted to the 50*50 scale
#'
#' @return two dataframes (one for past, one for present) with the following
#' columns \code{IdGrid}, \code{Type}, \code{Variable_Code}, \code{Metric} and
#' \code{Value} were value reflect the weighted mean and weighted sd of the
#' % of the cell filled by the studied land use
#'
#' @export
#'

compute.wm.wsd <- function(landuse_df) {


  # Compute two data frames with information about years:
  precise_lu <- divide.pres.past(landuse_df = landuse_df)
  past_lu_df <- precise_lu$past_landuse_df
  present_lu_df <- precise_lu$present_landuse_df

  # Create past df according to the 3 different time resolutions:
  past_1000_df <- past_lu_df %>%
    dplyr::filter(Time %in% c("8000BC", "7000BC", "6000BC", "5000BC", "4000BC",
                              "3000BC", "2000BC", "1000BC"))
  past_100_df <- past_lu_df %>%
    dplyr::filter(Time %in% c("0AD", "100AD", "200AD",
                              "300AD", "400AD", "500AD", "600AD", "700AD", "800AD",
                              "900AD", "1000AD", "1100AD", "1200AD", "1300AD",
                              "1400AD", "1500AD", "1600AD", "1700AD"))
  past_10_df <- past_lu_df %>%
    dplyr::filter(Time %in% c("1710AD", "1720AD", "1730AD", "1740AD", "1750AD", "1760AD",
                              "1770AD", "1780AD", "1790AD", "1800AD", "1810AD", "1820AD",
                              "1830AD", "1840AD"))

  # Create present df according to the 2 different resolutions:
  present_10_df <- present_lu_df %>%
    dplyr::filter(Time %in% c("1850AD", "1860AD", "1870AD", "1880AD", "1890AD",
                              "1900AD", "1910AD", "1920AD", "1930AD", "1940AD",
                              "1950AD"))
  present_1_df <- present_lu_df %>%
    dplyr::filter(Time %in% c("1951AD", "1952AD", "1953AD", "1954AD", "1955AD",
                              "1956AD", "1957AD", "1958AD", "1959AD", "1960AD",
                              "1961AD", "1962AD", "1963AD", "1964AD", "1965AD",
                              "1966AD", "1967AD", "1968AD", "1969AD", "1970AD",
                              "1971AD", "1972AD", "1973AD", "1974AD", "1975AD",
                              "1976AD", "1977AD", "1978AD", "1979AD", "1980AD",
                              "1981AD", "1982AD", "1983AD", "1984AD", "1985AD",
                              "1986AD", "1987AD", "1988AD", "1989AD", "1990AD",
                              "1991AD", "1992AD", "1993AD", "1994AD", "1995AD",
                              "1996AD", "1997AD", "1998AD", "1999AD", "2000AD",
                              "2001AD", "2002AD", "2003AD", "2004AD", "2005AD",
                              "2006AD", "2007AD", "2008AD", "2009AD", "2010AD",
                              "2011AD", "2012AD", "2013AD", "2014AD", "2015AD",
                              "2016AD", "2017AD", "2018AD", "2019AD", "2020AD",
                              "2021AD", "2022AD", "2023AD"))

  # PAST METRICS ------

  # Compute the mean of each time period (based on resolution)
  # for each grid cell:
  past_1000_mean_df <- past_1000_df %>%
    dplyr::group_by(Idgrid) %>%
    dplyr::summarise(Mean = mean(Value))
  past_100_mean_df <- past_100_df %>%
    dplyr::group_by(Idgrid) %>%
    dplyr::summarise(Mean = mean(Value))
  past_10_mean_df <- past_10_df %>%
    dplyr::group_by(Idgrid) %>%
    dplyr::summarise(Mean = mean(Value))

  # Weight by the number of years:
  past_1000_mean_df <- past_1000_mean_df %>%
    dplyr::mutate("Weighted_mean_1000" = (Mean * 7000))
  past_100_mean_df <- past_100_mean_df %>%
    dplyr::mutate("Weighted_mean_100" = (Mean * 1700))
  past_10_mean_df <- past_10_mean_df %>%
    dplyr::mutate("Weighted_mean_10" = (Mean * 140))

  # Link the three tables of resolution to compute the actual mean:
  past_wmean_df <- past_1000_mean_df %>%
    dplyr::left_join(past_100_mean_df[, -2], by = "Idgrid") %>%
    dplyr::left_join(past_10_mean_df[, -2], by = "Idgrid")

  # Compute the final weighted mean:
  past_final_wmean_df <- past_wmean_df %>%
    dplyr::mutate("Weighted_mean" = (Weighted_mean_1000 +
                                     Weighted_mean_100 +
                                     Weighted_mean_10) / (7000 + 1700 + 140))


  # Compute the square diff between xi and mean over the given resol period:
  past_1000_sd_df <- dplyr::left_join(past_1000_df,
                                      past_1000_mean_df)


  # PRESENT METRICS ------

  # Compute the mean of each time period (based on resolution)
  # for each grid cell:
  present_10_mean_df <- present_10_df %>%
    dplyr::group_by(Idgrid) %>%
    dplyr::summarise(Mean = mean(Value))
  present_1_mean_df <- present_1_df %>%
    dplyr::group_by(Idgrid) %>%
    dplyr::summarise(Mean = mean(Value))


  # Weight by the number of years:
  present_10_mean_df <- present_10_mean_df %>%
    dplyr::mutate("Weighted_mean_10" = (Mean * 100))
  present_1_mean_df <- present_1_mean_df %>%
    dplyr::mutate("Weighted_mean_1" = (Mean * 72))


  # Link the two tables of resolution to compute the actual mean:
  present_wmean_df <- present_10_mean_df %>%
    dplyr::left_join(present_1_mean_df[, -2], by = "Idgrid")

  # Compute the final weighted mean:
  present_final_wmean_df <- present_wmean_df %>%
    dplyr::mutate("Weighted_mean" = (Weighted_mean_10 +
                                       Weighted_mean_1) / (100 + 72))









}
