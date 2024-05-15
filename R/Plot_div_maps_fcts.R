################################################################################
##
## Fcts to plot taxonomic, functional and phylogenetic diversities maps
##
## Camille Magneville
##
## 03/10/2023
##
## Plot_div_maps_fcts.R
##
################################################################################





#' Plot A Given Diversity Metric At A Given Scale
#'
#' @param div_per_cell_df a data frame with the spatial cells in rows and the
#' following columns: \strong{Idgrid} with the identity of each cell,
#' \strong{metric} name of the metric for column name (ex: \code{sp_richness}),
#' \strong{Grid} referring to the scale studied (only one value here),
#' \strong{Taxon} with Taxon's name (should be the same for all df as
#' one taxon is studied here). Columns should be \bold{in this order}.
#'
#' @param div_facet_nm a character string referring to the name of the diversity
#' facets studied: Can be either \code{TD}, \code{FD} or \code{PD}.
#'
#' @param metric_nm a character string referring to the name of the diversity
#' metric studied.
#'
#' @param grid a \code{sf} object referring to the spatial grid used, either
#' the 10x10km grid (WOODIV one) or the 50x50km one (INTEGRADIV one).
#'
#' @param continuous if continuous equals \code{TRUE}, diversity values are
#' plotted along a continuous scale. Otherwise they are plotted along a
#' discrete scale defined by 1/10 of values.
#'
#' @param plot_title a TRUE/FALSE value referring to plotting or not the graph
#' title (on the diversity facet studied)
#'
#' @param save a TRUE/FALSE value referring to saving or not the graph
#'
#' @return a data frame with the values of taxonomic diversity (here species
#' richness) per cell of the chosen grid.
#'
#' @export
#'
#' @examples
#'


div.maps.plot <- function(div_per_cell_df,
                          div_facet_nm,
                          metric_nm,
                          grid,
                          continuous,
                          col_pal,
                          plot_title,
                          save) {


  # Rename the column referring to the diversity metric:
  colnames(div_per_cell_df)[2] <- "Div_metric"


  # Make sure that same format for the Idgrid column in div and grid objects:
  div_per_cell_df <- dplyr::mutate(div_per_cell_df,
                                   Idgrid = as.character(Idgrid))
  grid <- dplyr::mutate(grid, Idgrid = as.character(Idgrid))

  # Link the div_per_cell_df and the grid so can have spatial info:
  spatial_div_per_cell_df <- dplyr::left_join(grid, div_per_cell_df,
                                              by = "Idgrid")

  # Some rows have Div_metric = NA, remove them:
  spatial_div_per_cell_df <- dplyr::filter(spatial_div_per_cell_df,
                                           ! is.na(Div_metric))

  # if maps to be plotted as continuous:
  if (continuous == TRUE) {

    # Set the colour scale limit to go from negative max to positive max:
    limit <- max(abs(spatial_div_per_cell_df$Div_metric)) * c(-1, 1)

    # if plot title is TRUE:
    if (plot_title == TRUE) {

          div_map <- ggplot2::ggplot(data = spatial_div_per_cell_df) +

            ggplot2::geom_sf(ggplot2::aes(fill = Div_metric)) +

            ggplot2::scale_fill_distiller(type = "div", limit = limit) +

            ggplot2::theme(legend.position = "bottom",
                           legend.title = ggplot2::element_text(size = 12),
                           axis.text = ggplot2::element_text(size = 12),
                           axis.title = ggplot2::element_text(size = 12),
                           panel.grid.major = ggplot2::element_line(colour = "lightgrey"),
                           panel.background = ggplot2::element_rect(fill = NA,
                                                                    colour = "black"))  +

            ggplot2::labs(x = "Longitude (EPSG 3035)", y = "Latitude (EPSG 3035)",
                          fill= metric_nm) +

            ggplot2::ggtitle(paste0(div_facet_nm, sep = " - ",
                                    unique(div_per_cell_df$Taxon)))

    }

    # if no title to plot:
    if (plot_title == FALSE) {

        div_map <- ggplot2::ggplot(data = spatial_div_per_cell_df) +

          ggplot2::geom_sf(ggplot2::aes(fill = Div_metric)) +

          harrypotter::scale_fill_hp(option = col_pal, name = metric_nm,
                                     limits = c(0, 1))  +

          ggplot2::theme(legend.position = "bottom",
                         legend.title = ggplot2::element_text(size = 12),
                         axis.text = ggplot2::element_text(size = 12),
                         axis.title = ggplot2::element_text(size = 12),
                         panel.grid.major = ggplot2::element_line(colour = "lightgrey"),
                         panel.background = ggplot2::element_rect(fill = NA,
                                                                  colour = "black"))  +

          ggplot2::labs(x = "Longitude (EPSG 3035)", y = "Latitude (EPSG 3035)",
                        fill= metric_nm)


    } # end if plot title = FALSE

  } # end if continuous = TRUE:


  if (continuous == FALSE) {

    # Retrieve the steps values used to define color palette:
    # compute min and max values:
    min_value <- min(spatial_div_per_cell_df$Div_metric)
    max_value <- max(spatial_div_per_cell_df$Div_metric)

    # compute 10 classes:
    interval_span <- (max_value - min_value)/10
    breaks <- c(round(min_value, 2), round(min_value + interval_span, 2),
                round(min_value + interval_span*2, 2),
                round(min_value + interval_span*3, 2),
                round(min_value + interval_span*4, 2),
                round(min_value + interval_span*5, 2),
                round(min_value + interval_span*6, 2),
                round(min_value + interval_span*7, 2),
                round(min_value + interval_span*8, 2),
                round(min_value + interval_span*9, 2),
                round(max_value, 2))


    # if plot title is TRUE:
    if (plot_title == TRUE) {

      div_map <- ggplot2::ggplot(data = spatial_div_per_cell_df) +

        ggplot2::geom_sf(ggplot2::aes(fill = Div_metric)) +

        ggplot2::scale_fill_gradientn(colors = col_pal,
                                      breaks = breaks, name = metric_nm)  +

        ggplot2::theme(legend.position = "bottom",
                       legend.title = ggplot2::element_text(size = 12),
                       legend.text = ggplot2::element_text(angle = 90, size = 7),
                       axis.text = ggplot2::element_text(size = 12),
                       axis.title = ggplot2::element_text(size = 12),
                       panel.grid.major = ggplot2::element_line(colour = "lightgrey"),
                       panel.background = ggplot2::element_rect(fill = NA,
                                                                colour = "black"))  +

        ggplot2::labs(x = "Longitude (EPSG 3035)", y = "Latitude (EPSG 3035)",
                      fill= metric_nm) +

        ggplot2::ggtitle(paste0(div_facet_nm, sep = " - ",
                                unique(div_per_cell_df$Taxon)))

    }

    # if plot title is FALSE:
    if (plot_title == FALSE) {

      div_map <- ggplot2::ggplot(data = spatial_div_per_cell_df) +

        ggplot2::geom_sf(ggplot2::aes(fill = Div_metric)) +

        ggplot2::scale_fill_gradientn(colors = col_pal,
                                      breaks = breaks, name = metric_nm)  +

        ggplot2::theme(legend.position = "bottom",
                       legend.title = ggplot2::element_text(size = 12),
                       legend.text = ggplot2::element_text(angle = 90, size = 7),
                       axis.text = ggplot2::element_text(size = 12),
                       axis.title = ggplot2::element_text(size = 12),
                       panel.grid.major = ggplot2::element_line(colour = "lightgrey"),
                       panel.background = ggplot2::element_rect(fill = NA,
                                                                colour = "black"))  +

        ggplot2::labs(x = "Longitude (EPSG 3035)", y = "Latitude (EPSG 3035)",
                      fill= metric_nm)

    }


  } # if discrete scale


  # Print the plot:
  print(div_map)

  # Save if needed:
  if (save == TRUE) {

    ggplot2::ggsave(plot = div_map,
                    filename = here::here("outputs",
                                          paste0(unique(div_per_cell_df$Taxon),
                                                 sep = "_",
                                                 metric_nm,
                                                 sep = "_",
                                                 unique(div_per_cell_df$Grid),
                                                 sep = ".",
                                                 "pdf")),
                    device = "pdf",
                    scale = 1,
                    height = 3000,
                    width = 5000,
                    units = "px",
                    dpi = 600)

  }

}

