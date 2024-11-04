################################################################################
##                                                                            ##
##                    Function: Integrity_map                                 ##
##                                                                            ##
## This function plot the map of the four moments of distribution of the      ##
## traits and the distance to the low boundary and uniform distribution of the## 
## SKR                                                                        ##  
##                                                                            ##
## Elysa SILVA - elysilvam@ua.es                                              ##
##                                                                            ##
## 28/08/2024                                                                 ##
##                                                                            ##
################################################################################


#' Title
#'
#' @param data .........DAtaframe with the Idgrid and the four moments of dist 
#'                      SKR distance. Is the output of Integrity_trait function 
#'                      in long format (Idgrid, Variable, Value)
#' @param spgrid .......Shapefile with the grid (50x50 or 10x10 km)
#' @param cellID .......Name of the column of the ID of the grid in the spgrid
#' @param title ........Taxa or group
#' @param legend_fill ..Color pallet ussually given by the function 
#'                      ggplot2::scale_fill_continuous(low = "yellow", high = "red")
#' @param multiple .....If show all maps (4 moments + SKR_lb and SKR_ud) or filter
#'                      variable to be maped(shows only one)
#'
#' @return Map or group of maps
#' 
#' 


Integrity_map <- function(data,
                          spgrid,
                          cellID,
                          title,
                          legend_fill_m,
                          legend_fill_skr,
                          multiple){
  
  # Function based plot -----------------------------------------------------
  
  
  map_fct <- function(data,
                      spgrid,
                      base_map,
                      cellID,
                      title,
                      legend_fill){
    
    sf_data <- spgrid |>
      dplyr::left_join(data, by = dplyr::join_by(!! rlang::sym(cellID) == "Idgrid"))
    
    box_m <- sf::st_bbox(spgrid)
    
    plot<- ggplot2::ggplot() +
      ggplot2::geom_sf(data= EU_base, fill = "lightgrey", color = "#636363")+
      ggplot2::geom_sf(data= sf_data, ggplot2::aes(fill = Value), 
                       color = "lightgrey") +
      legend_fill+
      ggplot2::theme(legend.position = "right")  +
      ggplot2::theme(legend.title = ggplot2::element_text(size = 12,
                                                          face = "bold")) +
      ggplot2::theme(axis.text = ggplot2::element_text(size = 10),
                     axis.title = ggplot2::element_text(size = 12,
                                                        face = "bold")) +
      ggplot2::labs(x = "Longitude", y = "Latitude", title = title,
                    fill = paste(data$Variable[1])) +
      ggplot2::theme(panel.grid.major = ggplot2::element_line(colour = "lightgrey",
                                                              linewidth = 0.1)) +
      ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white",
                                                              colour = "darkgrey"))+
      ggplot2::coord_sf(xlim = c(box_m["xmin"], box_m["xmax"]),
                        ylim = c(box_m["ymin"], box_m["ymax"])) 
    return(plot)
    
  }
  
  if (multiple == FALSE) {
    
    plot_vf <- map_fct(data = data,
                       spgrid = spgrid,
                       cellID = cellID,
                       title = title, 
                       legend_fill = legend_fill_m)
    
  }  
  
  if (multiple == TRUE) {
    
    # Mean .....................................................................
    p1 <- map_fct(data = data |> 
                    dplyr::filter(Variable == "Mean"), 
                  spgrid = spgrid, 
                  cellID = cellID, 
                  title = paste(title, "- mean"), 
                  legend_fill = legend_fill_m)
    
    # Variance ...................................................................
    p2 <- map_fct(data |> 
                    dplyr::filter(Variable == "Var"), 
                  spgrid = spgrid, 
                  cellID = cellID, 
                  title = paste(title, "- Variance"), 
                  legend_fill = legend_fill_m)
    
    # Skewness ..................................................................
    p3 <- map_fct(data |> 
                    dplyr::filter(Variable == "Skewness"), 
                  spgrid = spgrid, 
                  cellID = cellID, 
                  title = paste(title, "- Skewness"), 
                  legend_fill = legend_fill_m)
    
    # Kurtosis ..................................................................
    p4 <- map_fct(data |> 
                    dplyr::filter(Variable == "Kurtosis"), 
                  spgrid = spgrid, 
                  cellID = cellID, 
                  title = paste(title, "- Kurtosis"), 
                  legend_fill = legend_fill_m)
    
    # SKR_lb .......................................................................
    p5 <- map_fct(data |> 
                    dplyr::filter(Variable == "SKR_ud"), 
                  spgrid = spgrid, 
                  cellID = cellID, 
                  title = paste(title, "- SKR_Unifrom distribution"), 
                  legend_fill = legend_fill_skr)
    
    # SKR_ud .......................................................................
    p6 <- map_fct(data |> 
                    dplyr::filter(Variable == "SKR_ud")|>
                    dplyr::mutate(Value = abs(Value)), 
                  spgrid = spgrid, 
                  cellID = cellID, 
                  title = paste(title, "- abs(SKR_Unifrom distribution)"), 
                  legend_fill = legend_fill_skr)
    
    
    
    # Gather plots .............................................................
    
    plot_vf <- p1 + p2 + p3 + p4 + p5 + p6 +
      patchwork::plot_layout(ncol = 2, nrow = 3)
    
  } 
  
  return(plot_vf) 
  
}






# EXAMPLE of maps I send it to you :) -------------------------------------


map_plot <- Integrity_map(data = Map_data_50,
                          spgrid = spgrid_50,
                          cellID = "GRD_ID",
                          title = "Taxa_SKR_ud",
                          legend_fill_m = legend_fill_skr,
                          legend_fill_skr = legend_fill_skr,
                          multiple = FALSE)

map_plot <- map_plot+ggplot2::facet_wrap(~Taxa, ncol = 2)  