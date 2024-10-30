### TriColoured maps and density triangles

# Library load ----

library(tidyverse)

## for assigning values in three colours
library(tricolore)

## ggtern is the ggplot package for making triangles
library(ggtern)

## Colour palettes hashtag USE BATLOW
library(scico)

# Simulate data ----
set.seed(31415)

simdata <- data.frame(
  X = runif(1000, 0, 1),
  Y = runif(1000, 0, 0.5),
  Z = runif(1000, 0.25, 0.75)
)


# Tricol Map ----

# This calculates the values for the plotting;

# Breaks can be the amount of splitting if you can categories, but setting this in Inf gives the full range of colours
# This may increase computation time
# You will have to mess around with the colour settings on this
rgb.values <- Tricolore(simdata, breaks = Inf, show_data = FALSE,
                                p1 = "X", p2 = "Y", p3 = "Z", legend = TRUE)


# If legend is T, creates a list where the second element is the legend
rgb.values$key

# If show_data is T:
Tricolore(simdata, breaks = Inf, show_data = T,
          p1 = "X", p2 = "Y", p3 = "Z", legend = TRUE)$key

# And the first element is the RBG colours, which you can then map as values for your data;
head(rgb.values$rgb)

## Create blank raster
library(terra)
library(tidyterra)

rgb.raster <- rast(nrows = 100,
                   ncols = 10)

values(rgb.raster) <- rgb.values$rgb

## Assign values

## Plot
ggplot() + 
  geom_spatraster_rgb(data = rgb.raster) + theme_void()

## I have created art.
## You can inset element with patchwork for the key or just treat it by itself:
class(rgb.values$key)

# Density triangles ----

# Okay, now for the density triangles
# You can do these as is with ggtern, which includes functionality for different types of binning
# Sum returns number of cases

# Number of bins is the number of segments along each axis, which is probably self explanatory. 
# Can be long computation times if there is a lot of data

# Here is the relevant geom call;
ggtern(data = simdata, aes(X,Y,Z), colour = "black") +
  geom_tri_tern(fun = sum, bins = 5,  show.legend = T)

# And now with all my nonsense so you can see the specifics:

ggtern(data = simdata, aes(X,Y,Z), colour = "black") +
  geom_tri_tern(fun = sum, bins = 5,  show.legend = T) + 
  scale_fill_scico(na.value = "white", direction = -1, palette = "lajolla", name = "Number of cells") +
  theme_bw() + 
  Rlab("Z") + Llab("X") + Tlab("Y") + # Right, left and top labels - check your order or copy the order here I guess
  ggtitle("") +
  theme_arrowlarge() + ggtern::theme_notitles() + # Specific ggtern theme elements
  theme(tern.axis.arrow = element_line(size = 3), tern.axis.arrow.text = element_text(size = 20))


# I wanted proportion though, so I wrote a function to calculate this:

special.function.tern <- function(x){
  return(sum(x)/nrow(simdata))
}

ggtern(data = simdata, aes(X,Y,Z), colour = "black") +
  geom_tri_tern(fun = special.function.tern, bins = 5,  show.legend = T) + 
  scale_fill_scico(na.value = "white", direction = -1, palette = "lajolla", name = "Proportion of cells") +
  theme_bw() + 
  Rlab("Z") + Llab("X") + Tlab("Y") + # Right, left and top labels - check your order or copy the order here I guess
  ggtitle("") +
  theme_arrowlarge() + ggtern::theme_notitles() + # Specific ggtern theme elements
  theme(tern.axis.arrow = element_line(size = 3), tern.axis.arrow.text = element_text(size = 20))
