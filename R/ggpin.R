

# new geom

#' Title Create a colour pin
#'
#' @param colour the colour of the pin
#' @param opacity the opacity of the pin
#'
#' @return a pin
#'
#' @import magick
mz_colourpin <- function(colour = "darkred", opacity = 50) {

  needle <- image_read(system.file("img", "pin-needle.png", package = "ggpin"))
  ball <- image_read(system.file("img", "pin-ball.png", package = "ggpin"))
  col_ball <- image_colorize(ball, opacity = opacity, color = colour)
  col_pin <- image_append(image = c(col_ball, needle),
                          stack = TRUE)

  return(col_pin)
}


#' Title draw a pin on a map
#'
#' Calls `mz_colourpin` to create a `rasterGrob`
#'
#' @param data the data
#' @param params params
#' @param size the size of the pin
#'
#' @return
#' @import grid
draw_key_pin <- function(data, params, size) {
  grid::rasterGrob(image = mz_colourpin(data$colour),
                   x = 0.5, y = 0.5)
}


GeomPin <- ggproto("GeomPin", Geom,
                   required_aes = c("x", "y"),
                   default_aes = c(colour = "darkred", size = 1),
                   draw_key = draw_key_pin,

                   draw_panel = function(data, panel_params, coord) {
                     coords = coord$transform(data, panel_params)
                     coords = coords[order(-coords$y - coords$x),]

                     farbe <- unique(coords$colour)[1]
                     out <- grid::rasterGrob(image = mz_colourpin(farbe),
                                             x = coords[coords$colour == farbe,]$x,
                                             y = coords[coords$colour == farbe,]$y,
                                             width = coords$size / 50,
                                             just = c("centre", "bottom"))

                     if (length(unique(coords$colour)) > 1)
                       for(f in unique(coords$colour)[-1]) {
                         out <- grid::gList(out,
                                            grid::rasterGrob(image = mz_colourpin(f),
                                                             x = coords[coords$colour == f,]$x,
                                                             y = coords[coords$colour == f,]$y,
                                                             width = coords$size / 50,
                                                             just = c("centre", "bottom")))
                       }

                    return(out)

                   })


#' Title
#'
#' @param mapping Set of aesthetic mappings created by aes() or aes_(). If specified and inherit.aes = TRUE (the default), it is combined with the default mapping at the top level of the plot. You must supply mapping if there is no plot mapping.
#' @param data The data to be displayed in this layer. There are three options:
#'             If NULL, the default, the data is inherited from the plot data as specified in the call to ggplot()
#'             A data.frame, or other object, will override the plot data. All objects will be fortified to produce a data frame. See fortify() for which variables will be created.
#'             A function will be called with a single argument, the plot data. The return value must be a data.frame, and will be used as the layer data. A function can be created from a formula (e.g. ~ head(.x, 10)).
#' @param stat This should always be "identity"
#' @param position Position adjustment, either as a string, or the result of a call to a position adjustment function.
#' @param na.rm If FALSE, the default, missing values are removed with a warning. If TRUE, missing values are silently removed.
#' @param show.legend logical. Should this layer be included in the legends? NA, the default, includes if any aesthetics are mapped. FALSE never includes, and TRUE always includes. It can also be a named logical vector to finely select the aesthetics to display.
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather than combining with them. This is most useful for helper functions that define both data and aesthetics and shouldn't inherit behaviour from the default plot specification, e.g. borders().
#' @param ... Other arguments passed on to layer(). These are often aesthetics, used to set an aesthetic to a fixed value, like colour = "red" or size = 3. They may also be parameters to the paired geom/stat.
#'
#' @return
#' @export
#' @import ggplot2
geom_pin <- function(mapping = NULL, data = NULL, stat = "identity",
                     position = "identity", na.rm = FALSE, show.legend = NA,
                     inherit.aes = TRUE, ...) {

  GeomPin <- ggproto("GeomPin", Geom,
                     required_aes = c("x", "y"),
                     default_aes = c(colour = "darkred", size = 1),
                     draw_key = draw_key_pin,

                     draw_panel = function(data, panel_params, coord) {
                       coords = coord$transform(data, panel_params)
                       coords = coords[order(-coords$y - coords$x),]

                       farbe <- unique(coords$colour)[1]
                       out <- grid::rasterGrob(image = mz_colourpin(farbe),
                                               x = coords[coords$colour == farbe,]$x,
                                               y = coords[coords$colour == farbe,]$y,
                                               width = coords$width,
                                               just = c("centre", "bottom"))

                       if (length(unique(coords$colour)) > 1)
                         for(f in unique(coords$colour)[-1]) {
                           out <- grid::gList(out,
                                              grid::rasterGrob(image = mz_colourpin(f),
                                                               x = coords[coords$colour == f,]$x,
                                                               y = coords[coords$colour == f,]$y,
                                                               width = coords$width,
                                                               just = c("centre", "bottom")))
                         }

                       return(out)

                     })

  layer(
    geom = GeomPin, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


