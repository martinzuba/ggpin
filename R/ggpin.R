

# new geom

mz_colourpin <- function(colour = "darkred", opacity = 50) {

  needle <- image_read("src/pin-needle.png")
  ball <- image_read("src/pin-ball.png")

  col_ball <- image_colorize(ball, opacity = opacity, color = colour)

  col_pin <- image_append(image = c(col_ball, needle),
                          stack = TRUE)

  return(col_pin)
}


GeomPin <- ggproto("GeomPin", Geom,
                   required_aes = c("x", "y"),
                   default_aes = c(colour = "darkred", width = 0.02),
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



draw_key_pin <- function(data, params, size) {
   grid::rasterGrob(image = mz_colourpin(data$colour),
                    x = 0.5, y = 0.5)

}

#' Map pins
#'
#' @param mapping
#' @param data
#' @param stat
#' @param position
#' @param na.rm
#' @param show.legend
#' @param inherit.aes
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
geom_pin <- function(mapping = NULL, data = NULL, stat = "identity",
                     position = "identity", na.rm = FALSE, show.legend = NA,
                     inherit.aes = TRUE, ...) {
  layer(
    geom = GeomPin, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


