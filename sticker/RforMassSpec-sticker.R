library(ggplot2)
library(png)
library(grid)
library(hexSticker)

## Settings:
## R blue: #246abe
## R grey dark: #95959c
## R grey light: #bbbdc0
col_border <- "#246abe"
col_bg <- "#ABB7B7"      ## Edward
col_bg <- "#ffffff"
col_text <- "#bbbdc0"

#' @param x x offset of the hexagon's center
#'
#' @param y y offset of the hexagon's center
#'
#' @param radius the radius (side length) of the hexagon.
#'
#' @param from_radius from where should the segment be drawn? defaults to the center
#'
#' @param to_radius to where should the segment be drawn? defaults to the radius
#'
#' @param from_angle from which angle should we draw?
#'
#' @param to_angle to which angle should we draw?
#'
#' @param fill fill color
#'
#' @param color line color
#'
#' @param size size of the line?
hex_segment2 <- function(x = 1, y = 1, radius = 1, from_radius = 0,
                         to_radius = radius, from_angle = 30, to_angle = 90,
                         fill = NA, color = NA, size = 1.2) {
    from_angle <- from_angle * pi / 180
    to_angle <- to_angle * pi / 180
    coords <- data.frame(x = x + c(from_radius * cos(from_angle),
                                   to_radius * cos(from_angle),
                                   to_radius * cos(to_angle),
                                   from_radius * cos(to_angle)),
                         y = y + c(from_radius * sin(from_angle),
                                   to_radius * sin(from_angle),
                                   to_radius * sin(to_angle),
                                   from_radius * sin(to_angle))
                         )
    geom_polygon(aes_(x = ~x, y = ~y), data = coords,
                 fill = fill, color = color, size = size)
}

img <- readPNG("../logo/R4MassSpec-logo.png")
img <- rasterGrob(img, width = 1.0, x = 0.5, y = 0.6,
                       interpolate = TRUE)

## Manually define...
col_blue = "#246abe"
col_grey = "#95959c"
col_purple = "#9200fc"
col_orange = "#f4810b"
col_yellow = "#fef14e"
col_white = "#ffffff"

## colored beams.
hex <- ggplot() +
    geom_hexagon(size = 1.2, fill = col_bg, color = NA) +
    hex_segment2(size = 0, fill = paste0(col_blue, 60),
                 from_radius = 0, to_radius = 1,
                 from_angle = 150, to_angle = 210) +
    geom_polygon(data = data.frame(x = c(1, 1 + sqrt(3)/2, 1 + sqrt(3)/3),
                                   y = c(1, 1.5, 1.5 + 1/6)),
                 aes(x = x, y = y),
                 fill = paste0(col_yellow, 40)) +
    geom_polygon(data = data.frame(x = c(1, 1 + sqrt(3)/3, 1 + sqrt(3)/6),
                                   y = c(1, 1.5 + 1/6, 1.5 + 1/3)),
                 aes(x = x, y = y),
                 fill = paste0(col_orange, 40)) +
    geom_polygon(data = data.frame(x = c(1, 1 + sqrt(3)/6, 1),
                                   y = c(1, 1.5 + 1/3, 2)),
                 aes(x = x, y = y),
                 fill = paste0(col_purple, 40)) +
    geom_hexagon(size = 1.2, fill = NA, color = col_grey) +
    geom_subview(subview = img, x = 1.05, y = 0.749,
                 width = 1.8, height = 1.7) +
    geom_url("www.RforMassSpectrometry.org", color = col_grey) + 
    theme_sticker()
save_sticker(filename = "RforMassSpectrometry.png", hex)


## ## Custom border
## hex_border <- ggplot() +
##     geom_hexagon(size = 0, fill = col_bg, color = NA) +
##     hex_segment2(size = 0, fill = col_yellow, from_radius = 0.96, to_radius = 1,
##                  from_angle = -90, to_angle = -30) +
##     hex_segment2(size = 0, fill = col_orange, from_radius = 0.96, to_radius = 1,
##                  from_angle = -30, to_angle = 30) + 
##     hex_segment2(size = 0, fill = col_purple, from_radius = 0.96, to_radius = 1,
##                  from_angle = 30, to_angle = 90) + 
##     hex_segment2(size = 0, fill = col_grey, from_radius = 0.96, to_radius = 1,
##                  from_angle = 90, to_angle = 150) + 
##     hex_segment2(size = 0, fill = col_grey, from_radius = 0.96, to_radius = 1,
##                  from_angle = 150, to_angle = 210) + 
##     hex_segment2(size = 0, fill = col_grey, from_radius = 0.96, to_radius = 1,
##                  from_angle = 210, to_angle = 270) +
##     geom_subview(subview = img, x = 1, y = 0.8,
##                  width = 1.7, height = 1.7) +
##     geom_url("www.rformassspectrometry.org", color = col_grey) + 
##     theme_sticker()
## hex_border
## save_sticker(filename = "fancy-border.png", hex_border)

## ## Internal beams
## hex_border <- ggplot() +
##     geom_hexagon(size = 0, fill = col_bg, color = NA) +
##     hex_segment2(size = 0, fill = paste0(col_blue, 60),
##                  from_radius = 0, to_radius = 1,
##                  from_angle = 150, to_angle = 210) +
##     hex_segment2(size = 0, fill = paste0(col_yellow, 60),
##                  from_radius = 0, to_radius = 1,
##                  from_angle = 270, to_angle = 330) +
##     geom_subview(subview = img, x = 1, y = 0.8,
##                  width = 1.7, height = 1.7) +
##     geom_hexagon(size = 1.2, fill = NA, color = col_grey) +
##     geom_url("www.rformassspectrometry.org", color = col_grey) + 
##     theme_sticker()
## hex_border
## save_sticker(filename = "colored-segments.png", hex_border)

## ## cube
## hex_border <- ggplot() +
##     geom_hexagon(size = 0, fill = col_bg, color = NA) +
##     hex_segment2(size = 0, fill = paste0(col_orange, 30),
##                  from_radius = 0, to_radius = 1,
##                  from_angle = 150, to_angle = 210) +
##     hex_segment2(size = 0, fill = paste0(col_orange, 30),
##                  from_radius = 0, to_radius = 1,
##                  from_angle = 210, to_angle = 270) +
##     hex_segment2(size = 0, fill = paste0(col_purple, 30),
##                  from_radius = 0, to_radius = 1,
##                  from_angle = 270, to_angle = 330) +
##     hex_segment2(size = 0, fill = paste0(col_purple, 30),
##                  from_radius = 0, to_radius = 1,
##                  from_angle = 330, to_angle = 30) +
##     hex_segment2(size = 0, fill = paste0(col_yellow, 30),
##                  from_radius = 0, to_radius = 1,
##                  from_angle = 30, to_angle = 90) +
##     hex_segment2(size = 0, fill = paste0(col_yellow, 30),
##                  from_radius = 0, to_radius = 1,
##                  from_angle = 90, to_angle = 150) +
##     geom_subview(subview = img, x = 1, y = 0.8,
##                  width = 1.7, height = 1.7) +
##     geom_url("www.rformassspectrometry.org", color = col_grey) + 
##     theme_sticker()
## hex_border
## save_sticker(filename = "cube.png", hex_border)
