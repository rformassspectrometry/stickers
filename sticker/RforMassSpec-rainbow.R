library(ggplot2)
library(png)
library(grid)
library(hexSticker)

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
    geom_polygon(aes(x = coords$x, y = coords$y), data = coords,
                 fill = fill, color = color, size = size)
}

img <- readPNG("../logo/R4MassSpec-logo.png")
img <- rasterGrob(img, width = 1.2, x = 0.5, y = 0.5,
                  interpolate = TRUE)

## Rainbow color
red <- "#ff0000"
orange <- "#ffa52c"
yellow <- "#ead018"
green <- "#007e15"
blue <- "#0505f9"
purple <- "#86007d"

## Colors:
col_bg = "#ffffff"
col_text = "#2e3131"
top_right_col = paste0(red, "88")
top_right_bg = paste0(red, 10)
right_col = paste0(orange, "88")
right_bg = paste0(orange, 10)
bottom_right_col = paste0(yellow, "88")
bottom_right_bg = paste0(yellow, 10)
bottom_left_col = paste0(green, "88")
bottom_left_bg = paste0(green, 10)
left_col = paste0(blue, "88")
left_bg = paste0(blue, 10)
top_left_col = paste0(purple, "88")
top_left_bg = paste0(purple, 10)

hex <- ggplot() +
    geom_hexagon(size = 1.2, fill = col_bg, color = NA) +
    ## Top right; first area then border.
    hex_segment2(size = 0, fill = top_right_bg,
                 from_radius = 0, to_radius = 0.9,
                 from_angle = 30, to_angle = 90) +
    hex_segment2(size = 0, fill = top_right_col,
                 from_radius = 0.9, to_radius = 1,
                 from_angle = 30, to_angle = 90) +
    ## Right
    hex_segment2(size = 0, fill = right_bg,
                 from_radius = 0, to_radius = 0.9,
                 from_angle = 330, to_angle = 30) +
    hex_segment2(size = 0, fill = right_col,
                 from_radius = 0.9, to_radius = 1,
                 from_angle = 330, to_angle = 30) +
    ## Bottom right
    hex_segment2(size = 0, fill = bottom_right_bg,
                 from_radius = 0, to_radius = 0.9,
                 from_angle = 270, to_angle = 330) +
    hex_segment2(size = 0, fill = bottom_right_col,
                 from_radius = 0.9, to_radius = 1,
                 from_angle = 270, to_angle = 330) +
    ## Bottom left
    hex_segment2(size = 0, fill = bottom_left_bg,
                 from_radius = 0, to_radius = 0.9,
                 from_angle = 210, to_angle = 270) +
    hex_segment2(size = 0, fill = bottom_left_col,
                 from_radius = 0.9, to_radius = 1,
                 from_angle = 210, to_angle = 270) +
    ## Left
    hex_segment2(size = 0, fill = left_bg,
                 from_radius = 0, to_radius = 0.9,
                 from_angle = 150, to_angle = 210) +
    hex_segment2(size = 0, fill = left_col,
                 from_radius = 0.9, to_radius = 1,
                 from_angle = 150, to_angle = 210) +
    ## Top left    
    hex_segment2(size = 0, fill = top_left_bg,
                 from_radius = 0, to_radius = 0.9,
                 from_angle = 90, to_angle = 150) +
    hex_segment2(size = 0, fill = top_left_col,
                 from_radius = 0.9, to_radius = 1,
                 from_angle = 90, to_angle = 150) +
    ## Image
    geom_subview(subview = img, x = 1.05, y = 0.9,
                 width = 1.4, height = 1.4) +
    ## font size for linux: 6.5, macOS
    geom_url("www.RforMassSpectrometry.org", x = 0.99, y = 0.13,
             color = col_text, size = 4.7) + 
    theme_sticker()
save_sticker(filename = "RforMassSpec-rainbow.png", hex, dpi = 300)

## Second version - maybe for t-shirt.
hex <- ggplot() +
    geom_hexagon(size = 3, fill = NA, color = NA) +
    ## Top right; first area then border.
    hex_segment2(size = 0, fill = "#ffffff",
                 from_radius = 0.5, to_radius = 0.6,
                 from_angle = 30, to_angle = 90) +
    hex_segment2(size = 0, fill = top_right_col,
                 from_radius = 0.5, to_radius = 0.6,
                 from_angle = 30, to_angle = 90) +
    ## Right
    hex_segment2(size = 0, fill = "#ffffff",
                 from_radius = 0.5, to_radius = 0.6,
                 from_angle = 330, to_angle = 30) +
    hex_segment2(size = 0, fill = right_col,
                 from_radius = 0.5, to_radius = 0.6,
                 from_angle = 330, to_angle = 30) +
    ## Bottom right
    hex_segment2(size = 0, fill = "#ffffff",
                 from_radius = 0.5, to_radius = 0.6,
                 from_angle = 270, to_angle = 330) +
    hex_segment2(size = 0, fill = bottom_right_col,
                 from_radius = 0.5, to_radius = 0.6,
                 from_angle = 270, to_angle = 330) +
    ## Bottom left
    hex_segment2(size = 0, fill = "#ffffff",
                 from_radius = 0.5, to_radius = 0.6,
                 from_angle = 210, to_angle = 270) +
    hex_segment2(size = 0, fill = bottom_left_col,
                 from_radius = 0.5, to_radius = 0.6,
                 from_angle = 210, to_angle = 270) +
    ## Left
    hex_segment2(size = 0, fill = "#ffffff",
                 from_radius = 0.5, to_radius = 0.6,
                 from_angle = 150, to_angle = 210) +
    hex_segment2(size = 0, fill = left_col,
                 from_radius = 0.5, to_radius = 0.6,
                 from_angle = 150, to_angle = 210) +
    ## Top left    
    hex_segment2(size = 0, fill = "#ffffff",
                 from_radius = 0.5, to_radius = 0.6,
                 from_angle = 90, to_angle = 150) +
    hex_segment2(size = 0, fill = top_left_col,
                 from_radius = 0.5, to_radius = 0.6,
                 from_angle = 90, to_angle = 150) +
    ## Image
    geom_subview(subview = img, x = 1.05, y = 0.9,
                 width = 1.4, height = 1.4) +
    theme_sticker()
save_sticker(filename = "RforMassSpec-rainbow2.png", hex, dpi = 300)

## Second version - maybe for t-shirt.
hex <- ggplot() +
    geom_hexagon(size = 1.2, fill = "#ffffff", color = NA) +
    ## Top right; first area then border.
    hex_segment2(size = 0, fill = paste0(red, 80),
                 from_radius = 0, to_radius = 0.17,
                 from_angle = 30, to_angle = 90) +
    hex_segment2(size = 0, fill = paste0(orange, 80),
                 from_radius = 0.17, to_radius = 0.33,
                 from_angle = 30, to_angle = 90) +
    hex_segment2(size = 0, fill = paste0(yellow, 80),
                 from_radius = 0.33, to_radius = 0.5,
                 from_angle = 30, to_angle = 90) +
    hex_segment2(size = 0, fill = paste0(green, 80),
                 from_radius = 0.5, to_radius = 0.66,
                 from_angle = 30, to_angle = 90) +
    hex_segment2(size = 0, fill = paste0(blue, 80),
                 from_radius = 0.66, to_radius = 0.83,
                 from_angle = 30, to_angle = 90) +
    hex_segment2(size = 0, fill = paste0(purple, 80),
                 from_radius = 0.83, to_radius = 1,
                 from_angle = 30, to_angle = 90) +
    ## Right
    hex_segment2(size = 0, fill = paste0(red, 20),
                 from_radius = 0, to_radius = 0.17,
                 from_angle = 330, to_angle = 30) +
    hex_segment2(size = 0, fill = paste0(orange, 20),
                 from_radius = 0.17, to_radius = 0.33,
                 from_angle = 330, to_angle = 30) +
    hex_segment2(size = 0, fill = paste0(yellow, 20),
                 from_radius = 0.33, to_radius = 0.5,
                 from_angle = 330, to_angle = 30) +
    hex_segment2(size = 0, fill = paste0(green, 20),
                 from_radius = 0.5, to_radius = 0.66,
                 from_angle = 330, to_angle = 30) +
    hex_segment2(size = 0, fill = paste0(blue, 20),
                 from_radius = 0.66, to_radius = 0.83,
                 from_angle = 330, to_angle = 30) +
    hex_segment2(size = 0, fill = paste0(purple, 20),
                 from_radius = 0.83, to_radius = 1,
                 from_angle = 330, to_angle = 30) +
    ## Bottom right 270 330
    hex_segment2(size = 0, fill = paste0(red, 20),
                 from_radius = 0, to_radius = 0.17,
                 from_angle = 270, to_angle = 330) +
    hex_segment2(size = 0, fill = paste0(orange, 20),
                 from_radius = 0.17, to_radius = 0.33,
                 from_angle = 270, to_angle = 330) +
    hex_segment2(size = 0, fill = paste0(yellow, 20),
                 from_radius = 0.33, to_radius = 0.5,
                 from_angle = 270, to_angle = 330) +
    hex_segment2(size = 0, fill = paste0(green, 20),
                 from_radius = 0.5, to_radius = 0.66,
                 from_angle = 270, to_angle = 330) +
    hex_segment2(size = 0, fill = paste0(blue, 20),
                 from_radius = 0.66, to_radius = 0.83,
                 from_angle = 270, to_angle = 330) +
    hex_segment2(size = 0, fill = paste0(purple, 20),
                 from_radius = 0.83, to_radius = 1,
                 from_angle = 270, to_angle = 330) +
    ## Bottom left 210 270
    hex_segment2(size = 0, fill = paste0(red, 20),
                 from_radius = 0, to_radius = 0.17,
                 from_angle = 210, to_angle = 270) +
    hex_segment2(size = 0, fill = paste0(orange, 20),
                 from_radius = 0.17, to_radius = 0.33,
                 from_angle = 210, to_angle = 270) +
    hex_segment2(size = 0, fill = paste0(yellow, 20),
                 from_radius = 0.33, to_radius = 0.5,
                 from_angle = 210, to_angle = 270) +
    hex_segment2(size = 0, fill = paste0(green, 20),
                 from_radius = 0.5, to_radius = 0.66,
                 from_angle = 210, to_angle = 270) +
    hex_segment2(size = 0, fill = paste0(blue, 20),
                 from_radius = 0.66, to_radius = 0.83,
                 from_angle = 210, to_angle = 270) +
    hex_segment2(size = 0, fill = paste0(purple, 20),
                 from_radius = 0.83, to_radius = 1,
                 from_angle = 210, to_angle = 270) +
    ## Left 150 210
    hex_segment2(size = 0, fill = paste0(red, 20),
                 from_radius = 0, to_radius = 0.17,
                 from_angle = 150, to_angle = 210) +
    hex_segment2(size = 0, fill = paste0(orange, 20),
                 from_radius = 0.17, to_radius = 0.33,
                 from_angle = 150, to_angle = 210) +
    hex_segment2(size = 0, fill = paste0(yellow, 20),
                 from_radius = 0.33, to_radius = 0.5,
                 from_angle = 150, to_angle = 210) +
    hex_segment2(size = 0, fill = paste0(green, 20),
                 from_radius = 0.5, to_radius = 0.66,
                 from_angle = 150, to_angle = 210) +
    hex_segment2(size = 0, fill = paste0(blue, 20),
                 from_radius = 0.66, to_radius = 0.83,
                 from_angle = 150, to_angle = 210) +
    hex_segment2(size = 0, fill = paste0(purple, 20),
                 from_radius = 0.83, to_radius = 1,
                 from_angle = 150, to_angle = 210) +
    ## Top left 90 150
    hex_segment2(size = 0, fill = paste0(red, 20),
                 from_radius = 0, to_radius = 0.17,
                 from_angle = 90, to_angle = 150) +
    hex_segment2(size = 0, fill = paste0(orange, 20),
                 from_radius = 0.17, to_radius = 0.33,
                 from_angle = 90, to_angle = 150) +
    hex_segment2(size = 0, fill = paste0(yellow, 20),
                 from_radius = 0.33, to_radius = 0.5,
                 from_angle = 90, to_angle = 150) +
    hex_segment2(size = 0, fill = paste0(green, 20),
                 from_radius = 0.5, to_radius = 0.66,
                 from_angle = 90, to_angle = 150) +
    hex_segment2(size = 0, fill = paste0(blue, 20),
                 from_radius = 0.66, to_radius = 0.83,
                 from_angle = 90, to_angle = 150) +
    hex_segment2(size = 0, fill = paste0(purple, 20),
                 from_radius = 0.83, to_radius = 1,
                 from_angle = 90, to_angle = 150) +
    ## Image
    geom_subview(subview = img, x = 1.05, y = 0.9,
                 width = 1.5, height = 1.5) +
    ## font size for linux: 6.5, macOS
    geom_url("www.RforMassSpectrometry.org", x = 0.99, y = 0.13,
             color = col_text, size = 4.7) + 
    theme_sticker()
save_sticker(filename = "RforMassSpec-rainbow3.png", hex, dpi = 300)
