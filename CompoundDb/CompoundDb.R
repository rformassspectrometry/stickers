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
    geom_polygon(aes_(x = ~x, y = ~y), data = coords,
                 fill = fill, color = color, size = size)
}


img <- readPNG("drawings/CompoundDb-drawing.png")
img <- rasterGrob(img, width = 1.5, x = 0.5, y = 0.5,
                       interpolate = TRUE)

## Manually define...
col_blue = "#246abe"
col_grey = "#95959c"
col_grey = "#838289" # The color after Gimp converting the color scheme
col_purple = "#9200fc"
col_orange = "#f4810b"
col_yellow = "#fef14e"
col_white = "#ffffff"
col_black = "#2e3131"
col_brown <- "#321a0c"

## Define the colors for the parts
col_bg <- col_white               # the background
col_upper_half <- col_purple              # upper right.
col_upper_border <- "#f45b43"      # upper half border.
col_lower_border <- "#fdf9d3"            # lower half border.
col_text_border <- col_grey        # color for text in border
col_text <- col_grey
font_text <- "Aller_Rg"

## To use this font, copy the ttf to the font directory of the
## hexSticker package
font_text <- "Aller_Lt"
font_text <- "Aller_Rg"
## font_text <- "SpaceMono-Regular"

col_yellow <- "#fdf9d3"
col_red <- "#f45b43"
col_red_2 <- "#f79174"

hex <- ggplot() +
    geom_hexagon(size = 1.2, fill = col_bg, color = NA) +
    ## hex_segment2(size = 0, fill = col_upper_half,
    ##              from_radius = 0, to_radius = 1,
    ##              from_angle = 330, to_angle = 30) +
    ## hex_segment2(size = 0, fill = col_upper_half,
    ##              from_radius = 0, to_radius = 1,
    ##              from_angle = 30, to_angle = 90) +
    ## hex_segment2(size = 0, fill = col_upper_half,
    ##              from_radius = 0, to_radius = 1,
    ##              from_angle = 90, to_angle = 150) +

    hex_segment2(size = 0, fill = paste0(col_yellow, 10),
                 from_radius = 0, to_radius = 0.9,
                 from_angle = 330, to_angle = 30) +
    hex_segment2(size = 0, fill = paste0(col_yellow, 10),
                 from_radius = 0, to_radius = 0.9,
                 from_angle = 30, to_angle = 90) +
    hex_segment2(size = 0, fill = paste0(col_red_2, 10),
                 from_radius = 0, to_radius = 0.9,
                 from_angle = 90, to_angle = 150) +
    hex_segment2(size = 0, fill = paste0(col_red_2, 10),
                 from_radius = 0, to_radius = 0.9,
                 from_angle = 150, to_angle = 210) +
    hex_segment2(size = 0, fill = paste0(col_red, 10),
                 from_radius = 0, to_radius = 0.9,
                 from_angle = 210, to_angle = 270) +
    hex_segment2(size = 0, fill = paste0(col_red, 10),
                 from_radius = 0, to_radius = 0.9,
                 from_angle = 270, to_angle = 330) +
    ## border
    hex_segment2(size = 0, fill = paste0(col_red_2, "ff"),
                 from_radius = 0.9, to_radius = 1,
                 from_angle = 330, to_angle = 30) +
    hex_segment2(size = 0, fill = paste0(col_red_2, "ff"),
                 from_radius = 0.9, to_radius = 1,
                 from_angle = 30, to_angle = 90) +
    hex_segment2(size = 0, fill = paste0(col_red_2, "ff"),
                 from_radius = 0.9, to_radius = 1,
                 from_angle = 90, to_angle = 150) +
    hex_segment2(size = 0, fill = paste0(col_red, "ff"),
                 from_radius = 0.9, to_radius = 1,
                 from_angle = 150, to_angle = 210) +
    hex_segment2(size = 0, fill = paste0(col_red, "ff"),
                 from_radius = 0.9, to_radius = 1,
                 from_angle = 210, to_angle = 270) +
    hex_segment2(size = 0, fill = paste0(col_red, "ff"),
                 from_radius = 0.9, to_radius = 1,
                 from_angle = 270, to_angle = 330) +
    
    geom_subview(subview = img, x = 1.0, y = 0.98,
                 width = 1.05, height = 1.0) +
    ## font size for linux: 6.5, macOS
    geom_url("www.bioconductor.org", x = 1.06, y = 0.09,
             color = col_bg, size = 2, family = font_text) + 
    theme_sticker()
save_sticker(filename = "CompoundDb.png", hex)

