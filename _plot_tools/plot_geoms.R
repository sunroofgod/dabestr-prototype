# Halfviolin Geom
draw_panel_halfviolin <- function(data, panel_scales, coord) {
  coords <- coord$transform(data, panel_scales) 
  
  violin <- polygonGrob(x = coords$x,
                        y = coords$y,
                        gp = gpar(col = coords$col,
                                  fill = coords$fill,
                                  alpha = coords$alpha))
  
}

GeomHalfViolin <- ggproto("GeomHalfViolin", Geom,
                          required_aes = c("x", "y"),
                          default_aes = aes(col = "#ff800e", 
                                            fill = "#ff800e",
                                            alpha = 0.8),
                          draw_key = draw_key_point,
                          draw_panel = draw_panel_halfviolin)

geom_halfviolin <- function(mapping = NULL, data = NULL, stat = "identity", 
                            position = "identity", show.legend = NA, 
                            na.rm = FALSE, inherit.aes = TRUE, ...) {
  layer(data = data, 
        mapping = mapping,
        stat = stat,
        geom = GeomHalfViolin,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...))
}

# Boot_CI Geom
draw_panel_boot_ci <- function(data, panel_scales, coord) {
  coords <- coord$transform(data, panel_scales) 
  
  ci_line <- segmentsGrob(x0 = coords$x,
                          x1 = coords$x,
                          y0 = coords$ymin,
                          y1 = coords$ymax,
                          gp = gpar(lwd = coords$size * .pt,
                                    lineend = coords$lineend))
  
  ci_dot <- pointsGrob(x = coords$x,
                       y = coords$middle,
                       pch = coords$shape)
  
  gTree(children = gList(ci_line, ci_dot))
  
}

GeomBootCI <- ggproto("GeomBootCI", Geom,
                      required_aes = c("x", "ymin", "ymax", "middle"),
                      default_aes = aes(size = 1.2,
                                        shape = 19, 
                                        lwd = 2,
                                        lineend = "square"),
                      draw_key = draw_key_point,
                      draw_panel = draw_panel_boot_ci)

geom_bootci <- function(mapping = NULL, data = NULL, stat = "identity", 
                        position = "identity", show.legend = NA, 
                        na.rm = FALSE, inherit.aes = TRUE, ...) {
  layer(data = data, 
        mapping = mapping,
        stat = stat,
        geom = GeomBootCI,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...))
}

# Proportion Bar Geom
draw_panel_proportion_bar <- function(data, panel_scales, coord) {
  coords <- coord$transform(data, panel_scales) 
  
  failure_bar <- rectGrob(x = coords$x,
                          y = coords$yfailure,
                          width = coords$width,
                          height = 1 - coords$proportionsuccess,
                          gp = gpar(col = coords$colour,
                                    fill = "white"))
  
  success_bar <- rectGrob(x = coords$x,
                          y = coords$ysuccess,
                          width = coords$width,
                          height = coords$proportionsuccess,
                          gp = gpar(col = coords$colour,
                                    fill = alpha(coords$fill, coords$alpha)))
  
  gTree(children = gList(failure_bar, success_bar))
  
}

GeomProportionBar <- ggproto("GeomProportionBar", Geom,
                             required_aes = c("x", "yfailure", "ysuccess", "proportionsuccess"),
                             default_aes = aes(colour = NA,
                                               width = 0.2,
                                               fill = "grey35",
                                               alpha = NA),
                             draw_key = draw_key_polygon,
                             draw_panel = draw_panel_proportion_bar)

geom_proportionbar <- function(mapping = NULL, data = NULL, stat = "identity", 
                               position = "identity", show.legend = NA, 
                               na.rm = FALSE, inherit.aes = TRUE, ...) {
  layer(data = data, 
        mapping = mapping,
        stat = stat,
        geom = GeomProportionBar,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...))
}