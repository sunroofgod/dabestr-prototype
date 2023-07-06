dabest_plot <- function(dabest_obj.mean_diff, float_contrast = TRUE) {
  is_colour <- dabest_obj.mean_diff$is_colour
  raw_legend <- NULL
  
  if (isFALSE(float_contrast)) {
    
    raw_plot <- plot_raw(dabest_obj.mean_diff, float_contrast=FALSE) + scale_color_nejm() + scale_fill_nejm()
    delta_plot <- plot_delta(dabest_obj.mean_diff, float_contrast=FALSE) + scale_color_nejm() + scale_fill_nejm()
    
    if(isTRUE(is_colour)) {
      raw_legend <- get_legend(raw_plot + 
                                 guides(alpha = "none") +
                                 theme(legend.box.margin = margin(0, 0, 0, 0)))
    }
    
    final_plot <- cowplot::plot_grid(
      plotlist   = list(raw_plot + theme(legend.position="none"), 
                        raw_legend, 
                        delta_plot + theme(legend.position="none"),
                        NULL),
      nrow       = 2,
      ncol       = 2,
      rel_widths = c(0.9, 0.1),
      axis       = "lr",
      align      = "b")
    
    return(final_plot)
    
  } else {
    
    raw_plot <- plot_raw(dabest_obj.mean_diff, float_contrast=TRUE) + scale_color_nejm() + scale_fill_nejm()
    delta_plot <- plot_delta(dabest_obj.mean_diff, float_contrast=TRUE) + scale_color_nejm() + scale_fill_nejm()
    
    if(isTRUE(is_colour)) {
      raw_legend <- get_legend(raw_plot + 
                                 guides(color = guide_legend(nrow = 1),
                                        alpha = "none") + 
                                 theme(legend.position = "bottom"))
    }
    
    final_plot <- cowplot::plot_grid(
      plotlist   = list(raw_plot + theme(legend.position="none"), 
                        delta_plot + theme(legend.position="none")),
      nrow       = 1,
      ncol       = 2,
      rel_widths = c(0.8, 0.2),
      axis       = "lr",
      align      = "h")
    
    final_plot <- cowplot::plot_grid(final_plot, raw_legend, ncol = 1, rel_heights = c(1, 0.1))
    
    return(final_plot)
  }
  
  final_plot
}