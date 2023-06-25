dabest_plot <- function(dabest_obj.mean_diff, float_contrast = TRUE) {
  if (isFALSE(float_contrast)) {
    cowplot::plot_grid(
      plotlist   = list(plot_raw(dabest_obj.mean_diff, float_contrast=FALSE), 
                        plot_delta(dabest_obj.mean_diff, float_contrast=FALSE)),
      nrow       = 2,
      ncol       = 1,
      axis       = "lr",
      align      = "b")
  } else {
    cowplot::plot_grid(
      plotlist   = list(plot_raw(dabest_obj.mean_diff, float_contrast=TRUE), 
                        plot_delta(dabest_obj.mean_diff, float_contrast=TRUE)),
      nrow       = 1,
      ncol       = 2,
      rel_widths = c(0.7, 0.2),
      axis       = "lr",
      align      = "h")
  }
}