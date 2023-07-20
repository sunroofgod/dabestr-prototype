#' Helper functions that deal with assignment of plot_kwargs for plot
#' 
#' @description
#' Contains function `assign_plot_kwargs`.

#' Function that handles plot_kwargs for final plotting
assign_plot_kwargs <- function(dabest_effectsize_obj, plot_kwargs) {
  swarm_label <- dabest_effectsize_obj$raw_y_labels
  contrast_label <- dabest_effectsize_obj$delta_y_labels
  custom_palette <- "d3"
  swarm_ylim <- NULL
  contrast_ylim <- NULL
  delta2_ylim <- NULL
  delta2_label <- NULL
  show_delta2 <- dabest_effectsize_obj$delta2
  show_mini_meta <- dabest_effectsize_obj$minimeta
  
  if(isFALSE(is.null(plot_kwargs$swarm_label))) {
    swarm_label <- plot_kwargs$swarm_label
  }
  if(isFALSE(is.null(plot_kwargs$contrast_label))) {
    contrast_label <- plot_kwargs$contrast_label
  }
  if(isFALSE(is.null(plot_kwargs$custom_palette))) {
    custom_palette <- plot_kwargs$custom_palette
  }
  if(isFALSE(is.null(plot_kwargs$swarm_ylim))) {
    swarm_ylim <- plot_kwargs$swarm_ylim
  }
  if(isFALSE(is.null(plot_kwargs$contrast_ylim))) {
    contrast_ylim <- plot_kwargs$contrast_ylim
  }
  if(isFALSE(is.null(plot_kwargs$delta2_ylim))) {
    delta2_ylim <- plot_kwargs$delta2_ylim
  }
  if(isFALSE(is.null(plot_kwargs$delta2_label))) {
    delta2_label <- plot_kwargs$delta2_label
  }
  if(isFALSE(is.null(plot_kwargs$show_delta2))) {
    show_delta2 <- plot_kwargs$show_delta2
  }
  if(isFALSE(is.null(plot_kwargs$show_mini_meta))) {
    show_mini_meta <- plot_kwargs$show_mini_meta
  }
  return(list(
    swarm_label = swarm_label,
    contrast_label = contrast_label,
    custom_palette = custom_palette,
    swarm_ylim = swarm_ylim,
    contrast_ylim = contrast_ylim,
    delta2_ylim = delta2_ylim,
    delta2_label = delta2_label,
    show_delta2 = show_delta2,
    show_mini_meta = show_mini_meta
  ))
}