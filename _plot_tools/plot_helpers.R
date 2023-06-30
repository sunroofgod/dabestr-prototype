# Function for creation of df for tuftelines plot
df_for_tufte <- function(raw_data, enquo_x, enquo_y, proportional){
  tufte_lines_df <- raw_data %>%
    dplyr::group_by(!!enquo_x) %>%
    dplyr::summarize(mean = mean(!!enquo_y),
                     median = median(!!enquo_y),
                     sd = sd(!!enquo_y),
                     lower_quartile = stats::quantile(!!enquo_y)[2],
                     upper_quartile = stats::quantile(!!enquo_y)[4])
  
  if(isTRUE(proportional)){
    tufte_lines_df <- tufte_lines_df %>%
      dplyr::mutate(sd = sd/10)
  }
  tufte_lines_df <- tufte_lines_df %>%
    dplyr::mutate(lower_sd = mean - sd, upper_sd = mean + sd)
  
  return(tufte_lines_df)
}

# Raw plot function
plot_raw <- function(dabest_effectsize_obj, float_contrast) {
  enquo_x = dabest_effectsize_obj$enquo_x
  enquo_y = dabest_effectsize_obj$enquo_y
  enquo_id_col = dabest_effectsize_obj$enquo_id_col
  enquo_colour = dabest_effectsize_obj$enquo_colour
  proportional = dabest_effectsize_obj$proportional
  proportional_data = dabest_effectsize_obj$proportional_data
  
  raw_data <- dabest_effectsize_obj$raw_data
  Ns <- dabest_effectsize_obj$Ns
  raw_y_range_vector <- dabest_effectsize_obj$ylim
  
  raw_y_max <- raw_y_range_vector[2]
  raw_y_min <- raw_y_range_vector[1]
  raw_y_mean <- raw_y_max - raw_y_min
  
  test_summary <- dabest_effectsize_obj$test_summary
  control_summary <- dabest_effectsize_obj$control_summary
  is_paired <- dabest_effectsize_obj$is_paired
  
  idx <- dabest_effectsize_obj$idx
  raw_x_max = length(unlist(idx))
  x_axis_raw <- c(seq(1, raw_x_max,1))
  
  effsize_type <- dabest_effectsize_obj$delta_y_labels
  
  # to add: check if multiplot, float_contrast = FALSE automatically
  
  plot_components <- create_plot_components(proportional, is_paired, float_contrast)
  main_plot_type <- plot_components$main_plot_type
  is_summary_lines <- plot_components$summary_lines
  is_tufte_lines <- plot_components$tufte_lines
  
  sankey_bar_gap <- 0.02
  
  # Initialise raw_plot and add main_plot_type component
  raw_plot <- switch(
    main_plot_type,
    
    "swarmplot" =
      ggplot() +
      # to add: need to handle colour for swarm & slope graphs
      geom_beeswarm(data = raw_data, 
                    aes(x = x_axis_raw, 
                        y = !!enquo_y, 
                        colour = !!enquo_colour)) +
      guides(colour = "none", alpha = "none", group = "none"),
    
    "slope" = 
      ggplot() +
      geom_line(data = raw_data,
                aes(x = x_axis_raw, 
                    y = !!enquo_y,
                    colour = !!enquo_colour,
                    group = !!enquo_id_col,
                    alpha = 0.8)) +
      guides(alpha = "none", group = "none"),
    
    "unpaired proportions" = 
      ggplot() +
      geom_proportionbar(data = proportional_data,
                         aes(x = x_axis_raw,
                             y = proportion_success,
                             colour = !!enquo_x, 
                             fill = !!enquo_x,
                             width = bar_width)) +
      guides(colour = "none", fill ="none"),
    
    "sankey" =
      ggplot() +
      geom_sankeyflow(data = data_for_flow1, 
                      aes(x = x, y = y, fillcol = "#db6159")) +
      geom_sankeyflow(data = data_for_flow2, 
                      aes(x = x, y = y, fillcol = "#818181")) +
      geom_sankeyflow(data = data_for_rect_top, 
                      aes(x = x, y = y, fillcol = "#818181")) +
      geom_sankeyflow(data = data_for_rect_bot, 
                      aes(x = x, y = y, fillcol = "#db6159")) +
      geom_sankeybar(data = data_for_bars, 
                     aes(x = x_axis_raw,
                         ysuccess = y_success, 
                         yfailure = y_failure, 
                         proportionsuccess = proportion_success, 
                         width = bar_width,
                         gap = sankey_bar_gap))
  )
  
  # Add scaling to axis & theme
  raw_plot <- raw_plot +
    theme_classic() +
    scale_x_continuous(limits = c(0.6,1:raw_x_max+1),
                       expand = c(0,0),
                       breaks = c(1:raw_x_max),
                       labels = Ns$swarmticklabs) +
    scale_y_continuous(limits = c(raw_y_min, raw_y_max), expand = c(0,0))
  
  # Add summary_lines component
  if(isTRUE(is_summary_lines)) {
    raw_plot <- raw_plot +
      geom_segment(colour = "black",linewidth = 0.3,
                   aes(x = 1, 
                       xend = 3,
                       y = control_summary, 
                       yend = control_summary)) +
      geom_segment(colour = "black", linewidth = 0.3,
                   aes(x = 2, 
                       xend = 3, 
                       y = test_summary, 
                       yend = test_summary))
  }
  
  # Add tufte_lines component
  if(isTRUE(is_tufte_lines)) {
    tufte_lines_df <- df_for_tufte(raw_data, enquo_x, enquo_y, proportional)
    if(main_plot_type == "sankey"){
      tufte_gap_value <- sankey_bar_gap
    } else {
      tufte_gap_value <- ifelse(proportional, min(tufte_lines_df$mean)/20, min(tufte_lines_df$mean)/50)
      tufte_gap_value <- ifelse(float_contrast, tufte_gap_value, tufte_gap_value*2)
    }
    tufte_side_adjust_value <- ifelse(proportional, 0, 0.1)
    
    row_num <- raw_x_max
    row_ref <- c(seq(1, row_num, 1)) + tufte_side_adjust_value
    x_ref <- row_ref
    
    y_top_t <-list(y = tufte_lines_df$mean + tufte_gap_value,  
                   yend = tufte_lines_df$upper_sd)
    y_bot_t <-list(y = tufte_lines_df$mean - tufte_gap_value, 
                   yend = tufte_lines_df$lower_sd) 
    if (isTRUE(str_detect(effsize_type, "edian"))) {
      y_top_t <-list(y = tufte_lines_df$median + tufte_gap_value,  
                     yend = tufte_lines_df$upper_quartile)
      y_bot_t <-list(y = tufte_lines_df$mean - tufte_gap_value, 
                     yend = tufte_lines_df$lower_quartile) 
    }
    raw_plot <- raw_plot +
      geom_segment(data = tufte_lines_df, linewidth = 0.8,
                   aes(x = row_ref, 
                       xend = row_ref, 
                       y = y_bot_t$y, 
                       yend = y_bot_t$yend),
                   lineend = "square") +
      geom_segment(data = tufte_lines_df, linewidth = 0.8,
                   aes(x = row_ref, 
                       xend = row_ref, 
                       y = y_top_t$y, 
                       yend = y_top_t$yend),
                   lineend = "square")
  }
  
  # Remove x-axis and redraw depending on float_contrast
  if(isTRUE(float_contrast)) {
    raw_plot <- raw_plot +
      float_contrast_theme +
      geom_segment(linewidth = 0.8, color = "black", x = 0, xend = 2.5, y = raw_y_min, yend = raw_y_min)
  } else {
    raw_plot <- raw_plot +
      non_float_contrast_theme +
      
      # to add: redrawing of axis for multiplot false float_contrast
      # Redraw x-axis line
      geom_segment(linewidth = 0.5, 
                   x = 1, 
                   xend = 2, 
                   y = raw_y_min + raw_y_mean/30, 
                   yend = raw_y_min + raw_y_mean/30,
                   color = "black",
                   lineend = "square") +
      
      # Redraw ticks
      geom_segment(linewidth = 0.5,
                   x = 1, 
                   xend = 1, 
                   y = raw_y_min + raw_y_mean/30, 
                   yend = raw_y_min,
                   color = "black",
                   lineend = "square") +
      geom_segment(linewidth = 0.5,
                   x = 2, 
                   xend = 2, 
                   y = raw_y_min + raw_y_mean/30, 
                   yend = raw_y_min,
                   color = "black",
                   lineend = "square")
  }
  
  # Add y_labels component
  if(isTRUE(proportional)){
    raw_plot <- raw_plot +
      labs(y = "proportion of success")
  } else {
    raw_plot <- raw_plot +
      labs(y = "value")
  }
  
  return(raw_plot)
}

# Delta plot function
plot_delta <- function(dabest_effectsize_obj, float_contrast) {
  idx = dabest_effectsize_obj$idx
  bootstraps = dabest_effectsize_obj$bootstraps
  delta_x_labels = unlist(dabest_effectsize_obj$delta_x_labels)
  delta_y_labels = dabest_effectsize_obj$delta_y_labels
  delta_x_max = length(unlist(idx))
  float_contrast = float_contrast
  row_num = dabest_effectsize_obj$row_num
  raw_y_range_vector <- dabest_effectsize_obj$ylim
  control_summary <- dabest_effectsize_obj$control_summary
  test_summary <- dabest_effectsize_obj$test_summary
  
  # Extracting geom_bootci params
  ci_low = dabest_effectsize_obj$bca_ci_low
  ci_high = dabest_effectsize_obj$bca_ci_high
  difference = dabest_effectsize_obj$difference
  
  # Initialising ylim limits
  delta_y_max = .Machine$double.xmin
  delta_y_min = .Machine$double.xmax
  
  delta_plot <- ggplot()
  
  for(i in row_num){
    ci_coords <- density(bootstraps[[1]])
    x_coords_ci <- ci_coords$x
    y_coords_ci <- ci_coords$y
    
    # Standardise y
    y_coords_ci <- (y_coords_ci - min(y_coords_ci))/(max(y_coords_ci) - min(y_coords_ci))
    y_coords_ci <- y_coords_ci/4
    
    if(isTRUE(float_contrast)){
      y_coords_ci <- y_coords_ci*2 + i
    }else{
      y_coords_ci <- y_coords_ci + i
    }
    
    min_x_coords <- min(x_coords_ci)
    max_x_coords <- max(x_coords_ci)
    
    # Keeping track of ylim limits
    if(min_x_coords < delta_y_min){
      delta_y_min <- min_x_coords
    }
    if(max_x_coords > delta_y_max){
      delta_y_max <- max_x_coords
    }
    
    delta_plot <- delta_plot +
      geom_halfviolin(aes(x = y_coords_ci, y = x_coords_ci))
    
  }
  delta_plot <- delta_plot +
    geom_bootci(aes(x = row_num,
                    ymin = ci_low,
                    ymax = ci_high,
                    middle = difference))
  
  delta_y_mean <- (delta_y_max - delta_y_min)/2
  
  if(isTRUE(float_contrast)){
    # left-right graph
    # Calculate new ylims to align summary lines
    min_raw_y <- raw_y_range_vector[1]
    raw_y_range <- raw_y_range_vector[2] - raw_y_range_vector[1]
    min_y_coords <- difference/(1 - (test_summary - min_raw_y)/(control_summary - min_raw_y))
    delta_y_range <- raw_y_range * -min_y_coords/(control_summary - min_raw_y)
    
    delta_plot <- delta_plot +
      theme_classic() +
      
      # Draw summary lines
      geom_segment(colour = "black", 
                   linewidth = 0.3, 
                   aes(x = 1.5, 
                       xend = 3, 
                       y = difference, 
                       yend = difference)) +
      geom_segment(colour = "black", 
                   linewidth = 0.3, 
                   aes(x = 1.5, 
                       xend = 3, 
                       y = 0, 
                       yend = 0)) +
      
      # Extend x-axis & add labels
      scale_x_continuous(limits = c(1.5,3),
                         expand = c(0,0),
                         breaks = c(2),
                         labels = delta_x_labels[2]) +
      
      # Scale y-axis for alignment of summary lines & change position of y-axis to right
      scale_y_continuous(limits = c(min_y_coords, 
                                    min_y_coords + delta_y_range),
                         expand = c(0, 0),
                         position = "right") +
      float_contrast_theme +
      
      # Redraw x-axis line
      geom_hline(linewidth = 0.8,
                 yintercept = min_y_coords)
    
  } else {
    # top-down graph
    # Scale x-axis for alignment & add labels
    delta_plot <- delta_plot +
      theme_classic() +
      scale_x_continuous(limits = c(0.8,3),
                         expand = c(0,0),
                         breaks = c(1:delta_x_max),
                         labels = delta_x_labels) +
      non_float_contrast_theme +
      
      geom_segment(colour = "black", 
                   linewidth = 0.3, 
                   aes(x = 0.8, 
                       xend = 3, 
                       y = 0, 
                       yend = 0)) +
      scale_y_continuous(limits = c(delta_y_min - delta_y_mean/10, 
                                    delta_y_max + delta_y_mean/10),
                         expand = c(0,0))
    
    # settle aesthetics of the x-axis, try to make it look like the python ver
    # should be a for loop but its not going to be for now
    # for loop is for j in length(idx)
    #               for k in length(idx[[i]])
    
    delta_plot <- delta_plot +
      
      # Redraw x-axis line
      geom_segment(linewidth = 0.5, 
                   aes(x = 1, 
                       xend = 2, 
                       y = delta_y_min - delta_y_mean/15, 
                       yend = delta_y_min - delta_y_mean/15),
                   lineend = "square") +
      
      # Redraw ticks
      geom_segment(linewidth = 0.5,
                   aes(x = 1, 
                       xend = 1, 
                       y = delta_y_min - delta_y_mean/15, 
                       yend = delta_y_min - delta_y_mean/10),
                   lineend = "square") +
      geom_segment(linewidth = 0.5,
                   aes(x = 2, 
                       xend = 2, 
                       y = delta_y_min - delta_y_mean/15, 
                       yend = delta_y_min - delta_y_mean/10),
                   lineend = "square")
  }
  
  delta_plot <- delta_plot +
    labs(y = delta_y_labels)
  
  return(delta_plot)
}