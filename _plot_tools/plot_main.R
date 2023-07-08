# Raw plot function
plot_raw <- function(dabest_effectsize_obj, float_contrast, plot_kwargs) {
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
  if(isFALSE(float_contrast) && isFALSE(proportional)) {
    raw_y_min <- raw_y_min - (raw_y_max - raw_y_min)/15
  }
  raw_y_mean <- raw_y_max - raw_y_min
  
  test_summary <- dabest_effectsize_obj$test_summary
  control_summary <- dabest_effectsize_obj$control_summary
  is_paired <- dabest_effectsize_obj$is_paired
  is_colour <- dabest_effectsize_obj$is_colour
  
  idx <- dabest_effectsize_obj$idx
  raw_x_max = length(unlist(idx))
  x_axis_raw <- c(seq(1, raw_x_max,1))
  
  effsize_type <- dabest_effectsize_obj$delta_y_labels
  
  # check if multiplot
  if(length(idx) >= 2) {
    float_contrast = FALSE
  }
  
  plot_components <- create_plot_components(proportional, is_paired, float_contrast)
  main_plot_type <- plot_components$main_plot_type
  is_summary_lines <- plot_components$is_summary_lines
  is_tufte_lines <- plot_components$is_tufte_lines
  
  if(main_plot_type == "sankey"){
    sankey_bar_gap <- 0.02
    sankey_df <- create_dfs_for_sankey(float_contrast = float_contrast, 
                                       raw_data = raw_data,
                                       proportional_data = proportional_data,
                                       enquo_id_col = enquo_id_col,
                                       x_axis_raw = x_axis_raw,
                                       gap = sankey_bar_gap)
    flow1 <- sankey_df$flow1
    flow2 <- sankey_df$flow2
    rect_top <- sankey_df$rect_top
    rect_bot <- sankey_df$rect_bot
    bars <- sankey_df$bars
  }
  
  if(as_label(enquo_colour) == "NULL" && main_plot_type != "slope") {
    enquo_colour <- enquo_x
  }
  
  bar_width <- ifelse(float_contrast, 0.15, 0.10)
  
  # Initialise raw_plot and add main_plot_type component
  raw_plot <- switch(
    main_plot_type,
    
    "swarmplot" =
      ggplot() +
      # to add: need to handle colour for swarm & slope graphs
      geom_beeswarm(data = raw_data, 
                    aes(x = x_axis_raw, 
                        y = !!enquo_y, 
                        colour = !!enquo_colour),
                    cex = 2),
    
    "slope" = 
      ggplot() +
      geom_line(data = raw_data,
                aes(x = x_axis_raw, 
                    y = !!enquo_y,
                    colour = !!enquo_colour,
                    group = !!enquo_id_col,
                    alpha = 0.8)),
    
    "unpaired proportions" = 
      ggplot() +
      geom_proportionbar(data = proportional_data,
                         aes(x = x_axis_raw,
                             y = proportion_success,
                             colour = !!enquo_x, 
                             fill = !!enquo_x,
                             width = bar_width)),
    
    "sankey" =
      ggplot() +
      geom_sankeyflow(data = flow1, 
                      aes(x = x, y = y, fillcol = "#db6159")) +
      geom_sankeyflow(data = flow2, 
                      aes(x = x, y = y, fillcol = "#818181")) +
      geom_sankeyflow(data = rect_top, 
                      aes(x = x, y = y, fillcol = "#818181")) +
      geom_sankeyflow(data = rect_bot, 
                      aes(x = x, y = y, fillcol = "#db6159")) +
      geom_sankeybar(data = bars, 
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
    coord_cartesian(ylim = c(raw_y_min, raw_y_max),
                    xlim = c(0.6,raw_x_max+0.5),
                    expand = FALSE,
                    clip = "off") +
    scale_x_continuous(breaks = c(1:raw_x_max),
                       labels = Ns$swarmticklabs)
  
  # Add summary_lines component
  if(isTRUE(is_summary_lines)) {
    raw_plot <- raw_plot +
      geom_segment(colour = "black",linewidth = 0.3,
                   aes(x = 1, 
                       xend = raw_x_max+0.5,
                       y = control_summary, 
                       yend = control_summary)) +
      geom_segment(colour = "black", linewidth = 0.3,
                   aes(x = 2, 
                       xend = raw_x_max+0.5, 
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
    tufte_side_adjust_value <- ifelse(proportional, 0, 0.10)
    
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
    
    # to change: temporary fix for tufte lines black for proportional graphs
    if(isTRUE(proportional) | isTRUE(is_colour)) {
      raw_plot <- raw_plot +
        geom_segment(data = tufte_lines_df, 
                     linewidth = 0.8,
                     colour = "black",
                     aes(x = row_ref, 
                         xend = row_ref, 
                         y = y_bot_t$y, 
                         yend = y_bot_t$yend,
                         colour = !!enquo_x),
                     lineend = "square") +
        geom_segment(data = tufte_lines_df, 
                     linewidth = 0.8,
                     colour = "black",
                     aes(x = row_ref, 
                         xend = row_ref, 
                         y = y_top_t$y, 
                         yend = y_top_t$yend,
                         colour = !!enquo_x),
                     lineend = "square")
    } else {
      raw_plot <- raw_plot +
        geom_segment(data = tufte_lines_df, linewidth = 0.8,
                     aes(x = row_ref, 
                         xend = row_ref, 
                         y = y_bot_t$y, 
                         yend = y_bot_t$yend,
                         colour = !!enquo_x),
                     lineend = "square") +
        geom_segment(data = tufte_lines_df, linewidth = 0.8,
                     aes(x = row_ref, 
                         xend = row_ref, 
                         y = y_top_t$y, 
                         yend = y_top_t$yend,
                         colour = !!enquo_x),
                     lineend = "square")
    }
  }
  
  # Remove x-axis and redraw depending on float_contrast
  if(isTRUE(float_contrast)) {
    raw_plot <- raw_plot +
      float_contrast_theme +
      geom_segment(linewidth = 0.45, 
                   color = "black",
                   aes(x = 0.6, xend = raw_x_max+0.2, y = raw_y_min, yend = raw_y_min))
    
  } else {
    # Obtain dfs for xaxis redraw
    dfs_for_xaxis_redraw <- create_dfs_for_xaxis_redraw(idx)
    df_for_line <- dfs_for_xaxis_redraw$df_for_line
    df_for_ticks <- dfs_for_xaxis_redraw$df_for_ticks
    
    raw_plot <- raw_plot +
      non_float_contrast_theme +
      # Redraw xaxis line
      geom_segment(data = df_for_line,
                   linewidth = 0.5,
                   lineend = "square", 
                   color = "black",
                   aes(x = x, 
                       xend = xend, 
                       y = raw_y_min + raw_y_mean/20, 
                       yend = raw_y_min + raw_y_mean/20))  +
      # Redraw xaxis ticks
      geom_segment(data = df_for_ticks,
                   linewidth = 0.5,
                   lineend = "square", 
                   color = "black",
                   aes(x = x, 
                       xend = x, 
                       y = raw_y_min + raw_y_mean/20, 
                       yend = raw_y_min))
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
plot_delta <- function(dabest_effectsize_obj, float_contrast, plot_kwargs) {
  idx = dabest_effectsize_obj$idx
  bootstraps = dabest_effectsize_obj$bootstraps
  delta_x_labels = unlist(dabest_effectsize_obj$delta_x_labels)
  delta_y_labels = dabest_effectsize_obj$delta_y_labels
  delta_x_max = length(unlist(idx))
  float_contrast = float_contrast
  raw_y_range_vector <- dabest_effectsize_obj$ylim
  control_summary <- dabest_effectsize_obj$control_summary
  test_summary <- dabest_effectsize_obj$test_summary
  
  is_colour <- dabest_effectsize_obj$is_colour
  is_paired <- dabest_effectsize_obj$is_paired
  
  # Initialising ylim limits
  delta_y_max = .Machine$double.xmin
  delta_y_min = .Machine$double.xmax
  
  # Obtain boot
  boot_result <- dabest_effectsize_obj$boot_result
  boots <- boot_result$bootstraps
  
  # Extracting geom_bootci params
  ci_low = boot_result$bca_ci_low
  ci_high = boot_result$bca_ci_high
  difference = boot_result$difference
  
  # obtain x_axis breaks for bootci and scale_x_continuous
  x_axis_breaks <- c()
  
  delta_plot <- ggplot()
  df_for_violin <- data.frame(
    x = NA,
    y = NA,
    tag = NA
  )
  
  # check if multiplot
  if(length(unlist(idx)) >= 2) {
    float_contrast <- FALSE
  }
  
  curr_boot_idx = 1
  curr_x_idx = 0
  for (group in idx) {
    curr_x_idx <- curr_x_idx + 1
    temp_df_violin <- data.frame(x = NA,
                                 y = NA,
                                 tag = toString(curr_x_idx))
    df_for_violin <- rbind(df_for_violin, temp_df_violin)
    
    for (i in 2:length(group)) {
      curr_x_idx <- curr_x_idx + 1
      x_axis_breaks <- append(x_axis_breaks, curr_x_idx)
      
      ci_coords <- density(boots[[curr_boot_idx]])
      
      x_coords_ci <- ci_coords$x
      y_coords_ci <- ci_coords$y
      
      # Standardise y
      y_coords_ci <- (y_coords_ci - min(y_coords_ci))/(max(y_coords_ci) - min(y_coords_ci))
      y_coords_ci <- y_coords_ci/6
      
      if (isFALSE(float_contrast)) {
        y_coords_ci <- y_coords_ci/1.5
      }
      
      y_coords_ci <- y_coords_ci + curr_x_idx
      
      min_x_coords <- min(x_coords_ci)
      max_x_coords <- max(x_coords_ci)
      
      # Keeping track of ylim limits
      if (min_x_coords < delta_y_min) {
        delta_y_min <- min_x_coords
      }
      if (max_x_coords > delta_y_max) {
        delta_y_max <- max_x_coords
      }
      
      temp_df_violin <- data.frame(x = x_coords_ci,
                                   y = y_coords_ci,
                                   tag = rep(toString(curr_x_idx),512))
      
      df_for_violin <- rbind(df_for_violin, temp_df_violin)
      
      curr_boot_idx <- curr_boot_idx + 1
    }
  }
  df_for_violin <- df_for_violin %>%
    arrange(tag, x , y)
  
  if (isTRUE(is_paired) || isTRUE(is_colour)) {
    delta_plot <- delta_plot +
      geom_halfviolin(na.rm = TRUE, 
                      data = df_for_violin,
                      aes(x = y, y = x, group = tag))
  } else {
    delta_plot <- delta_plot +
      geom_halfviolin(na.rm = TRUE, 
                      data = df_for_violin,
                      aes(x = y, y = x, fill = tag))
  }
  
  # return(delta_plot)
  
  delta_plot <- delta_plot +
    geom_bootci(
      aes(x = x_axis_breaks,
          ymin = ci_low,
          ymax = ci_high,
          middle = difference))
  
  delta_y_mean <- (delta_y_max - delta_y_min)/2
  
  if (isTRUE(float_contrast)) {
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
                   aes(x = 1.8, 
                       xend = delta_x_max+0.25, 
                       y = difference, 
                       yend = difference)) +
      geom_segment(colour = "black", 
                   linewidth = 0.3, 
                   aes(x = 1.8, 
                       xend = delta_x_max+0.25, 
                       y = 0, 
                       yend = 0)) +
      
      # Extend x-axis & add labels
      scale_x_continuous(limits = c(1.8,delta_x_max+0.25),
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
    dfs_for_xaxis_redraw <- create_dfs_for_xaxis_redraw(idx)
    df_for_line <- dfs_for_xaxis_redraw$df_for_line
    df_for_ticks <- dfs_for_xaxis_redraw$df_for_ticks
    
    delta_plot <- delta_plot +
      theme_classic() +
      scale_x_continuous(limits = c(0.6,delta_x_max+0.5),
                         expand = c(0,0),
                         breaks = x_axis_breaks,
                         labels = delta_x_labels) +
      non_float_contrast_theme +
      
      # Drawing of the 0 line
      geom_segment(colour = "black", 
                   linewidth = 0.3, 
                   aes(x = 0.6, 
                       xend = delta_x_max+0.5, 
                       y = 0, 
                       yend = 0)) +
      scale_y_continuous(limits = c(delta_y_min - delta_y_mean/2, 
                                    delta_y_max + delta_y_mean/2),
                         expand = c(0,0)) +
      
      # Redraw xaxis line
      geom_segment(data = df_for_line,
                   linewidth = 0.5,
                   lineend = "square", 
                   color = "black",
                   aes(x = x, 
                       xend = xend, 
                       y = delta_y_min - delta_y_mean/3.5, 
                       yend = delta_y_min - delta_y_mean/3.5))  +
      # Redraw xaxis ticks
      geom_segment(data = df_for_ticks,
                   linewidth = 0.5,
                   lineend = "square", 
                   color = "black",
                   aes(x = x, 
                       xend = x, 
                       y = delta_y_min - delta_y_mean/3.5, 
                       yend = delta_y_min - delta_y_mean/2))
  }
  
  delta_plot <- delta_plot +
    labs(y = delta_y_labels)
  
  return(delta_plot)
}