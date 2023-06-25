load <- function(
    data,
    x = NULL,
    y = NULL,
    idx = NULL,
    is_paired = FALSE,
    id_col = NULL,
    ci = 95,
    colour = NULL
){
  
  # Storing plotting params as quosures
  enquo_x <- enquo(x)
  enquo_y <- enquo(y)
  enquo_id_col <- enquo(id_col)
  enquo_colour <- enquo(colour)
  
  name_x <- as_name(enquo_x)
  name_y <- as_name(enquo_y)
  
  if (!is.null(idx)){
    raw_data <- data %>%
      filter(!!enquo_x %in% idx) %>%
      
      # (to be changed later for multi-group)
      mutate(x_axis_raw = case_when(
        Group == idx[1] ~ 1,
        Group == idx[2] ~ 2,
      ))
    
    # Obtain raw_y_range_vector
    ylim <- range(raw_data[[name_y]])
    
    # Extending ylim for geom_beeswarm points being plotted
    ylim[1] <- ylim[1] - (ylim[2]-ylim[1])/10
    ylim[2] <- ylim[2] + (ylim[2]-ylim[1])/10
    
    # Creation of x-axis label
    Ns <- raw_data %>%
      dplyr::group_by(!!enquo_x) %>%
      dplyr::count()
    Ns$swarmticklabs <- do.call(paste, c(Ns[c(name_x, "n")], sep = "\nN = "))
    
    # (to be changed later for multi-group)
    buffer_Ns <- data.frame(Group = "", n = 0, swarmticklabs = "") 
    Ns <- rbind(Ns)
    
    # Calculation of summary lines
    summaries <- raw_data %>%
      group_by(!!enquo_x) %>%
      summarise(summary_stats = mean(!!enquo_y))
    
    control_summary <- summaries$summary_stats[1]
    test_summary <- summaries$summary_stats[2]
    
    dabest_object <- list(
      raw_data = raw_data,
      enquo_x = enquo_x,
      enquo_y = enquo_y,
      enquo_id_col = enquo_id_col,
      enquo_colour = enquo_colour,
      idx = idx,
      is_paired = is_paired,
      ci = ci,
      Ns = Ns,
      control_summary = control_summary,
      test_summary = test_summary,
      ylim = ylim
    )
    
    class(dabest_object) <- c("dabest")
    
    return(dabest_object)
  }
  stop()
}