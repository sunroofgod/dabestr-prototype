effsize_boot <- function(
    data, 
    effect_size_func, 
    reps = 5000, 
    paired = FALSE
){
  
  s <- c(rep(1, length(data$control)),
         rep(2, length(data$test)))
  
  bootboot <- function(d, indices, paired) {
    c <- d[indices[s == 1]]
    t <- d[indices[s == 2]]
    
    return(effect_size_func(c, t, paired))
  }
  
  b <- boot(
    c(data$control, data$test),
    statistic = bootboot,
    R = reps,
    strata = s,
    paired = paired
  )
  
  return(b)
}

bootstrap <- function(
    dabest_obj,
    effect_size_func,
    seed = 12345,
    reps = 5000,
    boot_labs
){
  
  boot_result <- tibble()
  
  raw_data <- dabest_obj$raw_data
  idx <- dabest_obj$idx
  if (isFALSE(is.list(idx))) {
    idx <- list(idx)
  }
  enquo_x <- dabest_obj$enquo_x
  enquo_y <- dabest_obj$enquo_y
  ci <- dabest_obj$ci
  is_paired <- dabest_obj$is_paired
  paired <- dabest_obj$paired
  is_colour <- dabest_obj$is_colour
  
  quoname_x <- as_name(enquo_x)
  quoname_y <- as_name(enquo_y)
  delta_x_labels <- list()
  delta_y_labels <- boot_labs
  
  if (isFALSE(is_paired) || isTRUE(paired == "baseline")) {
    for (group in idx) {
      group_length <- length(group)
      
      ctrl_tibble <- raw_data %>% 
        filter(!!enquo_x == !!group[1])
      ctrl_measurement <- ctrl_tibble[[quoname_y]]
      
      tests <- group[2:group_length]
      
      for (test_group in tests) {
        test_tibble <- raw_data %>%
          filter(!!enquo_x == !!test_group)
        
        test_measurement <- test_tibble[[quoname_y]]
        
        xlabels <- paste(test_group, group[1], sep="\nminus\n")
        delta_x_labels <- append(delta_x_labels, xlabels)
        
        control_test_measurement <- list(control = ctrl_measurement,
                                         test = test_measurement)
        
        set.seed(seed)
        
        boots <- effsize_boot(data = control_test_measurement,
                              effect_size_func = effect_size_func,
                              reps = reps,
                              paired = is_paired)
        
        if (ci < 0 | ci > 100) {
          err_string <- str_interp("`ci` must be between 0 and 100, not ${ci}")
          stop(err_string)
        }
        
        bootci <- boot.ci(boots, conf=ci/100, type = c("perc","bca"))
        
        boot_row <- list(
          control_group = group[1],
          test_group = test_group,
          bootstraps = list(as.vector(boots$t)),
          nboots = length(boots$t),
          bca_ci_low = bootci$bca[4],
          bca_ci_high = bootci$bca[5],
          pct_ci_low = bootci$percent[4],
          pct_ci_high = bootci$percent[5],
          ci = ci,
          difference = boots$t0
        )
        boot_result <- bind_rows(boot_result, boot_row)
      }
    }
  } else {
    for (group in idx) {
      group_length <- length(group)
      for (i in 1:(group_length-1)) {
        control_group <- group[i]
        test_group <- group[i+1]
        
        ctrl_tibble <- raw_data %>% 
          filter(!!enquo_x == !!control_group)
        ctrl_measurement <- ctrl_tibble[[quoname_y]]
        
        test_tibble <- raw_data %>% 
          filter(!!enquo_x == !!test_group)
        test_measurement <- test_tibble[[quoname_y]]
        
        xlabels <- paste(test_group, control_group, sep="\nminus\n")
        delta_x_labels <- append(delta_x_labels, xlabels)
        
        control_test_measurement <- list(control = ctrl_measurement,
                                         test = test_measurement)
        
        set.seed(seed)
        
        boots <- effsize_boot(data = control_test_measurement,
                              effect_size_func = effect_size_func,
                              reps = reps,
                              paired = is_paired)
        
        if (ci < 0 | ci > 100) {
          err_string <- str_interp("`ci` must be between 0 and 100, not ${ci}")
          stop(err_string)
        }
        
        bootci <- boot.ci(boots, conf=ci/100, type = c("perc","bca"))
        
        boot_row <- list(
          control_group = group[1],
          test_group = test_group,
          bootstraps = list(as.vector(boots$t)),
          nboots = length(boots$t),
          bca_ci_low = bootci$bca[4],
          bca_ci_high = bootci$bca[5],
          pct_ci_low = bootci$percent[4],
          pct_ci_high = bootci$percent[5],
          ci = ci,
          difference = boots$t0
        )
        boot_result <- bind_rows(boot_result, boot_row)
      }
    }
  }
  
  out <- list(raw_data = raw_data,
              idx = idx,
              delta_x_labels = delta_x_labels,
              delta_y_labels = delta_y_labels,
              raw_data = raw_data,
              is_paired = is_paired,
              is_colour = is_colour,
              paired = paired,
              Ns = dabest_obj$Ns,
              control_summary = dabest_obj$control_summary,
              test_summary = dabest_obj$test_summary,
              ylim = dabest_obj$ylim,
              enquo_x = dabest_obj$enquo_x,
              enquo_y = dabest_obj$enquo_y,
              enquo_id_col = dabest_obj$enquo_id_col,
              enquo_colour = dabest_obj$enquo_colour,
              proportional = dabest_obj$proportional,
              proportional_data = dabest_obj$proportional_data,
              boot_result = boot_result)
  
  return(out)
}