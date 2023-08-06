#' Effect Size function for dabest_obj
#' 
#' @name effect_size
#' 
#' @param dabest_obj dabest_obj created by loading in dataset along with other specified parameters with the [load()] function.
#' @returns 
#' A `dabest_effectsize_obj` list with 22 elements. The following are the elements contained within:

#' * `raw_data` The dataset passed to [load()] that was cleaned and altered for plotting.
#' * `idx` The list of control-test groupings as initially passed to [load()].
#' * `delta_x_labels` labels for the x-axis of the delta plot.
#' * `delta_y_labels` labels for the y-axis of the delta plot.
#' * `Ns` list of labels for x-axis of the raw plot.
#' * `raw_y_labels` labels for the y-axis of the raw plot.
#' * `is_paired` boolean value determining if it is a paired plot.
#' * `is_colour` boolean value determining if there is a colour column for the plot.
#' * `paired` paired ("sequential" or "baseline") as initially passed to [load()].
#' * `control_summary` value for plotting of control summary lines for float_contrast = `TRUE`.
#' * `test_summary` value for plotting of control summary lines for float_contrast = `TRUE`.
#' * `ylim` vector containing the y limits for the raw plot.
#' * `enquo_x` quosure of x as initially passed to [load()].
#' * `enquo_y` quosure of y as initially passed to [load()].
#' * `enquo_id_col` quosure of id_col as initially passed to [load()].
#' * `enquo_colour` quosure of colour as initially passed to [load()].
#' * `proportional` boolean value as initially passed to [load()].
#' * `minimeta` boolean value as initially passed to [load()].
#' * `delta` boolean value as initially passed to [load()].
#' * `proportional_data` list of calculations related to the plotting of proportion plots.
#' * `boot_result` list containing values related to the calculation of the effect sizes, bootstrapping and BCa correction.
#' * `permtest_pvals` list containing values related to the calculations of permutation t tests and the corresponding p values, 
#' and p values for different types of effect sizes and different statistical tests.
#' @description
#' Calculates the effect size for each pairing of control and test group in `dabest_obj$idx`.
#' These five effect sizes `mean_diff`, `median_diff`, `cohens_d`, `hedges_g` and `cliffs_delta`
#' are used for most plot types.
#'
#' @details
#' The plot types listed under here are only able to use the following effect sizes.
#' * Proportion plots offers only `mean_diff` and `cohens_h`
#' * Mini-Meta Delta plots offers only `mean_diff`
#' 
#' The other plots are able to use all given basic effect sizes as listed in the Description.
#'
#' @examples
#' data <- data.frame(Group = c("Control1", "Control1", "Test1", "Test1"),
#'                    Measurement = c(100, 80, 50, 40)) 
#' 
#' dabest_obj <- load(data, x = Group, y = Measurement, idx = c("Control1", "Test1"))
#' dabest_obj.mean_diff <- mean_diff(dabest_obj)
#' dabest_obj.mean_diff
#' 
#' # Alternatively, you can pipe it using the `%>%` operator from the magrittr library like so
#' dabest_obj.mean_diff <- load(data, x = Group, y = Measurement, idx = c("Control1", "Test1")) %>% 
#'                           mean_diff()
#' dabest_obj.mean_diff
#' 
#' @export 
mean_diff <- function(dabest_obj) {
  
  effect_size_type <- "mean_diff"
  if (class(dabest_obj)!="dabest") {
    cli::cli_abort(c("{.field dabest_obj} must be a {.cls dabest} object."),
                   "x" = "Please supply a {.cls dabest} object.")
  }
  
  effect_size_func <- function(control, test, paired) {
    if (identical(paired, FALSE)) {
      return(mean(test) - mean(control))
    }
    return(mean(test - control))
  }
  
  is_paired <- dabest_obj$is_paired
  
  if(is_paired){
    main_results <- bootstrap(dabest_obj, effect_size_func, boot_labs = "Paired\nmean difference")
    permtest_and_pvalues <- Pvalues_statistics(dabest_obj, ef_size_fn = effect_size_func, effect_size_type = effect_size_type)
    output <- c(main_results, permtest_and_pvalues)
  }
  main_results <- bootstrap(dabest_obj, effect_size_func, boot_labs = "Mean difference")
  permtest_and_pvalues <- Pvalues_statistics(dabest_obj, ef_size_fn = effect_size_func, effect_size_type = effect_size_type)
  output <- c(main_results, permtest_and_pvalues)
  class(output) <- c("dabest_effectsize")
  
  return(output)
}

#' @rdname effect_size
#' @export
median_diff <- function(dabest_obj) {
  effect_size_type <- "median_diff"
  if (class(dabest_obj)!="dabest") {
    cli::cli_abort(c("{.field dabest_obj} must be a {.cls dabest} object."),
                   "x" = "Please supply a {.cls dabest} object.")
  }
  
  effect_size_func <- function(control, test, paired) {
    if (identical(paired, FALSE)) {
      return(stats::median(test) - stats::median(control))
    }
    return(stats::median(test - control))
  }
  
  is_paired <- dabest_obj$is_paired
  
  if(is_paired){
    main_results <- bootstrap(dabest_obj, 
                              effect_size_func, 
                              boot_labs = "Paired\nmedian difference")
    permtest_and_pvalues <- Pvalues_statistics(dabest_obj, 
                                               ef_size_fn = effect_size_func,
                                               effect_size_type = effect_size_type)
    output <- c(main_results, permtest_and_pvalues)
    
  }
  main_results <- bootstrap(dabest_obj, 
                            effect_size_func, 
                            boot_labs = "Median difference")
  permtest_and_pvalues <- Pvalues_statistics(dabest_obj, 
                                             ef_size_fn = effect_size_func,
                                             effect_size_type = effect_size_type)
  output <- c(main_results, permtest_and_pvalues)
  class(output) <- c("dabest_effectsize")
  return(output)
}


#' @rdname effect_size
#' @export 
cohens_d <- function(dabest_obj) {
  effect_size_type <- "cohens_d"
  if (class(dabest_obj)!="dabest") {
    cli::cli_abort(c("{.field dabest_obj} must be a {.cls dabest} object."),
                   "x" = "Please supply a {.cls dabest} object.")
  }
  
  effect_size_func <- function(control, test, paired) {
    return(effsize::cohen.d(test, control, paired=paired)$estimate)
  }
  
  main_results <- bootstrap(dabest_obj, 
                            effect_size_func, 
                            boot_labs = "Cohen's d")
  permtest_and_pvalues <- Pvalues_statistics(dabest_obj, 
                                             ef_size_fn = effect_size_func,
                                             effect_size_type = effect_size_type)
  output <- c(main_results, permtest_and_pvalues)
  class(output) <- c("dabest_effectsize")
  return(output)
}

#' @rdname effect_size
#' @export 
hedges_g <- function(dabest_obj) {
  effect_size_type <- "hedges_g"
  if (class(dabest_obj)!="dabest") {
    cli::cli_abort(c("{.field dabest_obj} must be a {.cls dabest} object."),
                   "x" = "Please supply a {.cls dabest} object.")
  }
  
  cohens_d_ <- function(control, test, paired) {
    return(effsize::cohen.d(test, control, paired=paired)$estimate)
  }
  
  effect_size_func <- function(control, test, paired) {
    cd <- cohens_d_(test, control, paired=paired)
    corr.factor <- -hedges_correction(test, control)
    return(cd * corr.factor)
  }
  
  main_results <- bootstrap(dabest_obj, 
                            effect_size_func, 
                            boot_labs = "Hedges' g")
  permtest_and_pvalues <- Pvalues_statistics(dabest_obj, 
                                             ef_size_fn = effect_size_func,
                                             effect_size_type = effect_size_type)
  output <- c(main_results, permtest_and_pvalues)
  class(output) <- c("dabest_effectsize")
  return(output)
}

#' @rdname effect_size
#' @export 
cliffs_delta <- function(dabest_obj) {
  effect_size_type <- "cliffs_delta"
  if (class(dabest_obj)!="dabest") {
    cli::cli_abort(c("{.field dabest_obj} must be a {.cls dabest} object."),
                   "x" = "Please supply a {.cls dabest} object.")
  }
  
  effect_size_func <- function(control, test, paired=NA) {
    return(effsize::cliff.delta(test, control)$estimate)
  }
  
  main_results <- bootstrap(dabest_obj, 
                            effect_size_func, 
                            boot_labs = "Cliffs' delta")
  permtest_and_pvalues <- Pvalues_statistics(dabest_obj, 
                                             ef_size_fn = effect_size_func,
                                             effect_size_type = effect_size_type)
  output <- c(main_results, permtest_and_pvalues)
  class(output) <- c("dabest_effectsize")
  return(output)
}

#' @rdname effect_size
#' @export 
cohens_h <- function(dabest_obj){
  effect_size_type <- "cohens_h"
  if (class(dabest_obj)!="dabest") {
    cli::cli_abort(c("{.field dabest_obj} must be a {.cls dabest} object."),
                   "x" = "Please supply a {.cls dabest} object.")
  }
  
  effect_size_func <- function(control, test, paired) {
    #remove nas and nulls later on
    prop_control <- mean(control)
    prop_test <- mean(test)
    
    # Arcsine transformation
    phi_control <- 2 * asin(sqrt(prop_control))
    phi_test <- 2 * asin(sqrt(prop_test))
    result <- phi_test - phi_control
    return(result)
  }
  
  main_results <- bootstrap(dabest_obj, 
                            effect_size_func, 
                            boot_labs = "Cohen's h")
  permtest_and_pvalues <- Pvalues_statistics(dabest_obj, 
                                             ef_size_fn = effect_size_func,
                                             effect_size_type = effect_size_type)
  output <- c(main_results, permtest_and_pvalues)
  class(output) <- c("dabest_effectsize")
  return(output)
}

hedges_correction <- function(x1, x2) {
  
  n1 <- length(x1)
  n2 <- length(x2)
  
  deg.freedom <- n1 + n2 - 2
  numer       <- gamma(deg.freedom/2)
  denom0      <- gamma((deg.freedom - 1) / 2)
  denom       <- sqrt((deg.freedom / 2)) * denom0
  
  if (is.infinite(numer) | is.infinite(denom)) {
    # Occurs when df is too large.
    # Applies Hedges and Olkin's approximation.
    df.sum <- n1 + n2
    denom <- (4 * df.sum) - 9
    out <- 1 - (3 / denom)
  } else out <- numer / denom
  
  return(out)
}