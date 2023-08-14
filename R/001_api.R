#' 
#' Loading data with dabestr 
#' 
#' @description 
#' `load()` collates the data in preparation for the computation of specified [effect_size]. 
#' Bootstrap resampling is used to compute non-parametric assumption-free confidence intervals. 
#' Visualization of the effect sizes and their confidence intervals using 
#' estimation plots is then performed with a specialized {plotting} function.
#' 
#' @param data A tidy dataframe.
#' @param x Column name in the dateset that specifies the treatment group. 
#' @param y Column name in the dataset that specifies the measurement values.
#' @param idx List of control-test groupings for which the 
#' effect size will be computed for.
#' @param paired Paired ("sequential" or "baseline").
#' @param id_col Column name indicating the identity of the
#'  datapoint if the data is paired. Must be supplied if paired is TRUE.
#' @param ci Double ranging from 0 to 100 which determines the bca_low and 
#' bca_high confidence interval calculations
#' @param colour Column name indicating the column to implement colour aesthetics 
#' in plotting functions.
#' @param proportional Boolean value determining whether the data provided is for
#' the plotting of proportional bars or sankey diagrams.
#' @param minimeta Boolean value determining if mini-meta analysis is conducted.
#' @param delta2 Boolean value determining Whether delta-delta analysis for 
#' 2 by 2 experimental designs are conducted.
#' @param experiment Experiment column name for delta2 analysis.
#' @param experiment_label String specifying the experiment label that is used to distinguish 
#' the experiment and the factors (being used in the plotting labels).
#' @param x1_level String setting the first factor level in 
#' a 2 by 2 experimental design.
#'
#' @return 
#' Returns a dabest_object list with 18 elements. The following are the elements contained within:
#'
#' - `raw_data` The tidy dataframe that was passed into [load()].
#' - `proportional_data` List of calculations related to the plotting of proportion plots.
#' - `enquo_x`  Quosure of x whose columns in the dataframe will be used to plot the x-axis.
#' - `enquo_y`  Quosure of y whose columns in the dataframe will be used to plot the y-axis.
#' - `enquo_id_col` Quosure of id_col whose columns in the dataframe will be used to 
#' distinguish subjects in repeated measures.
#' - `enquo_colour` Quosure of colour which will determine the color aesthetics of the plot.
#' - `proportional` Same as above.
#' - `minimeta`  Same as above.
#' - `delta2`  Same as above.
#' - `idx` Same as above.
#' - `is_paired` Boolean value determining if it is a paired plot.
#' - `is_colour` Boolean value determining if there is a colour column for the plot.
#' - `paired` Same as above.
#' - `ci` Numeric value which determines the confidence interval 
#' specified for the effect size and bootstrap calculations.
#' - `Ns` List of labels for x-axis of the raw plot.
#' - `control_summary` Numeric value for plotting of control summary lines for float_contrast= TRUE.
#' - `test_summary` Numeric value for plotting of test summary lines for float_contrast = TRUE.
#'  * `ylim` Vector containing the y limits for the raw plot.
#' 
#' @examples 
#' # Loading in of the dataset
#' data(twogroup_data)
#' 
#' # Creating a dabest object
#' dabest_obj <- load(data = twogroup_data, x = Group, y = Measurement, 
#' idx = c("Control1", "Group1"))
#' 
#' @export load
#'
load <- function(
    data,
    x,
    y,
    idx = NULL,
    paired = NULL,
    id_col = NULL,
    ci = 95,
    colour = NULL,
    proportional = FALSE,
    minimeta = FALSE,
    delta2 = FALSE,
    experiment = NULL,
    experiment_label = NULL,
    x1_level = NULL
    ){
  
  # Storing plotting params as quosures
  enquo_x <- rlang::enquo(x)
  enquo_y <- rlang::enquo(y)
  enquo_id_col <- rlang::enquo(id_col)
  enquo_colour <- rlang::enquo(colour)
  
  is_colour <- isFALSE(rlang::quo_is_null(enquo_colour))
  is_id_col <- isFALSE(rlang::quo_is_null(enquo_id_col))
  is_paired <- isFALSE(is.null(paired))
  
  name_x <- rlang::as_name(enquo_x)
  name_y <- rlang::as_name(enquo_y)
  
  #### Checking Validity of params ####
  if (isFALSE(name_x %in% colnames(data))) {
    cli::cli_abort(c("Column {.field x} is {.emph not} in {.field data}.", 
                     "x" = "Please enter a valid entry for {.field x} in {.fun load}."))
  }
  if (isFALSE(name_y %in% colnames(data))) {
    cli::cli_abort(c("Column {.field y} is {.strong not} in {.field data}.", 
                     "x" = "Please enter a valid entry for {.field y} in {.fun load}."))
  }
  if (isTRUE(is_id_col)) {
    if (isFALSE(rlang::as_name(enquo_id_col) %in% colnames(data))) {
      cli::cli_abort(c("Column {.field id_col} is {.strong not} in {.field data}.", 
                       "x" = "Please enter a valid entry for {.field id_col} in {.fun load}."))
    }
  }
  if (isTRUE(is_colour)) {
    if (isFALSE(rlang::as_name(enquo_colour) %in% colnames(data))) {
      cli::cli_abort(c("Column {.field colour} is {.strong not} in {.field data}.", 
                       "x" = "Please enter a valid entry for {.field colour} in {.fun load}."))
    }
  }
  if (isFALSE(delta2)) {
    if (is.null(idx)) {
      cli::cli_abort(c("Column {.field idx} is currently NULL.", 
                       "x" = "Please enter a valid entry for {.field idx} in {.fun load}."))
    }
    if (is.list(idx)) {
      general_idx_lengths <- sapply(idx,length)
      if (any(general_idx_lengths<2)==TRUE) {
        cli::cli_abort(c("Some {.field idx} does not consist of at least 2 groups",
                         "x" = "Make sure each nested group in {.field idx} has length >=2."))
      }
    } else {
      general_idx_lengths <- length(idx)
      if (any(general_idx_lengths<2)==TRUE) {
        cli::cli_abort(c("Some {.field idx} does not consist of at least 2 groups",
                         "x" = "Make sure each nested group in {.field idx} has length >=2."))
      }
    }
  }  
  
  
  ## Check that data is proportional
  if (isTRUE(proportional)) {
    values <- unique(data[[name_y]])
    if (isFALSE(setequal(c(0,1), values))) {
      cli::cli_abort(c("{.field proportional} is {.strong TRUE} but {.field data} is not proportional.", 
                       "x" = "{.field y} Column of {.field data} should only contain 1 and 0."))
    }
  }
  
  ## Check that id_col is not NULL if is_paired is TRUE
  if (isTRUE(is_paired) & isFALSE(is_id_col)) {
    cli::cli_abort(c("{.field paired} is {.strong TRUE} but no {.field id_col} was supplied.", 
                     "x" = "Please enter an entry for {.field id_col} in {.fun load}."))
  }
  
  ## Check that paired must be either "baseline" or "sequential"
  if (isTRUE(is_paired)) {
    if (isFALSE(paired %in% c("baseline","sequential"))) {
      cli::cli_abort(c("{.field paired} is not 'baseline' or 'sequential'.", 
                       "x" = "{.field paired} can only be 'baseline' or 'sequential'."))
    }
  }
  
  ## Make idx into a list if it is a vector
  if (typeof(idx) != "list" && isFALSE(is.null(idx))) {
    idx <- list(idx)
  }
  
  ## Check for valid mini-meta 
  if (isTRUE(minimeta)) {
    if (isTRUE(proportional)) {
      cli::cli_abort(c("{.field proportional} is {.strong TRUE} but {.field minimeta} is also {.strong TRUE}.", 
                       "x" = "{.field proportional} and {.field minimeta} cannot be {.strong TRUE} at the same time."))
    } else if (isTRUE(delta2)) {
      cli::cli_abort(c("{.field delta2} is {.strong TRUE} but {.field minimeta} is also {.strong TRUE}.", 
                       "x" = "{.field delta2} and {.field minimeta} cannot be {.strong TRUE} at the same time."))
    }
    
    minimeta_idx_lengths <- sapply(idx, length)
    if (any(minimeta_idx_lengths!=2)==TRUE) {
      cli::cli_abort(c("{.field minimeta} is {.strong TRUE}, but some {.field idx} does not consist of exactly 2 groups",
                       "x" = "You can only put in exactly 2 groups in {.field idx} when {.field minimeta} is {.strong TRUE}."))
    }
  }
  
  if (isTRUE(delta2)) {
    if (isTRUE(proportional)) {
      cli::cli_abort(c("{.field delta2} is {.strong TRUE} but {.field proportional} is also {.strong TRUE}.", 
                       "x" = "{.field delta2} and {.field proportional} cannot be {.strong TRUE} at the same time."))
    }
    
    enquo_experiment <- rlang::enquo(experiment)
    name_experiment <- rlang::as_name(enquo_experiment)
    
    # Make sure that data is a 2x2 ANOVA case
    if (length(unique(data[[name_experiment]]))!=2) {
      cli::cli_abort(c("{.field experiment} does not have a length of 2.", 
                       "x" = "There can only be 2 groups in {.field experiment} when {.field delta2} is {.strong TRUE}."))
    } else if (length(unique(data[[name_x]]))!=2) {
      cli::cli_abort(c("{.field x} does not have a length of 2.", 
                       "x" = "There can only be 2 groups in {.field x} when {.field delta2} is {.strong TRUE}."))
    }
    
    # Check for idx, experiment_label and x1_level
    if (isTRUE(is.null(idx))) {
      # Set levels for experiment and x if they are present
      if (isFALSE(is.null(experiment_label))) {
        data[[name_experiment]] = factor(x = data[[name_experiment]], levels = experiment_label)
      }
      if (isFALSE(is.null(x1_level))) {
        data[[name_x]] = factor(x = data[[name_x]], levels = x1_level)
      }
      data <- data %>%
        dplyr::arrange(!!enquo_experiment, !!enquo_x)
    }
    
    data <- data %>%
      dplyr::mutate(grouping = !!enquo_x) %>%
      tidyr::unite(!!enquo_experiment, c(!!enquo_x,!!enquo_experiment),sep = " ",remove=FALSE)
    if (dplyr::as_label(enquo_colour) == "NULL") {
      enquo_colour <- enquo_x
    }
    enquo_x <- enquo_experiment
    name_x <- rlang::as_name(enquo_x)
    is_colour <- TRUE
    
    # Obtain idx if is null
    if (isTRUE(is.null(idx))) {
      spread_idx <- unique(data[[name_experiment]])
      idx <- list()
      delta_group_size <- 2
      curr_group_size <- 0
      curr_group_vector <- c()
      for (group_name in spread_idx) {
        if (curr_group_size == delta_group_size) {
          curr_group_size <- 0
          idx <- c(idx, list(curr_group_vector))
          curr_group_vector <- c()
        }
        curr_group_vector <- append(curr_group_vector, group_name)
        curr_group_size <- curr_group_size + 1
      }
      idx <- c(idx, list(curr_group_vector))
    }
  }
  
  unlist_idx <- unlist(idx)
  
  raw_data <- data %>%
    dplyr::filter(!!enquo_x %in% unlist_idx) %>%
    dplyr::mutate(x_axis_raw = 0)
  
  raw_data[[name_x]] = factor(x = raw_data[[name_x]], levels = unlist_idx)
  
  for (i in 1:length(unlist_idx)) {
    raw_data <- raw_data %>%
      dplyr::mutate(x_axis_raw = ifelse(
        !!enquo_x == unlist_idx[i], i, x_axis_raw 
      ))
  }
  
  # Obtain raw_y_range_vector
  ylim <- range(raw_data[[name_y]])
  
  # Creation of x-axis label
  Ns <- raw_data %>%
    dplyr::group_by(!!enquo_x) %>%
    dplyr::count()
  Ns$swarmticklabs <- do.call(paste, c(Ns[c(name_x, "n")], sep = "\nN = "))
  
  # Extending ylim for plotting
  ylim[1] <- ylim[1] - (ylim[2]-ylim[1])/25
  ylim[2] <- ylim[2] + (ylim[2]-ylim[1])/25
  
  if(isTRUE(proportional)){
    proportional_data <- raw_data %>%
      dplyr::select(!!enquo_x, !!enquo_y, !!enquo_id_col, !!enquo_colour) %>%
      dplyr::group_by(!!enquo_x) %>%
      dplyr::summarise(proportion_success = mean(!!enquo_y),
                       y_success = proportion_success/2,
                       y_failure = (1+proportion_success)/2)
    
    control_summary <- proportional_data$proportion_success[1]
    test_summary <- proportional_data$proportion_success[2]
    
  } else {
    # Calculation of summary lines
    summaries <- raw_data %>%
      dplyr::group_by(!!enquo_x) %>%
      dplyr::summarise(summary_stats = mean(!!enquo_y))
    
    # Only currently works for two-groups, if needed for extended features in future, to be changed
    control_summary <- summaries$summary_stats[1]
    test_summary <- summaries$summary_stats[2]
    
    proportional_data <- NULL
  }
  
  dabest_object <- list(
    raw_data = raw_data,
    proportional_data = proportional_data,
    enquo_x = enquo_x,
    enquo_y = enquo_y,
    enquo_id_col = enquo_id_col,
    enquo_colour = enquo_colour,
    proportional = proportional,
    minimeta = minimeta,
    delta2 = delta2,
    idx = idx,
    is_paired = is_paired,
    is_colour = is_colour,
    paired = paired,
    ci = ci,
    Ns = Ns,
    control_summary = control_summary,
    test_summary = test_summary,
    ylim = ylim
  )
  
  class(dabest_object) <- c("dabest")

  print_dabest(dabest_object)
  return(dabest_object)
}
