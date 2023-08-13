#' 
#' Prepare Data for Analysis with dabestr
#' 
#' @description 
#' Estimation statistics is a statistical framework that focuses on effect sizes
#' and confidence intervals around them, rather than \emph{P} values and
#' associated dichotomous hypothesis testing. {load}() collates the data in 
#' preparation for the computation of {effect sizes}. Bootstrap resampling is 
#' used to compute non-parametric assumption-free confidence intervals. 
#' Visualization of the effect sizes and their confidence intervals using 
#' estimation plots is then performed with a specialized {plotting} function.
#' 
#' 
#' @param data A tidydataframe
#' @param x Column name in \code{.data} that specifies the treatment group of 
#' the \code{.data}.
#' @param y Column name in \code{.data} that specifies the measurement values 
#'  of the \code{.data}.
#' @param idx A vector containing factors or strings, formatted as a vector 
#'  of multiple lists of strings in the \code{x} columns.
#'  These must be quoted (ie. surrounded by quotation marks). It specifies which 
#'  groups are selected for the boostrap calculations etc. 
#' @param paired Boolean, default FALSE. If TRUE, the two (or more than 2) 
#'  groups are treated as
#'  paired samples. The first group is treated as pre-intervention and the
#'  second group is considered post-intervention.
#' @param id_col Default NULL. A column name indicating the identity of the
#'  datapoint if the data is paired. \emph{This must be supplied if paired is
#'  \code{TRUE}.}
#' @param ci A double, default 95, ranging from 0 to 100, it is the width of the 
#'  confidence interval for the bca_low and bca_high confidence interval 
#'  calculations
#' @param colour String, default NULL. A column name in in the \code{x} column, 
#'  it specifies the column to implement colour aesthetics in plotting functions
#'  to distinguish each treatment groups from the other 
#'  (or differentiate experiment factors or factor levels).
#' @param proportional Boolean, default FALSE. If TRUE, the \code{.data} provided
#'  is binary data containing 0s and 1s. Proportion of success, failure, etc, 
#'  will be calculated based on the data entries.
#' @param minimeta Boolean, default FALSE. If TRUE, a mini meta-analysis will 
#'  be conducted if the dabest object is parsed into the effect-size functions across all groups 
#'  specified in \code{idx}. The weighted average of the effectsizes and 
#'  differences between groups will be calculated.
#' @param delta2 Boolean, default FALSE. A parameter for 2 by 2 factorial 
#'  experiment design. If TRUE, a delta-delta analysis will be conducted if 
#'  the dabest object is parsed into the effect-size functions. 
#' @param experiment Default NULL. This is the experiment column name for delta2 
#'  analysis, e.g Treatment (shouldn't be quoted).
#' @param experiment_label String, default NULL. It specifies 
#' the experiment label that is used to distinguish 
#' the experiment and the factors (being used in the plotting labels).
#' @param x1_level String, default NULL. Setting the first factor level in 
#' a 2 by 2 experimental design (corresponding to \code{delta2} parameter.
#'
#' @return A dabest object with 18 elements

#'  * raw_data    tibble, The dataset passed to \code{\link{dabest}}, 
#' stored here as a \code{\link[tibble]{tibble}}.

#'  * proportional_data     tibble, The proportional data passed to 
#' \code{\link{load}()} or calculated based on the \code{proportional}, 
#' stored here as a \code{\link[tibble]{tibble}}.

#'  * enquo_x     quoted variable, The columns in \code{data} 
#' used to plot the x axis, respectively, as supplied to \code{\link{load}()}, 
#' this will be the treatment group column names. These are 
#' \href{https://adv-r.hadley.nz/quasiquotation.html}{quoted variables} for
#' \href{https://tidyeval.tidyverse.org/}{tidy evaluation} during the 
#' computation of effect sizes.

#'  * enquo_y     quoted variable, The columns in \code{data} 
#' used to plot the y axis, respectively, as supplied to \code{\link{load}()}, 
#' it is the column name for measurement values. These are
#' \href{https://adv-r.hadley.nz/quasiquotation.html}{quoted variables} for 
#' \href{https://tidyeval.tidyverse.org/}{tidy evaluation} during the 
#' computation of effect sizes.

#'  * enquo_id_col    quoted variable, The columns in \code{.data} 
#' used to distinguish subjects in repeated measures (i.e enquo_id_col). 
#' This is \href{https://adv-r.hadley.nz/quasiquotation.html}{a quoted variable} 
#' for \href{https://tidyeval.tidyverse.org/}{tidy evaluation} during the 
#' computation of effect sizes.

#'  * enquo_colour    Boolean, Whether to plot colour aesthetics 
#' where the column name enquo_colour is supplied. This is 
#' \href{https://adv-r.hadley.nz/quasiquotation.html}{a quoted variable} 
#' for \href{https://tidyeval.tidyverse.org/}{tidy evaluation} during 
#' the computation of effect sizes.

#'  * proportional    Boolean, Whether the .data provided 
#' is proportional (i.e proportional) for 
#' plotting the proportional bars or sankey diagrams.

#'  * minimeta    Boolean, Whether mini meta analysis is 
#' conducted and plotted.

#'  * delta2    Boolean, Whether delta-delta analysis for 
#' 2 by 2 experimental designs are conducted.

#'  * idx     Vector, The vector of control-test groupings. 
#' For each pair in idx, an effect size will be computed by downstream 
#' dabestr functions used to compute \link[=mean_diff]{effect sizes} 
#' (such as mean_diff()).

#'  * is_paired     Boolean, Whether or not the experiment consists 
#' of paired (aka repeated) observations.

#'  * is_colour     Boolean, Whether or not the plots produced 
#' later will have/implement colour aesthetics to distinguish between 
#' experimental factor levels or treatment groups.

#'  * paired     Character, {paired: Whether or not the 
#' experiment consists of paired (aka repeated) observations. Whether 
#' they are compared with "baseline": all other groups compared with 
#' one control group; or "sequential": paired-wise comparison and calculations 
#' based on the order of consecutive pairs.}

#'  * ci     Numeric, The width of the confidence interval 
#' specified at the beginning for the effect size, and bootstrap calculations.

#'  * Ns     A tibble that contains Group (group names), 
#' n (number of data points in the group), and swarmtick labels 
#' for plotting (swarmticklabs).

#'  * control_summary    List, Summary values 
#' (mean values are calculated as default), for each control group, 
#' stored in a list respectively.

#'  * test_summary     List, Summary values 
#' (mean values are calculated as default), for each test group, 
#' stored in a list respectively.

#'  * ylim     Numeric, A vector of 2 doubles that 
#' specifies min and max values of the data. This will also 
#' be used for plotting in producing the plots.
#' 
#' @examples 
#' ## Loading in of the dataset
#' data(twogroup_data)
#' 
#' ## Creating a dabest object
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
  if (typeof(idx) != "list") {
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
      tidyr::unite(!!enquo_experiment,c(!!enquo_x,!!enquo_experiment),sep = " ",remove=FALSE)
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
  
  return(dabest_object)
}