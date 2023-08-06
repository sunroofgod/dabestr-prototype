# Obtains bootstraps as well as bca intervals
# 
# Contains functions `effsize_boot`, `bootstrap`, `bca` and `boot_weighted_row`. 

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
  
  b <- boot::boot(
    c(data$control, data$test),
    statistic = bootboot,
    R = reps,
    strata = s,
    paired = paired
  )
  
  return(b)
}

# Main bootstrap function
bootstrap <- function(
    dabest_obj,
    effect_size_func,
    seed = 12345,
    reps = 5000,
    boot_labs
){
  
  boot_result <- tibble::tibble()
  
  raw_data <- dabest_obj$raw_data
  idx <- dabest_obj$idx
  
  if (isFALSE(is.list(idx))) {
    idx <- list(idx)
  }
  enquo_x <- dabest_obj$enquo_x
  enquo_y <- dabest_obj$enquo_y
  ci <- dabest_obj$ci
  paired <- dabest_obj$paired
  is_paired <- dabest_obj$is_paired
  is_colour <- dabest_obj$is_colour
  
  proportional <- dabest_obj$proportional
  
  quoname_x <- rlang::as_name(enquo_x)
  quoname_y <- rlang::as_name(enquo_y)
  delta_x_labels <- list()
  delta_y_labels <- boot_labs
  
  minimeta <- dabest_obj$minimeta
  delta2 <- dabest_obj$delta2
  
  ## Validity Checks
  if (isTRUE(is_paired) && boot_labs == "Cliffs' delta") {
    cli::cli_abort(c("{.var Cliffs delta} cannot be found when {.field paired} is not NULL.",
                     "x" = "Please change {.var effect_size_func}."))
  } else if (isTRUE(proportional) && !(boot_labs %in% c("Mean difference","Cohen's h","Paired\nmean difference"))) {
    cli::cli_abort(c("Other effect sizes besides {.var Cohens h} and {.var Mean difference} cannot be found when {.field                         paired} is not NULL.","x" = "Please change {.var effect_size_func}."))
  }
  
  if (isFALSE(is_paired) || isTRUE(paired == "baseline")) {
    for (group in idx) {
      group_length <- length(group)
      
      ctrl_tibble <- raw_data %>% 
        dplyr::filter(!!enquo_x == !!group[1])
      ctrl_measurement <- ctrl_tibble[[quoname_y]]
      
      tests <- group[2:group_length]
      
      ctrl_size <- length(ctrl_measurement)
      ctrl_var <- var_w_df(ctrl_measurement, ctrl_size)
      
      
      for (test_group in tests) {
        test_tibble <- raw_data %>%
          dplyr::filter(!!enquo_x == !!test_group)
        
        test_measurement <- test_tibble[[quoname_y]]
        
        xlabels <- paste(test_group, group[1], sep="\nminus\n")
        delta_x_labels <- append(delta_x_labels, xlabels)
        
        control_test_measurement <- list(control = ctrl_measurement,
                                         test = test_measurement)
        
        
        test_size <- length(test_measurement)
        test_var <- var_w_df(test_measurement, test_size)
        
        grp_var <- calculate_group_variance(ctrl_var = ctrl_var,
                                            ctrl_N = ctrl_size,
                                            test_var = test_var,
                                            test_N = test_size)
        
        weight <- 1/grp_var
        
        set.seed(seed)
        
        boots <- effsize_boot(data = control_test_measurement,
                              effect_size_func = effect_size_func,
                              reps = reps,
                              paired = is_paired)
        
        if (ci < 0 | ci > 100) {
          cli::cli_abort(c("{.field ci} is not between 0 and 100.",
                           "x" = "{.field ci} must be between 0 and 100, not {ci}."))
        }
        
        bootci <- boot::boot.ci(boots, conf=ci/100, type = c("perc","bca"))
        
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
          difference = boots$t0,
          weight = weight
        )
        boot_result <- dplyr::bind_rows(boot_result, boot_row)
      }
    }
  } else {
    for (group in idx) {
      group_length <- length(group)
      for (i in 1:(group_length-1)) {
        control_group <- group[i]
        test_group <- group[i+1]
        
        ctrl_tibble <- raw_data %>% 
          dplyr::filter(!!enquo_x == !!control_group)
        ctrl_measurement <- ctrl_tibble[[quoname_y]]
        
        test_tibble <- raw_data %>% 
          dplyr::filter(!!enquo_x == !!test_group)
        test_measurement <- test_tibble[[quoname_y]]
        
        xlabels <- paste(test_group, control_group, sep="\nminus\n")
        delta_x_labels <- append(delta_x_labels, xlabels)
        
        control_test_measurement <- list(control = ctrl_measurement,
                                         test = test_measurement)
        #add weights column
        ctrl_size <- length(ctrl_measurement)
        ctrl_var <- var_w_df(ctrl_measurement, ctrl_size)
        test_size <- length(test_measurement)
        test_var <- var_w_df(test_measurement, test_size)
        grp_var <- calculate_group_variance(ctrl_var = ctrl_var,
                                            ctrl_N = ctrl_size,
                                            test_var = test_var,
                                            test_N = test_size)
        
        weight <- 1/grp_var
        
        set.seed(seed)
        
        boots <- effsize_boot(data = control_test_measurement,
                              effect_size_func = effect_size_func,
                              reps = reps,
                              paired = is_paired)
        
        if (ci < 0 | ci > 100) {
          cli::cli_abort(c("{.field ci} is not between 0 and 100.",
                           "x" = "{.field ci} must be between 0 and 100, not {ci}."))
        }
        
        bootci <- boot::boot.ci(boots, conf=ci/100, type = c("perc","bca"))
        
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
          difference = boots$t0,
          weight = weight
        )
        boot_result <- dplyr::bind_rows(boot_result, boot_row)
      }
    }
  }
  if (isTRUE(minimeta)){
    boot_last_row <- boot_weighted_row(boot_result = boot_result, ci)
    boot_result <- dplyr::bind_rows(boot_result, boot_last_row)
  }
  if (isTRUE(delta2)) {
    boot_last_row <- boot_delta_delta(boot_result = boot_result,ci)
    boot_result <- dplyr::bind_rows(boot_result,boot_last_row)
  }
  
  raw_y_labels <- ifelse(proportional, "proportion of success", "value")
  
  out <- list(raw_data = raw_data,
              idx = idx,
              delta_x_labels = delta_x_labels,
              delta_y_labels = delta_y_labels,
              raw_y_labels = raw_y_labels,
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
              proportional = proportional,
              minimeta = minimeta,
              delta2 = dabest_obj$delta2,
              proportional_data = dabest_obj$proportional_data,
              boot_result = boot_result)
  
  class(out) <- c("dabest_effectsize")
  
  return(out)
}

# BCA function
bca <- function(bootstraps, conf.level = .95){
  # Inverse Variance Method
  if(stats::var(bootstraps)==0){
    lower <- mean(bootstraps)
    upper <- mean(bootstraps)
    return(c(lower, upper))
  }
  
  if(max(bootstraps)==Inf | min(bootstraps)==-Inf){
    stop("bca() function does not work when some values are infinite")
  }
  
  low <- (1 - conf.level)/2
  high <- 1 - low
  sims <- length(bootstraps)
  z.inv <- length(bootstraps[bootstraps < mean(bootstraps)])/sims
  z <- stats::qnorm(z.inv)
  U <- (sims - 1) * (mean(bootstraps, na.rm=TRUE) - bootstraps)
  top <- sum(U^3)
  under <- 6 * (sum(U^2))^{3/2}
  a <- top / under
  lower.inv <-  stats::pnorm(z + (z + stats::qnorm(low))/(1 - a * (z + stats::qnorm(low))))
  lower <- stats::quantile(bootstraps, lower.inv, names=FALSE)
  upper.inv <-  stats::pnorm(z + (z + stats::qnorm(high))/(1 - a * (z + stats::qnorm(high))))
  upper <- stats::quantile(bootstraps, upper.inv, names=FALSE)
  return(c(lower, upper))
} 

# Creates df of values (bca ci, weighted bootstraps) for minimeta
boot_weighted_row <- function(boot_result, ci){
  bootstraps <- boot_result$bootstraps
  weights <- boot_result$weight
  
  weighted_result <- Map(function(x, w) x * w,
                         boot_result$bootstraps, boot_result$weight)
  weighted_bootstrap <- Reduce("+", weighted_result)
  weighted_bootstrap <- weighted_bootstrap/sum(weights)
  
  
  weighted_difference <- calculate_weighted_delta(weight = boot_result$weight,
                                                  differences = boot_result$difference)
  bca_weighted <- bca(bootstraps = weighted_bootstrap)
  pct_interval <- confinterval(weighted_bootstrap, ci/100)
  boot_last_row <- list(
    control_group = 'Minimeta Overall Test',
    test_group = 'Minimeta Overall Test',
    bootstraps = list(as.vector(weighted_bootstrap)),
    nboots = length(weighted_bootstrap),
    bca_ci_low = bca_weighted[1],
    bca_ci_high = bca_weighted[2],
    pct_ci_low = pct_interval[1],
    pct_ci_high = pct_interval[2],
    ci = ci,
    difference = weighted_difference,
    weight = 1
  )
  return(boot_last_row)
}

# Creates df of values (bca ci, weighted bootstraps) for deltadelta
boot_delta_delta <- function(boot_result,ci) {
  bootstrap_delta_delta_neg <- Reduce("-",boot_result$bootstraps)
  bootstrap_delta_delta <- bootstrap_delta_delta_neg *-1
  difference_delta_delta <- calculate_delta_difference(boot_result$difference)
  bca_delta_delta <- bca(bootstrap_delta_delta)
  pct_interval <- confinterval(bootstrap_delta_delta,ci/100)
  boot_last_row <- list(
    control_group = 'Delta2 Overall Test',
    test_group = 'Delta2 Overall Test',
    bootstraps = list(as.vector(bootstrap_delta_delta)),
    nboots = length(bootstrap_delta_delta),
    bca_ci_low = bca_delta_delta[1],
    bca_ci_high = bca_delta_delta[2],
    pct_ci_low = pct_interval[1],
    pct_ci_high = pct_interval[2],
    ci = ci,
    difference = difference_delta_delta,
    weight = 1
  )
}

#Obtain permutation tests, permutations and p values

PermutationTest <- function(control,
                            test,
                            effect_size,
                            is_paired = NULL,
                            permutation_count = 5000,
                            random_seed = 12345,
                            ef_size_fn) {
  
  # Check if the arrays have the same length for paired test
  if (!is.null(is_paired) && length(control) != length(test)) {
    stop("The two arrays do not have the same length.")
  }
  
  # Initialize random number generator
  set.seed(random_seed)
  
  # Set required constants and variables
  control <- as.numeric(control)
  test <- as.numeric(test)
  
  control_sample <- control
  test_sample <- test
  
  BAG <- c(control, test)
  CONTROL_LEN <- length(control)
  EXTREME_COUNT <- 0
  THRESHOLD <- abs(ef_size_fn(control, test, is_paired))
  permutations <- vector("numeric", length = permutation_count)
  permutations_var <- vector("numeric", length = permutation_count)
  
  for (i in 1:permutation_count) {
    
    if (!is.null(is_paired)) {
      # Select which control-test pairs to swap.
      random_idx <- sample(1:CONTROL_LEN, size = sample(1:CONTROL_LEN, 1),
                           replace = TRUE)
      
      # Perform swap.
      for (idx in random_idx) {
        placeholder <- control_sample[idx]
        control_sample[idx] <- test_sample[idx]
        test_sample[idx] <- placeholder
      }
      
    } else {
      # Shuffle the bag and assign to control and test groups.
      shuffled <- sample(BAG)
      control_sample <- shuffled[1:CONTROL_LEN]
      test_sample <- shuffled[(CONTROL_LEN + 1):length(shuffled)]
    }
    
    es <- ef_size_fn(control_sample, test_sample, is_paired)
    
    control_var <- stats::var(control_sample, na.rm = TRUE)
    test_var <- stats::var(test_sample, na.rm = TRUE)
    control_N <- length(control_sample)
    test_N <- length(test_sample)
    var <- calculate_group_variance(control_var, control_N, test_var, test_N)
    
    permutations[i] <- es
    permutations_var[i] <- var
    
    if (abs(es) > THRESHOLD) {
      EXTREME_COUNT <- EXTREME_COUNT + 1
    }
  }
  
  pvalue <- EXTREME_COUNT / permutation_count
  
  perm_results <- list(
    permutations = permutations,
    permutations_var = permutations_var,
    pvalue = pvalue,
    es = es
  )
  
  return(perm_results)
}

# p values

pvals_statistics <- function(control, 
                             test, 
                             is_paired,
                             proportional,
                             effect_size){
  
  pvals_stats <- list()
  if (!is.null(is_paired) && !proportional) {
    # Wilcoxon test (non-parametric version of the paired T-test)
    wilcoxon <- stats::wilcox.test(control, test)
    pvalue_wilcoxon <- wilcoxon$p.value
    statistic_wilcoxon <- wilcoxon$statistic
    
    paired_t <- NA
    pvalue_paired_students_t <- NA
    statistic_paired_students_t <-NA
    
    if (effect_size != "median_diff") {
      # Paired Student's t-test
      paired_t <- stats::t.test(control, test, paired = TRUE, na.rm = TRUE)
      pvalue_paired_students_t <- paired_t$p.value
      statistic_paired_students_t <- paired_t$statistic
    }
    pvals_stats <- list(pvalue_wilcoxon = pvalue_wilcoxon,
                        wilcoxon = wilcoxon,
                        statistic_wilcoxon = statistic_wilcoxon,
                        paired_t = paired_t,
                        pvalue_paired_students_t = pvalue_paired_students_t,
                        statistic_paired_students_t = statistic_paired_students_t)
  } else if (!is.null(is_paired) && proportional) {
    # McNemar's test for binary paired data
    table <- matrix(c(sum(control == 0 & test == 0), sum(control == 0 & test == 1),
                      sum(control == 1 & test == 0), sum(control == 1 & test == 1)), 
                    nrow = 2, byrow = TRUE)
    mcnemar_result <- stats::mcnemar.test(table, correct = TRUE)
    pvalue_mcnemar <- mcnemar_result$p.value
    statistic_mcnemar <- mcnemar_result$statistic
    pvals_stats <- list(pvalue_mcnemar = pvalue_mcnemar,
                        statistic_mcnemar = statistic_mcnemar)
  } else if (effect_size == "cliffs_delta") {
    # Brunner-Munzel test
    brunner_munzel <- coin::brunnermunzel.test(control, test, na.rm = TRUE)
    pvalue_brunner_munzel <- brunner_munzel$p.value
    statistic_brunner_munzel <- brunner_munzel$statistic
    pvals_stats <- list(pvalue_brunner_munzel = pvalue_brunner_munzel,
                        statistic_brunner_munzel = statistic_brunner_munzel)
  } else if (effect_size == "median_diff") {
    # Kruskal-Wallis H-test
    kruskal <- stats::kruskal.test(list(control, test))
    pvalue_kruskal <- kruskal$p.value
    statistic_kruskal <- kruskal$statistic
    pvals_stats <- list(pvalue_kruskal = pvalue_kruskal,
                        statistic_kruskal = statistic_kruskal)
  } else {
    # For mean difference, Cohen's d, and Hedges' g
    # Welch's t-test (equal_var = FALSE) to not assume equal variances
    welch <- stats::t.test(control, test, equal.var = FALSE, na.rm = TRUE)
    pvalue_welch <- welch$p.value
    statistic_welch <- welch$statistic
    
    # Student's t-test (equal_var = TRUE) to assume equal variances
    students_t <- stats::t.test(control, test, equal.var = TRUE, na.rm = TRUE)
    pvalue_students_t <- students_t$p.value
    statistic_students_t <- students_t$statistic
    
    # Mann-Whitney test: non-parametric, does not assume normality of distributions
    tryCatch({
      mann_whitney <- stats::wilcox.test(control, test, alternative = "two.sided")
      pvalue_mann_whitney <- mann_whitney$p.value
      statistic_mann_whitney <- mann_whitney$statistic
    }, error = function(e) {
      # Occurs when the control and test are exactly identical in terms of rank (e.g., all zeros).
      pvalue_mann_whitney <- NA
      statistic_mann_whitney <- NA
    })
    
    standardized_es <- effsize::cohen.d(control, test, is_paired = NULL)
    
    # Cohen's h calculation for binary categorical data
    tryCatch({
      cohens_h_cal <- function(control, test) {
        #remove nas and nulls later on
        prop_control <- mean(control)
        prop_test <- mean(test)
        
        # Arcsine transformation
        phi_control <- 2 * asin(sqrt(prop_control))
        phi_test <- 2 * asin(sqrt(prop_test))
        result <- phi_test - phi_control
        return(result)
      }
      proportional_difference <- cohens_h_cal(control, test)
    }, error = function(e) {
      # Occur only when the data consists not only 0's and 1's.
      proportional_difference <- NA
    })
    
    pvals_stats <- list(pvalue_welch = pvalue_welch,
                        statistic_welch = statistic_welch,
                        # Student's t-test (equal_var = TRUE) to assume equal variances
                        students_t = students_t,
                        pvalue_students_t = pvalue_students_t,
                        statistic_students_t = statistic_students_t,
                        # Mann-Whitney test: non-parametric, does not assume normality of distributions
                        
                        pvalue_mann_whitney =  pvalue_mann_whitney, 
                        statistic_mann_whitney = statistic_mann_whitney)
  }
  
  return (pvals_stats)
}

# collate permtest and p values with function "Pvalues_statistics"

Pvalues_statistics <- function(dabest_object,
                               seed = 12345,
                               perm_count = 5000,
                               ef_size_fn = NULL,
                               effect_size_type) {
  
  permtest_pvals <- tibble::tibble()
  
  # check if effect size function is supplied
  if (is.null(ef_size_fn)){
    stop("No effect size calculation methods are supplied.")
  }
  
  raw_data <- dabest_object$raw_data
  idx <- dabest_object$idx
  
  if (isFALSE(is.list(idx))) {
    idx <- list(idx)
  }
  enquo_x <- dabest_object$enquo_x
  enquo_y <- dabest_object$enquo_y
  ci <- dabest_object$ci
  paired <- dabest_object$paired
  is_paired <- dabest_object$is_paired
  
  proportional <- dabest_object$proportional
  
  quoname_x <- rlang::as_name(enquo_x)
  quoname_y <- rlang::as_name(enquo_y)
  
  minimeta <- dabest_object$minimeta
  delta2 <- dabest_object$delta2
  
  if (isFALSE(is_paired) || isTRUE(paired == "baseline")) {
    for (group in idx) {
      control_group <- group[1]
      group_length <- length(group)
      
      ctrl_tibble <- raw_data %>% 
        dplyr::filter(!!enquo_x == !!group[1])
      ctrl_measurement <- ctrl_tibble[[quoname_y]]
      
      tests <- group[2:group_length]
      
      for (test_group in tests) {
        test_group <- test_group
        test_tibble <- raw_data %>%
          dplyr::filter(!!enquo_x == !!test_group)
        
        test_measurement <- test_tibble[[quoname_y]]
        
        xlabels <- paste(test_group, group[1], sep="\nminus\n")
        
        test_size <- length(test_measurement)
        
        es <- ef_size_fn(ctrl_measurement, test_measurement, paired)
        
        # do permutation tests accordingly
        PermutationTest_result <- PermutationTest(ctrl_measurement, 
                                                  test_measurement, 
                                                  effect_size = effect_size_type, 
                                                  is_paired = is_paired,
                                                  permutation_count = 5000, 
                                                  random_seed = 12345,
                                                  ef_size_fn = ef_size_fn)
        
        # calculate p values 
        pvals_and_stats <- pvals_statistics(ctrl_measurement, 
                                            test_measurement, 
                                            is_paired = is_paired,
                                            proportional = proportional,
                                            effect_size = effect_size_type)
        
        pval_row <- list(
          control_group = control_group,
          test_group = test_group,
          ef_sz_real = es,
          pval_permtest = PermutationTest_result$pvalue,
          permutation_test_results = list(PermutationTest_result),
          pval_for_tests = pvals_and_stats[1],
          pvalues = list(pvals_and_stats)
        )
        
        permtest_pvals <- dplyr::bind_rows(permtest_pvals, pval_row)
      }
    }
  } else {
    for (group in idx) {
      group_length <- length(group)
      for (i in 1:(group_length-1)) {
        control_group <- group[i]
        test_group <- group[i+1]
        
        ctrl_tibble <- raw_data %>% 
          dplyr::filter(!!enquo_x == !!control_group)
        ctrl_measurement <- ctrl_tibble[[quoname_y]]
        
        test_tibble <- raw_data %>% 
          dplyr::filter(!!enquo_x == !!test_group)
        test_measurement <- test_tibble[[quoname_y]]
        
        xlabels <- paste(test_group, control_group, sep="\nminus\n")
        
        control_test_measurement <- list(control = ctrl_measurement,
                                         test = test_measurement)
        #add weights column
        ctrl_size <- length(ctrl_measurement)
        ctrl_var <- var_w_df(ctrl_measurement, ctrl_size)
        test_size <- length(test_measurement)
        test_var <- var_w_df(test_measurement, test_size)
        grp_var <- calculate_group_variance(ctrl_var = ctrl_var,
                                            ctrl_N = ctrl_size,
                                            test_var = test_var,
                                            test_N = test_size)
        
        es <- ef_size_fn(ctrl_measurement, test_measurement, paired)
        
        # do permutation tests accordingly
        PermutationTest_result <- PermutationTest(ctrl_measurement, 
                                                  test_measurement, 
                                                  effect_size = effect_size_type,
                                                  is_paired = is_paired,
                                                  permutation_count = 5000, 
                                                  random_seed = 12345,
                                                  ef_size_fn = ef_size_fn)
        # calculate p values 
        pvals_and_stats <- pvals_statistics(ctrl_measurement, 
                                            test_measurement, 
                                            is_paired = is_paired,
                                            proportional = proportional,
                                            effect_size = effect_size_type)
        
        
        pval_row <- list(
          control_group = control_group,
          test_group = test_group,
          ef_sz_real = es,
          pval_permtest = PermutationTest_result$pvalue,
          permutation_test_results = list(PermutationTest_result),
          pval_for_tests = pvals_and_stats[1],
          pvalues = list(pvals_and_stats)
        )
        
        permtest_pvals <- dplyr::bind_rows(permtest_pvals, pval_row)
      }
    }
  }
  
  return(list(permtest_pvals = permtest_pvals))
}
