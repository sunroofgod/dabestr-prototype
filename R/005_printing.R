#' Printing function for dabest class objects
#' 
#' @description 
#' A modified print function which is automatically generatated when the dabest class object
#' and the dabest_effectsize object are created via the [load()] and [effect_size()] function.
#' 
#' @usage 
#' print_dabest(dabest_object,
#' effectsize=NULL)
#' 
#' @param dabest_object
#' @param effectsize
#'
#' @details 
#' - If it is a dabest class object, the print function will inform you of the 
#' effect size with the confidence interval to be computed.
#' 
#' - If it is a dabest_effectsize class object, the print function will inform you 
#' of the effect size difference between the control and test groups, with the bca_low
#' and bca_high values at the specified confidence interval. 
#' 
#' @export
#'
#' @examples
#' ## Print statement will be seen when load() or effect_size() functions are implemented.
#' 
#' ## Loading in of dataset
#' data(twogroup_data)
#' 
#' ## Creation of dabest class object
#' dabest_obj <- load(data = twogroup_data, x = Group, y = Measurement, idx = c("Control1", "Group1"))
#' 
#' ## Creation of dabest_effectsize class object
#' dabest_obj.mean_diff <- mean_diff(dabest_obj)
#' 
print_dabest <- function(dabest_object,
                         effectsize = NULL) {
  if (class(dabest_object) != "dabest" & class(dabest_object) != "dabest_effectsize") {
    cli::cli_abort(c("Only dabest class or dabest_effectsize objects can be used.", 
                     "x" = "Please enter a valid entry into the function."))
  }
  
  print_greeting_header()
  
  paired <- dabest_object$paired
  ci <- dabest_object$ci
  if (!is.null(effectsize)) {
    es <- effectsize
  }
  
  if (is.null(paired)) {
    rm_status <- ""
  } else if (paired =="sequential") {
    rm_status <- "for the sequential design of repeated-measures experiment \n"
  } else if (paired=="baseline") {
    rm_status <- "for repeated measures against baseline \n"
  }
  
  if (is.null(paired)) {
    paired_status <- "E"
  } else if (paired =="sequential") {
    paired_status <- "Paired e"
  } else if (paired =="baseline") {
    paired_status <- "Paired e"
  }
  
  if (class(dabest_object) =="dabest") {
    line1 <- paste(paired_status,"ffect size(s) ",rm_status,sep="")
    line2 <- paste("with ",ci,"% confidence intervals will be computed for:",sep="")
    cat(line1)
    cat(line2)
    cat("\n")
    print_each_comparism(dabest_object)
    print_ending(dabest_object)
  } else if (class(dabest_object)=="dabest_effectsize") {
    
    print_each_comparism_effectsize(dabest_object,es)
    print_ending(dabest_object)
  }
}