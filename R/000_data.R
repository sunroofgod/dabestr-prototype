#' Data to produce the basic 2-group Dabest plot
#'
#' Contains 2 control groups and 4 test groups.
#' @noRd
#' 
#' @format A data frame with 240 rows and 5 variables:
#'  \describe{
#'       \item{Dummy}{Dummy}
#'       \item{Gender}{Gender}
#'       \item{ID}{ID}
#'       \item{Group}{Which control group or test it is}
#'       \item{Measurement}{Measurement}
#'       }
#' 
#' @examples 
#' data(twogroup_data) # Lazy loading. Data becomes visible as soon as it is loaded.
"twogroup_data"




#' Data to proudce multi-group Dabest plot
#' 
#' Contains 3 control groups and 3 test groups.
#' @noRd
#' 
#' @format A data frame with 240 rows and 5 variables:
#'  \describe{
#'       \item{Dummy}{Dummy}
#'       \item{Gender}{Gender}
#'       \item{ID}{ID}
#'       \item{Group}{Which control group or test it is}
#'       \item{Measurement}{Measurement}
#'       }
#' 
#' @examples 
#' data(multigroup_data) # Lazy loading. Data becomes visible as soon as it is loaded
"multigroup_data"




#' Data to proudce proportional or sankey Dabest plot (Can also be used multi-sankey)
#' 
#' Contains 2 control groups and 4 test groups.
#' @noRd
#' 
#' @format A data frame with 240 rows and 3 variables:
#'  \describe{
#'       \item{Group}{Which control group or test it is}
#'       \item{Success}{1 or 0}
#'       \item{ID}{ID}
#'       }
#' 
#' @examples 
#' data(my.data.proportional) # Lazy loading. Data becomes visible as soon as it is loaded
"my.data.proportional"




#' Data to produce a mini-meta Dabest plot
#' 
#' Contains 3 control groups and 3 test groups.
#' @noRd
#' 
#' @format A data frame with 120 rows and 5 variables:
#'  \describe{
#'       \item{Dummy}{Dummy}
#'       \item{Gender}{Gender}
#'       \item{ID}{ID}
#'       \item{Group}{Which control group or test it is}
#'       \item{Measurement}{Measurement}
#'       }
#' 
#' @examples 
#' data(minimeta_data) # Lazy loading. Data becomes visible as soon as it is loaded
"minimeta_data"




#' Data to produce a delta2 Dabest plot
#' 
#' Contains 2 genotype groups and 2 treatment groups.
#' @noRd
#' 
#' @format A data frame with 80 rows and 6 variables:
#'  \describe{
#'       \item{Dummy}{Dummy}
#'       \item{Genotype}{Genotype}
#'       \item{ID}{ID}
#'       \item{Rep}{Rep}
#'       \item{Treatment}{Which treatment method was used}
#'       \item{Measurement}{Measurement}
#'       }
#' 
#' @examples 
#' data(deltadelta_data) # Lazy loading. Data becomes visible as soon as it is loaded
"deltadelta_data"









