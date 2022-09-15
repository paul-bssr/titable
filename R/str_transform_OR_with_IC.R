#' Title
#'
#' @description Function to summarize OR, IC, pvalue from their numerical values
#' in a string of the form "OR (IC_min-ICmax, p=pvalue)"
#'
#' @param OR A numeric input corresponding to an Odd Ratio
#' @param IC_min A numeric input corresponding to the lower boundary of the 95%
#' interval associated to OR
#' @param IC_max A numeric input corresponding to higher boundary of the 95%
#' interval associated to OR
#' @param pvalue A numeric input corresponding to the pvalue associated to OR
#' @param digits An integer input giving the number of digits for rounding OR
#' and IC
#' @param digits_p An integer input giving the number of digits for rounding
#' p_value
#'
#' @return a string "OR (IC_min-IC_max, p=pvalue)"
#'
#' @export
#'
#' @examples
#' str_transform_OR_with_IC(OR=2.02, IC_min=1.803, IC_max=2.405, pvalue=0.0025)
#' str_transform_OR_with_IC(OR=2.02, IC_min=1.803, IC_max=2.405, pvalue=0.0025,
#' digits=1, digits_p=4)
#'

str_transform_OR_with_IC <- function(OR, IC_min, IC_max, pvalue, digits=3,
                                     digits_p=3){

  stopifnot("Input OR must be numeric" = is.numeric(OR))
  stopifnot("Input IC_min must be numeric" = is.numeric(IC_min))
  stopifnot("Input IC_max must be numeric" = is.numeric(IC_max))
  stopifnot("Input pvalue must be numeric" = is.numeric(pvalue))
  stopifnot("Input OR must be a positive number" = (OR>0))
  stopifnot("Input IC_min must be a positive number" = (IC_min>0))
  stopifnot("Input IC_max must be a positive number" = (IC_max>0))
  stopifnot("Input pvalue must be a positive number" = (pvalue>0))

  OR_str = paste(
    as.character( round( OR, digits = digits) ),
    " (",
    as.character( round( IC_min, digits = digits ) ),
    "-",
    as.character( round( IC_max, digits = digits ) ),
    ", p=",
    as.character( round( pvalue, digits = digits_p ) ),
    ")",
    sep=""
  )

  return( OR_str )
}
