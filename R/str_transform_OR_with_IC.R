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
#' @param digits_p An integer input giving the number of significant digits for
#' p_value (use of signif function)
#' @param p_limit A float giving the limit value below which pvalue is printed
#' as "<p_limit"
#'
#' @return a string "OR (IC_min-IC_max, p=pvalue)"
#'
#' @export
#'
#' @examples
#' str_transform_OR_with_IC(OR=2.02, IC_min=1.803, IC_max=2.405, pvalue=0.0025)
#' str_transform_OR_with_IC(OR=2.02, IC_min=1.807, IC_max=2.403, pvalue=0.02564,
#' digits=2, digits_p=3)
#' str_transform_OR_with_IC(OR=2.02, IC_min=1.803, IC_max=2.405, pvalue=0.0003,
#' p_limit=0.001)
#'

str_transform_OR_with_IC <- function(OR, IC_min,
                                     IC_max,
                                     pvalue,
                                     digits=3,
                                     digits_p=3,
                                     p_limit=NULL){

  stopifnot("Input OR must be numeric" = is.numeric(OR))
  stopifnot("Input IC_min must be numeric" = is.numeric(IC_min))
  stopifnot("Input IC_max must be numeric" = is.numeric(IC_max))
  stopifnot("Input pvalue must be numeric" = is.numeric(pvalue))
  stopifnot("Input OR must be a positive number" = (OR>0))
  stopifnot("Input IC_min must be a positive number" = (IC_min>0))
  stopifnot("Input IC_max must be a positive number" = (IC_max>0))
  stopifnot("Input pvalue must be a positive number" = (pvalue>0))

  # Printing pvalue depending on p_limit
  pvalue_char <- paste(
    "p=",
    as.character( signif( pvalue, digits = digits_p )),
    sep="")

  if (!is.null(p_limit)){
    if (pvalue < p_limit){
      pvalue_char <- paste("p<", p_limit, sep="")
    }
  }

  # Construction of final character
  OR_str = paste(
    as.character( round( OR, digits = digits) ),
    " (",
    as.character( round( IC_min, digits = digits ) ),
    "-",
    as.character( round( IC_max, digits = digits ) ),
    ", ",
    pvalue_char,
    ")",
    sep=""
  )

  return( OR_str )
}
