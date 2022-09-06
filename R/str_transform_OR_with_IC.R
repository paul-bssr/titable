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
#' model_1 <- glm( diagnosis ~ texture + radius + perimeter,
#'                 data=wdbc.data,
#'                 family="binomial")
#' coef <- extract_OR_from_model(model_1, studied_var = "texture")
#' str_transform_OR_with_IC(coef)
#' str_transform_OR_with_IC(coef, digits=1, digits_p=4)
#'

str_transform_OR_with_IC <- function(coef_df, digits=3,
                                     digits_p=3){

  stopifnot("Input OR must be numeric" = is.numeric(coef_df$OR))
  stopifnot("Input IC_min must be numeric" = is.numeric(coef_df$IC_min))
  stopifnot("Input IC_max must be numeric" = is.numeric(coef_df$IC_max))
  stopifnot("Input pvalue must be numeric" = is.numeric(coef_df$p))
  stopifnot(
    "Input OR must be a positive number" = (sum(coef_df$OR<0)==0)
    )
  stopifnot(
    "Input IC_min must be a positive number" = (sum(coef_df$IC_min<0)==0)
    )
  stopifnot(
    "Input IC_max must be a positive number" = (sum(coef_df$IC_max<0)==0)
    )
  stopifnot(
    "Input pvalue must be a positive number" = (sum(coef_df$p<0)==0)
    )


  OR_str = data.frame(
    apply(coef_df, MARGIN = 1,
          FUN = function (x) paste(
            round(x["OR"],digits = digits), " (",
            round(x["IC_min"], digits = digits ), "-",
            round(x["IC_max"], digits = digits ), ", p=",
            round( x["p"], digits = digits_p ), ")",
            sep=""
            )
          )
  )

  colnames(OR_str) <- "str_OR_IC"

  return( OR_str )
}
