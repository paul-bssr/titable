#' @title Compute numeric values for OR, IC, pvalue for a glm model
#'
#' @description A function to compute numeric values for OR, IC, pvalue from a
#' fitted glm model corresponding to a Logistic Regression (family="binomial").
#' It takes into account quantitative and categorical data.
#'
#' @param model A glm model corresponding to a logistic regression
#' (family=binomial)
#' @param studied_var Name of the variable to explain
#' @param level A string indicating the level to consider for categorical
#' variables (default:NULL)
#'
#' @return A data.frame containing 4 numeric columns : OR, IC_min, IC_max, p. It
#' has one row for quantitative, and (n-1) row for categorical data (with n the
#' number of categories)
#'
#' @import stats
#'
#' @export
#'
#'
#' @examples
#' model_1 <- glm( diagnosis ~ texture + radius + compactness_quartile,
#'                 data=wdbc.data,
#'                 family="binomial")
#'
#' # For quantitative variable
#' extract_OR_from_model(model_1, studied_var = "texture")
#'
#' # For categorical variable
#' extract_OR_from_model(model_1, studied_var = "compactness_quartile",
#'                       level="2")
#'


extract_OR_from_model <- function(model, studied_var, level=NULL){

  # Checking type of inputs
  stopifnot("Model must be binomial family glm" =
            (model$family$family == "binomial") )
  stopifnot("Input studied_var must be a character" =
            ( class(studied_var) == "character" ) )


  # Variable to look for
  if ( !is.null(level) ){
    coef_var <- paste(studied_var, level, sep="")
  } else {
    coef_var <- studied_var
  }

  # OR, p extraction from coef
  coef = coef(summary(model))
  stopifnot("Input level not in model variables" =
              ( coef_var %in%  rownames(coef) ) )

  index_var = which(rownames(coef)==coef_var)
  OR = exp( coef[index_var ,"Estimate"] )
  p = coef[index_var,"Pr(>|z|)"]

  # IC_min, IC_max extraction from confint
  conf_int = confint(model)
  index_conf_int = which(rownames(conf_int)==coef_var)
  IC_min = exp( conf_int[index_conf_int, "2.5 %"] )
  IC_max = exp( conf_int[index_conf_int, "97.5 %"] )
  return( c(OR, IC_min, IC_max, p) )
}
