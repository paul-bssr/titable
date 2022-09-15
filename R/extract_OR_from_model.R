#' @title Compute numeric values for OR, IC, pvalue for a glm model
#'
#' @description A function to compute numeric values for OR, IC, pvalue from a
#' fitted glm model corresponding to a Logistic Regression (family="binomial")
#'
#' @param model A glm model corresponding to a logistic regression
#' (family=binomial)
#' @param studied_var Name of the variable to explain
#'
#' @return A vector containing 4 numeric values : OR, IC_min, IC_max, p
#'
#' @import stats
#'
#' @export
#'
#'
#' @examples
#' ### Only quantitative data
#' model_1 <- glm( diagnosis ~ texture + radius + perimeter,
#'                 data=wdbc.data,
#'                 family="binomial")
#' extract_OR_from_model(model_1, studied_var = "texture")
#'
#'
#' ### With factors
#' model_2 <- glm( diagnosis ~ compactness_quartile + radius + perimeter,
#'                 data=wdbc.data,
#'                 family="binomial")
#' extract_OR_from_model(model_2, studied_var = "compactness_quartile")
#'
extract_OR_from_model <- function(model, studied_var){

  # Checking type of inputs
  stopifnot("Model must be binomial family glm" =
            (model$family$family == "binomial") )
  stopifnot("Input studied_var must be a character" =
            ( class(studied_var) == "character" ) )

  # OR, p extraction from coef
  coef = coef(summary(model))
  cond_studied_var_coef = grepl( studied_var , rownames(coef))
  OR = exp( coef[cond_studied_var_coef ,"Estimate"] )
  p = coef[cond_studied_var_coef,"Pr(>|z|)"]

  # IC_min, IC_max extraction from confint
  conf_int = confint(model)
  cond_conf_int = grepl( studied_var , rownames(conf_int))
  IC_min = exp( conf_int[cond_conf_int, "2.5 %"] )
  IC_max = exp( conf_int[cond_conf_int, "97.5 %"] )
  return( data.frame(OR, IC_min, IC_max, p) )
}
