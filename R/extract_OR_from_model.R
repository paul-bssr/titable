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
#' model_1 <- glm( diagnosis ~ texture + radius + perimeter,
#'                 data=wdbc.data,
#'                 family="binomial")
#' extract_OR_from_model(model_1, studied_var = "texture")
#'
extract_OR_from_model <- function(model, studied_var){
  # Function to
  # Returns : OR, IC_min, IC_max, p

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
  return( c(OR, IC_min, IC_max, p) )
}
