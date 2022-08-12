#' Title
#'
#' @title Compute and summarize in a string OR, IC, pvalue for a glm model
#'
#' @description Compute values for OR, IC, pvalue for a glm model and summarizes
#' result in a string of the form "OR (IC_min-IC_max, p=pvalue)"
#'
#' @param model A glm model corresponding to a logistic regression
#' (family=binomial)
#' @param studied_var Name of the variable to explain
#'
#' @return A string of the form "OR (IC_min-IC_max, p=pvalue)
#' @export
#'
#' @examples
#' model_1 <- glm( death_J28 ~ ., data=data, family="binomial")
#' extract_OR_to_str(model_1, col)

extract_OR_to_str <- function(model, studied_var){
  coef <- extract_OR_from_model(model, studied_var)
  str_final <- str_transform_OR_with_IC(
    OR = coef[1],
    IC_min = coef[2],
    IC_max = coef[3],
    pvalue = coef[4]
  )
  return(str_final)
}
