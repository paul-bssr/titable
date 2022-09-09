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
#' @param level A string indicating the level to consider for categorical
#' variables (default:NULL)
#'
#' @return A string of the form "OR (IC_min-IC_max, p=pvalue)
#' @export
#'
#' @examples
#' model_1 <- glm( diagnosis ~ texture + radius + compactness_quartile,
#'                 data=wdbc.data,
#'                 family="binomial")
#'
#' # For quantitative variable
#' extract_OR_to_str(model_1, studied_var = "texture")
#'
#' # For categorical variable
#' extract_OR_to_str(model_1, studied_var = "compactness_quartile", level="2")

extract_OR_to_str <- function(model, studied_var, level=NULL){

  # Checking type of inputs
  stopifnot("Model must be binomial family glm" =
              (model$family$family == "binomial") )
  stopifnot("Input studied_var must be a character" =
              ( class(studied_var) == "character" ) )

  # Computing coefficients
  coef <- extract_OR_from_model(model, studied_var, level)

  # Converting to a string
  str_final <- str_transform_OR_with_IC(
    OR = coef[1],
    IC_min = coef[2],
    IC_max = coef[3],
    pvalue = coef[4]
  )
  return(str_final)
}
