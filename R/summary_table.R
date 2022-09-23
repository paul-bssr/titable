#' @title Build summary table for OR univariate/mulitvariate
#'
#' @description Function building a summary table for Odds Ratios (OR) computed
#' for a given `dependent` variable with one line for each explanatory variables
#' given in `studied_vars`. If `univariate` is  `TRUE` OR computed using
#' univariate logistic regressions are added to a column "Univariable model".
#' Finally, for each set of adjustment variables in `multivariate` a column
#' giving the OR after adjustment is added. Remark : if `dependent` variable is
#' also in the adjustement set, it is automatically removed.
#'
#' @param data A data.frame containing the data to be analyzed
#' @param dependent Character input containing the name of the binary variable
#' to explain
#' @param studied_vars Vector of characters with names of the different
#' explanatory variables to study
#' @param univariate Boolean. If TRUE, addition of univariate model to final
#' table
#' @param multivariate List of character vectors containing different sets to be
#' used for adjustement.
#' @param digits An integer input giving the number of digits for rounding OR
#' and IC
#' @param digits_p An integer input giving the number of significant digits for
#' p_value (use of signif function)
#' @param p_limit A float giving the limit value below which pvalue is printed
#' as "<p_limit"
#'
#' @return A data.frame containing :
#' \itemize{
#' \item{ One row for each explanatory variables in `studied_vars`}
#' \item{ Columns describing their distribution in the different categories of
#' `dependent` variable }
#' \item{ Columns for each univariate or multivariate model }
#' }
#'
#' @import finalfit
#' @export
#'
#' @examples
#' summary_table(data = wdbc.data,
#'               studied_vars = c("radius", "texture", "compactness_quartile"),
#'               dependent = "diagnosis",
#'               univariate = FALSE
#'              )
#'
#' summary_table(data = wdbc.data,
#'               studied_vars = c("radius", "texture", "compactness_quartile"),
#'               dependent = "diagnosis" )
#'
#' summary_table(data = wdbc.data,
#'               studied_vars = c("radius", "texture", "compactness_quartile"),
#'               dependent = "diagnosis",
#'               multivariate = list(c("smoothness", "texture"),
#'                                   c("concavity", "symmetry")),
#'               digits = 2,
#'               digits_p = 2,
#'               p_limit=0.001
#'              )

summary_table <- function(data,
                          studied_vars,
                          dependent,
                          univariate=TRUE,
                          multivariate=NULL,
                          digits = 3,
                          digits_p = 1,
                          p_limit = NULL
                          ) {
  # Checks
  for (col in studied_vars){
    if ( !(col %in% colnames(data)) ) {
      str_check_col = paste("Column", col, "of studied_vars not in data columns")
      stop(str_check_col)
    }
  }

  # Creation table skeleton
  table <- summary_factorlist(data, dependent, studied_vars)


  counter = 0
  for ( col in studied_vars ){
    print (col)
    ## Adding univariate model
    if (univariate){
      # Computing univariate model
      print ('Computing univariate')
      frm <- as.formula(paste(dependent, "~", col))
      model_univariate <- glm( formula = frm,
                               data=data,
                               family="binomial")

      # Assigning the result in output table
      table <- extract_OR_to_table( data = data,
                                    table = table,
                                    studied_var = col,
                                    model = model_univariate,
                                    OR_colname ="OR (univariate)",
                                    digits = digits,
                                    digits_p = digits_p,
                                    p_limit = p_limit
                                    )

    }


    ## Adding multivariable models
    if (is.null(multivariate)==FALSE){
      counter = 1
      for (adjustement_set in multivariate){
        str_adj <- paste( adjustement_set[adjustement_set != col],
                          collapse = " + ")
        print ( paste('Computing multivariate model on', str_adj) )

        # Computing multivariate model
        frm <- as.formula(paste(dependent, "~", col, "+", str_adj))
        model_multivariate <- glm( formula = frm,
                                   data=data,
                                   family="binomial")


        # Assigning the result in output table
        OR_multi_colname <- paste("OR (model ", counter, ")", sep = "" )
        table <- extract_OR_to_table( data = data,
                                      table = table,
                                      studied_var = col,
                                      model = model_multivariate,
                                      OR_colname = OR_multi_colname,
                                      digits = digits,
                                      digits_p = digits_p,
                                      p_limit = p_limit)

        counter = counter + 1
      }
    }

    cat(paste(col, "finished\n\n"))
  }

  table[is.na(table)] <- "-"
  table[ table$levels != "Mean (SD)", "levels"] <- paste(
    table[ table$levels != "Mean (SD)", "levels"], ", N(%)", sep=""
    )

  return(table)

}
