#' @title Build summary table for OR univariate/mulitvariate
#'
#' @description Function building a summary table for Odds Ratios (OR) computed
#' for a given `dependent` variable with one line for each explanatory variables
#' given in `studied_vars`. If `univariate` is  `TRUE` OR computed using
#' univariate logistic regressions are added to a column "Univariable model".
#' Finally, for each set of adjustment variables in `multivariate` a column
#' giving the OR after adjustment is added.
#'
#' @param dependent Character input containing the name of the binary variable
#' to explain
#' @param studied_vars Vector of characters with names of the different
#' explanatory variables to study
#' @param univariate Boolean. If TRUE, addition of univariate model to final
#' table
#' @param multivariate List of character vectors containing different sets to be
#' used for adjustement.
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
#'               studied_vars = c("radius", "texture"),
#'               dependent = "diagnosis",
#'               univariate = FALSE
#'              )
#'
#' summary_table(data = wdbc.data,
#'               studied_vars = c("radius", "texture"),
#'               dependent = "diagnosis" )
#'
#' summary_table(data = wdbc.data,
#'               studied_vars = c("radius", "texture"),
#'               dependent = "diagnosis",
#'               multivariate = list(c("smoothness", "texture"),
#'                                   c("concavity", "symmetry"))
#'              )

summary_table <- function(data,
                          studied_vars,
                          dependent,
                          univariate=TRUE,
                          multivariate=NULL) {
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
                               data=wdbc.data,
                               family="binomial")

      # Assigning the result in output table
      table <- extract_OR_to_table( data = data,
                                    table = table,
                                    studied_var = col,
                                    model = model_univariate,
                                    OR_colname ="OR (univariate)")
    }

      } else {
        table[
          table$label==col,
          "OR (univariate)"
          ] <- coef_summary[1,1]
      }
    }

    ## Adding multivariable models
    if (is.null(multivariate)==FALSE){
      counter = 1
      for (adjustement_set in multivariate){
        str_adj <- paste( adjustement_set[adjustement_set != col],
                          collapse = " + ")
        print ( paste('Computing multivariate model on', str_adj) )
        frm <- as.formula(paste(dependent, "~", col, "+", str_adj))
        model_multivariate <- glm( formula = frm,
                                   data=wdbc.data,
                                   family="binomial")

        # Assigning the result in output table
        table[
          table$label==col,
          paste("OR (model ", counter, ")", sep = "" )
          ] <- extract_OR_to_str(model_univariate, studied_var = col)

        counter = counter + 1

      }
    }

    cat(paste(col, "finished\n\n"))
  }

  return(table)

}
