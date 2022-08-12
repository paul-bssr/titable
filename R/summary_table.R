#' Title
#'
#' @param dependent
#' @param univariate
#' @param multivariate
#'
#' @return
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
#'                                   c("concavity", "symetry"))
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
      table[table$label==col, "OR (univariate)"] <- extract_OR_to_str(model_univariate, studied_var = col)
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
