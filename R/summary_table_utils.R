#' Function to complete OR part of summary table for a given quantitative or
#' categorical variable
#'
#' @param data A data.frame containing the data for Logistic Regression
#' @param table A data.fram containing the summary table
#' @param studied_var A character containing name of the variable of interest
#' @param model GLM model to use for OR extraction
#' @param OR_colname A character containing name of the final column
#' @param digits An integer input giving the number of digits for rounding OR
#' and IC
#' @param digits_p An integer input giving the number of significant digits for
#' p_value (use of signif function)
#' @param p_limit A float giving the limit value below which pvalue is printed
#' as "<p_limit"
#'
#' @return table data.frame completed
#'
#' @keywords internal


extract_OR_to_table <- function(data,
                                table,
                                studied_var,
                                model,
                                OR_colname,
                                digits=3,
                                digits_p=1,
                                p_limit
){
  if (is.factor(data[, studied_var])) {
    index_row <- which( table$label == studied_var)

    for (level in levels(data[[studied_var]])[-c(1)]){
      index_row = index_row + 1
      table[index_row, OR_colname] <- extract_OR_to_str(model = model,
                                                        studied_var = studied_var,
                                                        level = level,
                                                        digits = digits,
                                                        digits_p = digits_p,
                                                        p_limit = p_limit
                                                        )
    }
  } else {
    table[
      table$label==studied_var,
      OR_colname
    ] <- extract_OR_to_str(model,
                           studied_var = studied_var,
                           digits = digits,
                           digits_p = digits_p,
                           p_limit = p_limit
                           )
  }
  return(table)
}
