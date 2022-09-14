#' Function to complete OR part of summary table for a given quantitative or
#' categorical variable
#'
#' @param data A data.frame containing the data for Logistic Regression
#' @param table A data.fram containing the summary table
#' @param studied_var A character containing name of the variable of interest
#' @param model GLM model to use for OR extraction
#' @param OR_colname A character containing name of the final column
#'
#' @return table data.frame completed
#'
#' @keywords internal


extract_OR_to_table <- function(data,
                                table,
                                studied_var,
                                model,
                                OR_colname
){
  if (is.factor(data[, studied_var])) {
    index_row <- which( table$label == studied_var)

    for (level in levels(data[[studied_var]])[-c(1)]){
      index_row = index_row + 1
      table[index_row, OR_colname] <- extract_OR_to_str(model=model,
                                                        studied_var=studied_var,
                                                        level=level )
    }
  } else {
    table[
      table$label==studied_var,
      OR_colname
    ] <- extract_OR_to_str(model, studied_var = studied_var)
  }
  return(table)
}
