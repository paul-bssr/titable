################################################################################
# Utilities to save result data.frame in an excel
#

#' Function to save summary table in a nicely formatted excel file
#'
#' @description Function to save summary table in a nicely formatted excel file.
#' It can use an already existing excel file and add a sheet. Or, it can create
#' a new file.
#'
#' @param table A data.frame containing the data to save in excel file
#' @param filepath A character containing a path to the folder for saving
#' @param filename A character containing the name to give to the file
#' (without the extension)
#' @param sheetname A character containing the name to give to the excel sheet
#' @param title A character containing a title to put as header in the sheet.
#' Default :""
#' @param subtitle A character containing a subtitle appearing rigth below the
#' title. Default :""
#' @param append Boolean. If TRUE, save_summary_table will open an existing
#' file. If FALSE (default), it creates a new file and erase already existing
#' file with same name.
#' @param list_variables_renaming A list whose names correspond to variables to
#' rename and values to the new names to use
#' @param list_columns_renaming A list whose names correspond to columns to
#' rename and values to the new names to use
#' @param underline_p A boolean, indicating whether to add a font or not, to
#' significant pvalues (default: True)
#'
#' @return Nothing. But an excel file is saved at the given location.
#' @import xlsx
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Creating table to save
#' table <-summary_table(data = wdbc.data,
#'                       studied_vars = c("radius", "texture", "compactness_quartile"),
#'                       dependent = "diagnosis",
#'                       multivariate = list(c("smoothness", "texture"),
#'                                           c("concavity", "symmetry"))
#'                                           )
#' # Creating an excel file
#' save_summary_table(table, filepath="inst/extdata/", filename="test",
#'                    sheetname = "test_sheet_1",
#'                    title = "Regression logistic study",
#'                    subtitle = "This is an interesting study.",
#'                    list_variables_renaming = list("compactness_quartile"="Compactness quartile")
#'                    )
#'
#' # Adding a sheet to an existing excel file
#' save_summary_table(table, filepath="data/", filename="test",
#'                    sheetname = "test_sheet_2",
#'                    title = "Regression logistic study",
#'                    subtitle = "This is an interesting study.", append=TRUE)
#' }
#'
save_summary_table <- function(table,
                               filepath,
                               filename,
                               sheetname,
                               title = "",
                               subtitle = "",
                               append = FALSE,
                               list_variables_renaming = list(),
                               list_columns_renaming = list("label"="Variables",
                                                            "levels"=""),
                               underline_p = TRUE
                               ){
  # create a new workbook for outputs
  file = paste( filepath, filename, ".xlsx", sep="")
  if ( append ){
    wb <- loadWorkbook(file)
  }else{
    wb <- createWorkbook(type="xlsx")
  }


  # Creating a sheet
  sheet <- createSheet(wb, sheetName = sheetname)

  # Add title
  xlsx.addTitle(sheet, rowIndex=1, title=title,
                titleStyle = set_custom_style(wb, list_title))

  # Add sub title
  subtitle <- paste(subtitle,
                    ' Saved on : ',
                    format(Sys.time(), '%Y/%m/%d %H:%M:%S'),
                    sep='')
  xlsx.addTitle(sheet, rowIndex=2,
                title=subtitle,
                titleStyle = set_custom_style(wb, list_subtitle))

  # Renaming label column values in data.frame
  for (varname in names( list_variables_renaming )){
    table$label <- replace(table$label,
                           table$label==varname,
                           list_variables_renaming[[varname]]
                           )
  }

  # Renaming columns in data.frame
  for (colname in names( list_columns_renaming )){
    colnames(table) <- replace(colnames(table),
                           colnames(table)==colname,
                           list_columns_renaming[[colname]]
    )
  }

  # Adding dataframe to sheet
  list_result_cells <- list(`1` = set_custom_style(wb, list_rownames),
                            `2` = set_custom_style(wb, list_descriptive))
  for (i in 3:length(table)){
    name <- as.character(i)
    list_result_cells[[name]] <-  set_custom_style(wb, list_results)
  }
  addDataFrame(table, sheet, startRow=4, startColumn=1, row.names = FALSE,
               colnamesStyle = set_custom_style(wb, list_colnames),
               rownamesStyle = set_custom_style(wb, list_rownames),
               colStyle=list_result_cells
               )


  ### Some formatting
  # Change column width
  setColumnWidth(sheet, colIndex=c(1), colWidth=20)
  setColumnWidth(sheet, colIndex=c(2:4), colWidth=11)
  setColumnWidth(sheet, colIndex=c(5:ncol(table)), colWidth=32)

  # Change row height
  rows  <- getRows(sheet )
  setRowHeight(rows, multiplier = 2)

  # Change font for significant values
  if (underline_p){
    add_font_for_significant(wb, sheet, table)
  }

  # Workbook saving
  saveWorkbook(wb, file)
}


################################################################################
#
#
#' Function to add a title to excel file
#'
#' @description This functions is an helper to build an excel output. It enables
#' to add a title, or a subtitle, to a sheet object from a workbook (using xlsx
#' library)
#'
#' @param sheet A sheet object in which title will be written
#' @param rowIndex A numeric value indicating the row to contain the title
#' @param title A character to be used as title
#' @param titleStyle A cell style object to use for the title
#'
#' @keywords internal
#'
xlsx.addTitle <- function(sheet, rowIndex, title, titleStyle){
  rows <-createRow(sheet,rowIndex=rowIndex)
  sheetTitle <-createCell(rows, colIndex=1)
  setCellValue(sheetTitle[[1,1]], title)
  setCellStyle(sheetTitle[[1,1]], titleStyle)
}


################################################################################
#
#
#' Creating a cell style
#'
#' @description A function to define an xlsx cell style, depending on a list of
#' arguments `list_args`
#'
#' @param wb A workbook object
#' @param list_args A list containing differents lists of arguments : list_font,
#' list_alignment, and list_border. Those sublists contain respectively
#' arguments for Font, Alignment, and Border functions of CellStyle.
#'
#' @return A CellStyle defined thanks to `list_args`
#'
#' @keywords internal
#'
set_custom_style <- function(wb, list_args){
  custom_style <- CellStyle(wb)

  if ("list_font" %in% names( list_args ) & (length(list_args$list_font)) ){
    custom_style <- custom_style +
      do.call( Font, append( list(wb), list_args$list_font ) )
  }

  if ("list_alignment" %in% names( list_args ) &
      ( length(list_args$list_alignment) ) ){
    custom_style <- custom_style +
      do.call( Alignment, list_args$list_alignment )
  }

  if ("list_border" %in% names( list_args ) & (length(list_args$list_border)) ){
    custom_style <- custom_style +
      do.call( Border, list_args$list_border )
  }

  return(custom_style)
}

################################################################################
#
## Lists used to define different CellStyle (title, subtitle, columns...)

# List for title CellStyle
list_title <- list(
  list_font = list(
    heightInPoints = 16,
    color = "blue",
    isBold = TRUE,
    underline = 1)
  )

# List for subtitle CellStyle
list_subtitle = list(
  list_font = list(
    heightInPoints=14,
    isItalic=TRUE,
    isBold=FALSE)
)

# List for rownames CellStyle
list_rownames <- list(
  list_font  = list(
    isBold = TRUE
  ),
  list_alignment = list(
    wrapText=TRUE,
    vertical="VERTICAL_CENTER"
  )
)

# Style for column names
list_colnames <- list(
  list_font  = list(
    isBold = TRUE
  ),
  list_alignment = list(
    wrapText=TRUE,
    h="ALIGN_CENTER",
    vertical="VERTICAL_CENTER"
  ),
  list_border = list(
    color="black",
    position=c("TOP", "BOTTOM"),
    pen=c("BORDER_THIN", "BORDER_THICK")
  )
)

# Style for results cells
list_results <- list(
  list_alignment = list(
    wrapText=TRUE,
    h="ALIGN_RIGHT",
    vertical="VERTICAL_CENTER"
  )
)

# Style for descriptive column
list_descriptive <- list(
  list_alignment = list(
    h="ALIGN_LEFT",
    vertical="VERTICAL_CENTER"
  )
)

################################################################################
#
#' Extracting pvalue from string
#'
#' @param char_variable A character from which to extract p value
#'
#' @return A numeric value corresponding to pvalue
#'
#' @keywords internal
#'
extract_pvalue_from_char <- function(char_variable){
  pvalue <-as.numeric(
    stringr::str_extract(char_variable, "(?<=p[=<]).*(?=\\))")
    )
  return (pvalue)
}


################################################################################
#
#' Add background for significant values
#'
#' @description This function enables to add lavender background for cells
#' corresponding to a significant p value
#'
#' @param wb A Workbook object from xlsx library
#' @param sheet A sheet object from Workbook
#' @param table A data.frame containing the data to be saved, i.e. summary_table
#' object
#'
#' @keywords internal
#'
add_font_for_significant <- function(wb, sheet, table){
  # Extracting pvalues
  pvalue_table <- data.frame(
    lapply(table[, 5:length(table)], extract_pvalue_from_char)
  )

  # Computing indices of columns with significant p in excel
  index_significant_values <- data.frame(
    which( pvalue_table <= 0.05, arr.ind = TRUE)
  )
  index_significant_values$row <- index_significant_values$row+4
  index_significant_values$col <- index_significant_values$col+4

  # Filling corresponding cells
  rows <- getRows(sheet, rowIndex = 5:(4+nrow(table)) )
  cells <- getCells( rows )
  list_cellnames <- paste(index_significant_values$row,
                          index_significant_values$col,
                          sep=".")
  for ( cellname in list_cellnames ){
    cs <- c( CellStyle(wb) +
               Alignment(horizontal = "ALIGN_RIGHT",
                         vertical="VERTICAL_CENTER") +
               Fill(backgroundColor="lavender") )
    setCellStyle(cells[[cellname]], cs)
    }
  }
