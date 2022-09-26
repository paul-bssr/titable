#' Function to save summary table in a nicely formatted excel file
#'
#' @description Function to save summary table in a nicely formatted excel file.
#' It can use an already existing excel file and add a sheet. Or, it can create
#' a new file.
#'
#' @param summary_table A data.frame containing the data to save in excel file
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
#' @return
#' @export
#'
#' @examples
#' # Creating table to save
#' table <-summary_table(data = wdbc.data,
#'                       studied_vars = c("radius", "texture", "compactness_quartile"),
#'                       dependent = "diagnosis",
#'                       multivariate = list(c("smoothness", "texture"),
#'                                           c("concavity", "symmetry"))
#'                                           )
#' # Creating an excel file
#' save_summary_table(table, filepath="data/", filename="test",
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
#'

default_columns <- list("label"="Variables", "levels"="")

save_summary_table <- function(table,
                               filepath,
                               filename,
                               sheetname,
                               title = "",
                               subtitle = "",
                               append = FALSE,
                               list_variables_renaming = list(),
                               list_columns_renaming = default_columns,
                               underline_p = TRUE
                               ){
  # create a new workbook for outputs
  file = paste( filepath, filename, ".xlsx", sep="")
  if ( append ){
    wb <- loadWorkbook(file)
  }else{
    wb<-createWorkbook(type="xlsx")
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
                           list_variable_renaming[[varname]]
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
  add_font_for_significant(wb, sheet, table)

  # Workbook saving
  saveWorkbook(wb, file)
}


### Some helper functions

xlsx.addTitle <- function(sheet, rowIndex, title, titleStyle){
  #++++++++++++++++++++++++
  # Helper function to add titles
  #++++++++++++++++++++++++
  # - sheet : sheet object to contain the title
  # - rowIndex : numeric value indicating the row to
  #contain the title
  # - title : the text to use as title
  # - titleStyle : style object to use for title
  rows <-createRow(sheet,rowIndex=rowIndex)
  sheetTitle <-createCell(rows, colIndex=1)
  setCellValue(sheetTitle[[1,1]], title)
  setCellStyle(sheetTitle[[1,1]], titleStyle)
}


set_custom_style <- function(wb, list_args){
  custom_style <- CellStyle(wb) +
    do.call( Font, append( list(wb), list_args$list_font ) ) +
    do.call( Alignment, append( list(wb), list_args$list_alignment ) ) +
    #do.call( Border, list(list(wb), list_args$list_border) )
  return(custom_style)
}

list_title <- list(
  list_font = list(
    heightInPoints = 16,
    color = "blue",
    isBold = TRUE,
    underline = 1)#,
  #list_alignment = list(wrapText=TRUE, vertical="VERTICAL_CENTER", h="ALIGN_CENTER"),
  #list_border = list("black")
  )

list_subtitle = list(
  list_font = list(
    heightInPoints=14,
    isItalic=TRUE,
    isBold=FALSE)
)

list_rownames <- list(
  list_font  = list(
    isBold = TRUE
  ),
  list_alignment = list(
    wrapText=TRUE,
    vertical="VERTICAL_CENTER"
  )
)

# Style for the data table column names
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


extract_pvalue_from_char <- function(char_variable){
  pvalue <-as.numeric(str_extract(char_variable, "(?<=p=).*(?=\\))"))
  return (pvalue)
}

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
               Alignment(h="ALIGN_LEFT", vertical="VERTICAL_CENTER") +
               Fill(backgroundColor="lavender") )
    setCellStyle(cells[[cellname]], cs)
    }
  }
