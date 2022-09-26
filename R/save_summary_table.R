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
                               list_columns_renaming = default_columns
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

  # Initializing cell styles
  cell_styles <- set_cell_styles(wb)

  # Add title
  xlsx.addTitle(sheet, rowIndex=1, title=title,
                titleStyle = cell_styles[[1]])

  # Add sub title
  subtitle <- paste(subtitle,
                    'Extracted on : ',
                    format(Sys.time(), '%Y/%m/%d %H:%M:%S'),
                    sep='')
  xlsx.addTitle(sheet, rowIndex=2,
                title=subtitle,
                titleStyle = cell_styles[[2]])

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
  list_result_cells <- list(`1`=cell_styles[[3]])
  for (i in 3:length(table)){
    name <- as.character(i)
    list_result_cells[[name]] <- cell_styles[[5]]
  }
  addDataFrame(table, sheet, startRow=4, startColumn=1, row.names = FALSE,
               colnamesStyle = cell_styles[[4]],
               rownamesStyle = cell_styles[[3]],
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


set_cell_styles <- function(wb){
  # Define some cell styles
  #++++++++++++++++++++
  # Title and sub title styles
  TITLE_STYLE <- c( CellStyle(wb)+ Font(wb,  heightInPoints=16,
                                     color="blue", isBold=TRUE, underline=1) )
  SUB_TITLE_STYLE <- c( CellStyle(wb) +
    Font(wb,  heightInPoints=14,
         isItalic=TRUE, isBold=FALSE))

  # Styles for the data table rownames
  TABLE_ROWNAMES_STYLE <- CellStyle(wb) +
    Font(wb, isBold=TRUE) +
    Alignment(wrapText=TRUE, vertical="VERTICAL_CENTER")

  # Styles for the data table column names
  TABLE_COLNAMES_STYLE <- c( CellStyle(wb) + Font(wb, isBold=TRUE) +
    Alignment(wrapText=TRUE, h="ALIGN_CENTER", v="VERTICAL_CENTER") +
    Border(color="black", position=c("TOP", "BOTTOM"),
           pen=c("BORDER_THIN", "BORDER_THICK")))

  # Styles for results cells
  RESULTS_STYLE <- CellStyle(wb) +
    Alignment(h="ALIGN_RIGHT", vertical="VERTICAL_CENTER")

  output_styles <- list(TITLE_STYLE, SUB_TITLE_STYLE,
                     TABLE_ROWNAMES_STYLE, TABLE_COLNAMES_STYLE,
                     RESULTS_STYLE)

  return( output_styles )

}
